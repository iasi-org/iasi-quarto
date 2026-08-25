.supported_formats = c("html", "pdf", "typst", "epub", "doc", "odt", "git")
.all_formats = c("html", "pdf", "epub", "doc", "git")
.resolve_build_formats = function(format = NULL) {
  selection = .normalise_build_selection(format, "format")
  if (identical(selection, "all")) return(.all_formats)

  invalid = setdiff(selection, .supported_formats)
  if (length(invalid)) stop(.unsupported_formats_message(invalid), call. = FALSE)

  selection
}

.unsupported_formats_message = function(formats) {
  sprintf(
    "Unsupported format%s: %s. Supported formats are: %s.",
    if (length(formats) == 1L) "" else "s",
    paste(sprintf('"%s"', formats), collapse = ", "),
    paste(sprintf('"%s"', .supported_formats), collapse = ", ")
  )
}

.project_supported_formats = function(project) {
  types = project$format_types
  if (is.null(types)) types = .publication_format_types(project$type, isTRUE(project$html_landing_page))

  candidates = names(types)
  if (!length(candidates)) stop(sprintf("Unsupported Quarto project type '%s'.", .display_checked_value(project$type)), call. = FALSE)

  profiles = file.path(project$path, sprintf("_quarto-%s.yml", candidates))
  candidates[file.exists(profiles)]
}

.resolve_project_build_formats = function(project, formats, quiet = FALSE) {
  supported = .project_supported_formats(project)
  resolved = formats[formats %in% supported]
  missing = formats[!formats %in% supported]

  if (length(missing) && !quiet) for (format in missing) warning(sprintf("Ignorando '%s' porque falta '_quarto-%s.yml'.", format, format), call. = FALSE)

  resolved
}

.select_build_books = function(plan, book = NULL) {
  selection = .normalise_build_selection(book, "book")

  if (isTRUE(plan$current)) {
    if (!identical(selection, "all")) warning("The selected path is an IASI Quarto publication. The `book` selection will be ignored.", call. = FALSE)
    plan$selected_books = plan$books
    return(plan)
  }

  if (identical(selection, "all")) {
    plan$selected_books = plan$books
    return(plan)
  }

  candidates = plan$books
  relative_paths = unname(vapply(candidates, .relative_path, character(1), root = plan$path))
  directory_names = basename(candidates)
  book_names = sub("^[0-9]+-", "", directory_names)
  number_prefixes = sub("-.*$", "", directory_names)
  resolved = character()
  missing = character()

  for (requested in selection) {
    matches = requested == relative_paths | requested == directory_names | requested == book_names
    if (grepl("^[0-9]+$", requested)) matches = matches | suppressWarnings(as.integer(number_prefixes)) == as.integer(requested)

    if (!any(matches)) {
      missing = c(missing, requested)
      next
    }

    resolved = c(resolved, candidates[matches])
  }

  if (length(missing)) warning(sprintf("Books not found: %s.", paste(sprintf('"%s"', missing), collapse = ", ")), call. = FALSE)
  resolved = unique(resolved)
  if (!length(resolved)) stop("No IASI Quarto publications were selected.", call. = FALSE)

  plan$books = resolved
  plan$selected_books = resolved
  plan
}

.normalise_build_selection = function(value, argument) {
  if (is.null(value)) return("all")

  value = unique(as.character(value))
  if (!length(value) || anyNA(value) || any(!nzchar(value))) stop(sprintf("`%s` must contain at least one non-empty value.", argument), call. = FALSE)
  if ("all" %in% value && length(value) > 1L) stop(sprintf('`%s = "all"` cannot be combined with other values.', argument), call. = FALSE)

  value
}

.render_build_project = function(project, formats, quiet_missing = FALSE) {
  publication = project$publication
  project_formats = .resolve_project_build_formats(project, formats, quiet = quiet_missing)

  for (format in project_formats) {
    message(sprintf("Rendering '%s' as %s...", project$name, toupper(format)))
    renderer = get(paste0(".render_", format), mode = "function")
    publication = renderer(publication, .project_format_type(project, format))
  }

  if ("html" %in% project_formats) {
    .write_html_exports(project)
  }

  project$publication = publication
  project$render_formats = project_formats
  project
}

.resolve_export_output_file = function(project, profile) {
  profile_file = file.path(
    project$path,
    sprintf("_quarto-%s.yml", profile)
  )

  profile_quarto = if (file.exists(profile_file)) {
    .read_yaml_file(profile_file)
  } else {
    list()
  }

  candidates = list(
    .yaml_field(
      .yaml_section(profile_quarto, "book"),
      "output-file"
    ),
    .yaml_field(
      .yaml_section(project$quarto, "book"),
      "output-file"
    ),
    project$name
  )

  for (candidate in candidates) {
    if (
      is.character(candidate) &&
        length(candidate) == 1L &&
        !is.na(candidate) &&
        nzchar(candidate)
    ) {
      return(candidate)
    }
  }

  project$name
}

.write_html_exports = function(project) {
  groups = project$quarto$profile$group
  profiles = unique(unlist(groups, use.names = FALSE))
  profiles = setdiff(profiles, "html")
  profiles = profiles[profiles %in% .supported_formats]

  labels = c(
    pdf = "PDF",
    typst = "PDF (Typst)",
    epub = "eBook",
    doc = "DOC",
    odt = "ODT",
    git = "GitBook"
  )

  icons = c(
    pdf = "file-earmark-pdf",
    typst = "file-earmark-pdf",
    epub = "book",
    doc = "file-earmark-word",
    odt = "file-earmark-text",
    git = "book"
  )

  extensions = c(
    pdf = "pdf",
    typst = "pdf",
    epub = "epub",
    doc = "odt",
    odt = "odt"
  )

  items = vapply(
    profiles,
    function(profile) {
      label = if (profile %in% names(labels)) labels[[profile]] else profile
      icon = if (profile %in% names(icons)) icons[[profile]] else "download"

      href = if (identical(profile, "git")) {
        "../git/README.md"
      } else if (profile %in% names(extensions)) {
        output_file = .resolve_export_output_file(project, profile)
        sprintf("../%s/%s.%s", profile, output_file, extensions[[profile]])
      } else {
        sprintf("../%s/", profile)
      }

      sprintf(
        '    {"profile": %s, "text": %s, "icon": %s, "href": %s}',
        encodeString(profile, quote = '"'),
        encodeString(label, quote = '"'),
        encodeString(icon, quote = '"'),
        encodeString(href, quote = '"')
      )
    },
    character(1)
  )

  content = if (length(items)) {
    c(
      "{",
      '  "exports": [',
      paste(items, collapse = ",\n"),
      "  ]",
      "}"
    )
  } else {
    c(
      "{",
      '  "exports": []',
      "}"
    )
  }

  path = file.path(project$path, "_outputs", "html", "exports.json")
  .write_if_changed(content, path)
  invisible(path)
}


.report_build = function(plan) {
  renders = sum(vapply(plan$projects, function(project) length(project$render_formats), integer(1)))

  message("")
  message("IASI Quarto build")
  message("-----------------")
  message(sprintf("Status  : %s", if (isTRUE(plan$rendered)) "COMPLETED" else "INCOMPLETE"))
  message(sprintf("Projects: %d", length(plan$projects)))
  message(sprintf("Renders : %d", renders))
  message(sprintf("Formats : %s", paste(plan$formats, collapse = ", ")))
  message(sprintf("Elapsed : %.2f seconds", plan$elapsed))

  invisible(plan)
}
