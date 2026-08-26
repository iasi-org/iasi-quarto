.resolve_build_formats = function(format = NULL) {
  .normalise_build_selection(format, "format")
}

.project_declared_formats = function(project) {
  groups = .yaml_field(.yaml_section(project$quarto, "profile"), "group")
  formats = unique(as.character(unlist(groups, use.names = FALSE)))
  formats[!is.na(formats) & nzchar(formats)]
}

.resolve_project_build_formats = function(project, formats, warn = TRUE) {
  declared = .project_declared_formats(project)
  selected = if (identical(formats, "all")) declared else formats

  if (!identical(formats, "all")) {
    missing = selected[!selected %in% declared]

    if (length(missing) && warn) {
      for (format in missing) {
        warning(
          sprintf("Ignorando '%s': no está declarado en 'profile.group'.", format),
          call. = FALSE
        )
      }
    }

    selected = selected[selected %in% declared]
  }

  resolved = character()

  for (format in selected) {
    profile = file.path(project$path, sprintf("_quarto-%s.yml", format))

    if (!file.exists(profile)) {
      if (warn) {
        warning(
          sprintf("Ignorando '%s' porque falta '_quarto-%s.yml'.", format, format),
          call. = FALSE
        )
      }
      next
    }

    renderer = paste0(".render_", format)

    if (!exists(renderer, mode = "function")) {
      if (warn) {
        warning(
          sprintf("Ignorando '%s' porque no existe renderer '%s'.", format, renderer),
          call. = FALSE
        )
      }
      next
    }

    resolved = c(resolved, format)
  }

  resolved = unique(resolved)
  .order_project_build_formats(project, resolved, warn = warn)
}

.order_project_build_formats = function(project, formats, warn = TRUE) {
  if (!length(formats)) {
    return(formats)
  }

  if ("single" %in% formats && !"html" %in% formats) {
    html_profile = file.path(project$path, "_quarto-html.yml")
    html_renderer = ".render_html"

    if (!file.exists(html_profile)) {
      if (warn) {
        warning(
          "Ignorando 'single' porque requiere HTML y falta '_quarto-html.yml'.",
          call. = FALSE
        )
      }

      formats = formats[formats != "single"]
    } else if (!exists(html_renderer, mode = "function")) {
      if (warn) {
        warning(
          sprintf(
            "Ignorando 'single' porque requiere HTML y no existe renderer '%s'.",
            html_renderer
          ),
          call. = FALSE
        )
      }

      formats = formats[formats != "single"]
    } else {
      formats = c("html", formats)
    }
  }

  if ("html" %in% formats) {
    formats = c("html", formats[formats != "html"])
  }

  unique(formats)
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

.render_build_project = function(project, formats) {
  publication = project$publication
  project_formats = .resolve_project_build_formats(project, formats)

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
  profiles = .resolve_project_build_formats(project, "all", warn = FALSE)
  profiles = setdiff(profiles, "html")
  profiles = profiles[dir.exists(file.path(project$path, "_outputs", profiles))]

  labels = c(
    single = "HTML",
    pdf = "PDF",
    epub = "eBook",
    doc = "DOC",
    odt = "ODT",
    git = "GitBook"
  )

  icons = c(
    single = "file-earmark-code",
    pdf = "file-earmark-pdf",
    epub = "book",
    doc = "file-earmark-word",
    odt = "file-earmark-text",
    git = "book"
  )

  extensions = c(
    single = "html",
    pdf = "pdf",
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
