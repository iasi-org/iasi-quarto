.resolve_build_formats = function(format = NULL) {
  .normalise_build_selection(format, "format")
}

.project_declared_formats = function(project) {
  profile = .yaml_section(project$quarto, "profile")
  groups = .yaml_field(profile, "group")
  formats = unique(as.character(unlist(groups, use.names = FALSE)))
  formats = formats[!is.na(formats) & nzchar(formats)]

  if (length(formats)) {
    return(formats)
  }

  default = .yaml_field(profile, "default")

  if (
    is.character(default) &&
      length(default) == 1L &&
      !is.na(default) &&
      nzchar(default)
  ) {
    return(default)
  }

  switch(
    project$type,
    website = "web",
    character()
  )
}

.project_build_renderer = function(project, format) {
  switch(
    project$type,
    website = switch(
      format,
      web = ".render_website",
      NULL
    ),
    book = paste0(".render_", format),
    NULL
  )
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

    if (!file.exists(profile) && warn) {
      warning(
        sprintf(
          "No existe '_quarto-%s.yml'; se utilizará la configuración de '_quarto.yml'.",
          format
        ),
        call. = FALSE
      )
    }

    renderer = .project_build_renderer(project, format)

    if (is.null(renderer)) {
      if (warn) {
        warning(
          sprintf(
            "Ignorando '%s': no está soportado para proyectos '%s'.",
            format,
            project$type
          ),
          call. = FALSE
        )
      }
      next
    }

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

.pandoc_build_formats = function() {
  c("single", "docx", "odt")
}

.order_project_build_formats = function(project, formats, warn = TRUE) {
  if (!length(formats)) {
    return(formats)
  }

  if (!identical(project$type, "book")) {
    return(unique(formats))
  }

  pandoc_formats = intersect(formats, .pandoc_build_formats())

  if (length(pandoc_formats) && !"html" %in% formats) {
    html_profile = file.path(project$path, "_quarto-html.yml")
    html_renderer = ".render_html"

    if (!file.exists(html_profile) && warn) {
      warning(
        "No existe '_quarto-html.yml'; se utilizará la configuración de '_quarto.yml' para el HTML auxiliar.",
        call. = FALSE
      )
    }

    if (!exists(html_renderer, mode = "function")) {
      if (warn) {
        for (format in pandoc_formats) {
          warning(
            sprintf(
              "Ignorando '%s' porque requiere HTML y no existe renderer '%s'.",
              format,
              html_renderer
            ),
            call. = FALSE
          )
        }
      }

      formats = setdiff(formats, pandoc_formats)
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
  switch(
    project$type,
    website = .render_website(project, formats),
    book = .render_artifacts(project, formats),
    stop(
      sprintf(
        "Unsupported Quarto project type '%s'.",
        .display_checked_value(project$type)
      ),
      call. = FALSE
    )
  )
}

.render_artifacts = function(project, formats) {
  publication = project$publication
  requested_formats = if (identical(formats, "all")) {
    .project_declared_formats(project)
  } else {
    formats
  }

  project_formats = .resolve_project_build_formats(project, formats)
  html_generated = "html" %in% project_formats && !"html" %in% requested_formats
  pandoc_formats = intersect(project_formats, .pandoc_build_formats())
  needs_pandoc = length(pandoc_formats) > 0L

  configs = setNames(
    lapply(
      project_formats,
      function(format) .profile_config(project, format)
    ),
    project_formats
  )

  pandoc_config = NULL

  if (needs_pandoc) {
    pandoc_config = .build_output_config(project, "pandoc")

    for (format in pandoc_formats) {
      configs[[format]]$pandoc = pandoc_config
    }

    on.exit(
      .remove_pandoc(pandoc_config$output_path),
      add = TRUE
    )
  }

  if (html_generated) {
    on.exit(
      .remove_generated_html(configs$html$output_path),
      add = TRUE
    )
  }

  for (format in project_formats) {
    message(sprintf("Rendering '%s' as %s...", project$name, toupper(format)))
    renderer = get(paste0(".render_", format), mode = "function")
    publication = renderer(publication, configs[[format]])

    .remove_build_output_exclusions(
      project = project,
      output_path = configs[[format]]$output_path
    )

    if (identical(format, "html") && needs_pandoc) {
      .create_pandoc(
        html_path = configs$html$output_path,
        pandoc_path = pandoc_config$output_path
      )
    }
  }

  if ("html" %in% project_formats && !html_generated) {
    .write_html_exports(project)
  }

  if (html_generated) {
    publication$profiles = setdiff(publication$profiles, "html")
    project_formats = setdiff(project_formats, "html")
  }

  project$html_generated = html_generated
  project$publication = publication
  project$render_formats = project_formats
  project
}

# Quarto may copy non-rendered project directories into HTML as static
# resources. Output-only IASI paths must never become build inputs.
#
# We deliberately prune these paths after each render instead of rewriting the
# author's Quarto render targets. That keeps the author's book/website structure
# untouched and makes the rule independent of Quarto's resource-discovery
# details.
.build_output_exclusion_paths = function(project) {
  paths = .yaml_section(project$iasi, "paths")
  release = .yaml_field(paths, "release")

  if (
    !is.character(release) ||
      length(release) != 1L ||
      is.na(release) ||
      !nzchar(release)
  ) {
    return(character())
  }

  project_root = normalizePath(
    project$path,
    winslash = "/",
    mustWork = TRUE
  )

  release_path = if (.is_absolute_path(release)) {
    release
  } else {
    file.path(
      project_root,
      release
    )
  }

  release_path = normalizePath(
    release_path,
    winslash = "/",
    mustWork = FALSE
  )

  prefix = paste0(
    tolower(project_root),
    "/"
  )

  if (!startsWith(tolower(release_path), prefix)) {
    return(character())
  }

  relative = substring(
    release_path,
    nchar(project_root) + 2L
  )

  if (!nzchar(relative)) {
    return(character())
  }

  gsub(
    "\\\\",
    "/",
    relative
  )
}


.remove_build_output_exclusions = function(project, output_path) {
  if (
    is.null(output_path) ||
      !dir.exists(output_path)
  ) {
    return(invisible(character()))
  }

  exclusions = .build_output_exclusion_paths(project)

  if (!length(exclusions)) {
    return(invisible(character()))
  }

  removed = character()

  for (relative in exclusions) {
    target = file.path(
      output_path,
      relative
    )

    if (!file.exists(target) && !dir.exists(target)) {
      next
    }

    unlink(
      target,
      recursive = TRUE,
      force = TRUE
    )

    if (file.exists(target) || dir.exists(target)) {
      stop(
        sprintf(
          "Could not remove output-only path '%s' from build output.",
          relative
        ),
        call. = FALSE
      )
    }

    removed = c(
      removed,
      target
    )
  }

  invisible(removed)
}


.remove_generated_html = function(output_path) {
  if (!is.null(output_path) && dir.exists(output_path)) {
    unlink(
      output_path,
      recursive = TRUE,
      force = TRUE
    )
  }

  invisible(TRUE)
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

.resolve_export_target = function(project, profile) {
  config = .profile_config(project, profile)
  output_path = config$output_path

  if (is.null(output_path) || !dir.exists(output_path)) {
    return(NULL)
  }

  if (identical(profile, "git")) {
    target = file.path(output_path, "README.md")
    return(if (file.exists(target)) target else NULL)
  }

  extensions = c(
    single = "html",
    pdf = "pdf",
    pdfua = "pdf",
    epub = "epub",
    docx = "docx",
    odt = "odt"
  )

  if (!profile %in% names(extensions)) {
    return(output_path)
  }

  extension = extensions[[profile]]
  output_file = .resolve_export_output_file(project, profile)

  if (!grepl(sprintf("\\.%s$", extension), output_file, ignore.case = TRUE)) {
    output_file = paste0(output_file, ".", extension)
  }

  target = file.path(output_path, output_file)

  if (file.exists(target)) {
    return(target)
  }

  candidates = list.files(
    output_path,
    pattern = sprintf("\\.%s$", extension),
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (!length(candidates)) {
    return(NULL)
  }

  candidates[[1L]]
}

.write_html_exports = function(project) {
  html_path = .profile_config(project, "html")$output_path

  if (is.null(html_path) || !dir.exists(html_path)) {
    return(invisible(NULL))
  }

  profiles = .resolve_project_build_formats(project, "all", warn = FALSE)
  profiles = setdiff(profiles, "html")

  targets = lapply(
    profiles,
    function(profile) .resolve_export_target(project, profile)
  )
  names(targets) = profiles

  available = !vapply(targets, is.null, logical(1))
  profiles = profiles[available]
  targets = targets[available]

  labels = c(
    single = "HTML",
    pdf = "PDF",
    pdfua = "PDF/UA",
    epub = "eBook",
    docx = "DOCX",
    odt = "ODT",
    git = "GitBook"
  )

  icons = c(
    single = "file-earmark-code",
    pdf = "file-earmark-pdf",
    pdfua = "file-earmark-pdf",
    epub = "book",
    docx = "file-earmark-word",
    odt = "file-earmark-text",
    git = "book"
  )

  items = vapply(
    profiles,
    function(profile) {
      label = if (profile %in% names(labels)) labels[[profile]] else profile
      icon = if (profile %in% names(icons)) icons[[profile]] else "download"
      href = .relative_href(html_path, targets[[profile]])

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

  path = file.path(html_path, "exports.json")
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
