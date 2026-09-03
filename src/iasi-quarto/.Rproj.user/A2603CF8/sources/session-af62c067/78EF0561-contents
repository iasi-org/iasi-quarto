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

.pandoc_build_formats = function() {
  c("single", "docx", "odt")
}

.order_project_build_formats = function(project, formats, warn = TRUE) {
  if (!length(formats)) {
    return(formats)
  }

  pandoc_formats = intersect(formats, .pandoc_build_formats())

  if (length(pandoc_formats) && !"html" %in% formats) {
    html_profile = file.path(project$path, "_quarto-html.yml")
    html_renderer = ".render_html"

    if (!file.exists(html_profile)) {
      if (warn) {
        for (format in pandoc_formats) {
          warning(
            sprintf(
              "Ignorando '%s' porque requiere HTML y falta '_quarto-html.yml'.",
              format
            ),
            call. = FALSE
          )
        }
      }

      formats = setdiff(formats, pandoc_formats)
    } else if (!exists(html_renderer, mode = "function")) {
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

.write_html_exports = function(project) {
  profiles = .resolve_project_build_formats(project, "all", warn = FALSE)
  profiles = setdiff(profiles, "html")
  profiles = profiles[vapply(profiles, function(profile) {
    output_path = .profile_config(project, profile)$output_path
    !is.null(output_path) && dir.exists(output_path)
  }, logical(1))]

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

  extensions = c(
    single = "html",
    pdf = "pdf",
    pdfua = "pdf",
    epub = "epub",
    docx = "docx",
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

  html_output = .profile_config(project, "html")$output_path
  if (is.null(html_output)) stop(sprintf("Quarto HTML output directory is not configured for '%s'.", project$name), call. = FALSE)

  path = file.path(html_output, "exports.json")
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

# Build dispatch ----------------------------------------------------------

.r_build_formats = function() {
  c("r", "r-source", "r-binary")
}

.resolve_build_type = function(path = ".", format = NULL) {
  path = .normalise_project_path(path)

  if (!is.null(format)) {
    formats = unique(as.character(format))

    if (!length(formats) || anyNA(formats) || any(!nzchar(formats))) {
      stop("`format` must contain at least one non-empty value.", call. = FALSE)
    }

    package_formats = formats %in% .r_build_formats()

    if (any(package_formats)) {
      if (!all(package_formats)) {
        stop(
          "R package formats cannot be combined with Quarto formats in one build.",
          call. = FALSE
        )
      }
      return("package")
    }

    if (!identical(formats, "all")) {
      return("quarto")
    }
  }

  if (file.exists(file.path(path, "DESCRIPTION"))) {
    return("package")
  }

  "quarto"
}

.build_quarto = function(book = NULL, format = NULL, path = ".", force = FALSE) {
  started_at = Sys.time()
  formats = .resolve_build_formats(format)
  plan = validate(path)

  if (is.null(plan)) {
    return(NULL)
  }

  message("Construyendo...")
  plan = .select_build_books(plan = plan, book = book)
  plan = .discover(plan)
  plan = .check(plan)
  plan = .prepare(plan)
  plan$projects = lapply(plan$projects, .render_build_project, formats = formats)

  plan$rendered = all(vapply(plan$projects, function(project) isTRUE(project$publication$rendered), logical(1)))
  plan$formats = unique(unlist(lapply(plan$projects, `[[`, "render_formats"), use.names = FALSE))
  plan$elapsed = as.numeric(difftime(Sys.time(), started_at, units = "secs"))

  .report_build(plan)
  plan$outputs = .quarto_build_results(plan)
  plan
}

.quarto_build_results = function(plan) {
  results = list()

  for (project in plan$projects) {
    for (format in project$render_formats) {
      config = .profile_config(project, format)
      output_path = config$output_path

      if (is.null(output_path)) {
        output_path = project$publication$path
      }

      if (!file.exists(output_path) && !dir.exists(output_path)) next

      results[[length(results) + 1L]] = list(project = project$name, format = format, path = normalizePath(output_path, winslash = "/", mustWork = TRUE))
    }
  }

  results
}

.build_package = function(book = NULL, format = NULL, path = ".", force = FALSE) {
  path = .normalise_project_path(path)
  description = file.path(path, "DESCRIPTION")

  if (!file.exists(description)) {
    stop(sprintf("No R package DESCRIPTION found in '%s'.", path), call. = FALSE)
  }

  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Building R packages requires the 'devtools' package.", call. = FALSE)
  }

  formats = .resolve_package_build_formats(format)
  staging = tempfile("iasi-package-build-")
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  started_at = Sys.time()
  results = list()

  if ("r-source" %in% formats) {
    message("Building R source package...")
    artifact = devtools::build(pkg = path, path = staging, binary = FALSE, quiet = FALSE)
    results[[length(results) + 1L]] = list(project = basename(path), format = "r-source", path = normalizePath(artifact, winslash = "/", mustWork = TRUE), staging = staging)
  }

  if ("r-binary" %in% formats) {
    message("Building R binary package...")
    artifact = devtools::build(pkg = path, path = staging, binary = TRUE, quiet = FALSE)
    results[[length(results) + 1L]] = list(project = basename(path), format = "r-binary", path = normalizePath(artifact, winslash = "/", mustWork = TRUE), staging = staging)
  }

  elapsed = as.numeric(difftime(Sys.time(), started_at, units = "secs"))

  list(
    path = path,
    type = "package",
    projects = list(list(name = basename(path), path = path, type = "package")),
    formats = formats,
    rendered = TRUE,
    elapsed = elapsed,
    outputs = results
  )
}

.resolve_package_build_formats = function(format = NULL) {
  if (is.null(format) || identical(format, "all") || identical(format, "r")) {
    return(c("r-source", "r-binary"))
  }

  formats = unique(as.character(format))

  if (!length(formats) || anyNA(formats) || any(!nzchar(formats))) {
    stop("`format` must contain at least one non-empty value.", call. = FALSE)
  }

  invalid = setdiff(formats, c("r-source", "r-binary"))
  if (length(invalid)) {
    stop(
      sprintf(
        "Unsupported R package format%s: %s.",
        if (length(invalid) == 1L) "" else "s",
        paste(sprintf("'%s'", invalid), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  formats
}

.record_quarto_build_results = function(projects, result) {
  if (is.null(projects) || !length(projects) || is.null(result) || !length(result)) return(invisible(TRUE))

  for (project in projects) {
    project_results = result[vapply(result, function(item) identical(item$project, project$name), logical(1))]
    if (!length(project_results)) next

    output_root = dirname(project_results[[1]]$path)
    .record_build_state(project, source = output_root)
  }

  invisible(TRUE)
}

.move_build_results = function(result, type, projects) {
  if (is.null(result) || !length(result)) {
    return(result)
  }

  moved = vector("list", length(result))
  moved_from_staging = character()
  project_names = vapply(projects, function(project) project$name, character(1))

  for (i in seq_along(result)) {
    item = result[[i]]
    project = projects[[match(item$project, project_names)]]
    outputs = project$paths$outputs

    if (is.null(outputs)) {
      moved[[i]] = item
      next
    }

    if (.path_is_inside(item$path, outputs)) {
      message(sprintf("[build] Already in outputs: %s", item$path))
      moved[[i]] = item
      next
    }

    destination = .build_result_destination(outputs = outputs, type = type, result = item)
    message(sprintf("[build] Moving %s -> %s", item$path, destination))
    moved[[i]] = .move_build_result(result = item, destination = destination)
    if (!is.null(item$staging)) moved_from_staging = c(moved_from_staging, item$staging)
  }

  for (dir in unique(moved_from_staging)) {
    if (dir.exists(dir)) unlink(dir, recursive = TRUE, force = TRUE)
  }

  moved
}

.path_is_inside = function(path, directory) {
  path = normalizePath(path, winslash = "/", mustWork = TRUE)
  directory = normalizePath(directory, winslash = "/", mustWork = TRUE)

  identical(path, directory) || startsWith(path, paste0(directory, "/"))
}

.build_result_destination = function(outputs, type, result) {
  if (identical(type, "package")) {
    return(file.path(outputs, "software", result$project))
  }

  if (identical(type, "quarto")) {
    project = sub("^[0-9]+[-_. ]*", "", result$project)
    if (!nzchar(project)) project = result$project
    return(file.path(outputs, "docs", project, result$format))
  }

  stop(sprintf("Unsupported build result type '%s'.", type), call. = FALSE)
}

.move_build_result = function(result, destination) {
  source = result$path
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)

  if (dir.exists(source)) {
    target = destination
    if (dir.exists(target)) unlink(target, recursive = TRUE, force = TRUE)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    moved = file.rename(source, target)
    if (!moved) {
      dir.create(target, recursive = TRUE, showWarnings = FALSE)
      copied = file.copy(list.files(source, full.names = TRUE, all.files = TRUE, no.. = TRUE), target, recursive = TRUE, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
      if (!all(copied)) stop(sprintf("Could not move build output '%s' to '%s'.", source, target), call. = FALSE)
      unlink(source, recursive = TRUE, force = TRUE)
    }
    result$path = normalizePath(target, winslash = "/", mustWork = TRUE)
    return(result)
  }

  target = file.path(destination, basename(source))
  if (file.exists(target)) unlink(target, force = TRUE)
  moved = file.rename(source, target)
  if (!moved) {
    copied = file.copy(source, target, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
    if (!copied) stop(sprintf("Could not move build artifact '%s' to '%s'.", source, target), call. = FALSE)
    unlink(source, force = TRUE)
  }

  result$path = normalizePath(target, winslash = "/", mustWork = TRUE)
  result
}
