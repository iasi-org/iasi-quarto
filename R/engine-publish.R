.publish_project = function(project) {
  formats = .project_supported_formats(project)

  outputs = lapply(
    formats,
    function(format) {
      .discover_publish_output(
        project = project,
        format = format
      )
    }
  )

  names(outputs) = formats

  publish_path = file.path(
    project$path,
    "publish"
  )

  outputs = .check_publish_sources(
    project = project,
    outputs = outputs,
    publish_path = publish_path
  )

  if (dir.exists(publish_path)) {
    unlink(
      publish_path,
      recursive = TRUE,
      force = TRUE
    )
  }

  dir.create(
    publish_path,
    recursive = TRUE,
    showWarnings = FALSE
  )

  if ("html" %in% names(outputs)) {
    .copy_directory_contents(
      from = outputs$html$path,
      to = publish_path
    )
  }

  if ("pdf" %in% names(outputs)) {
    copied = file.copy(
      from = outputs$pdf$file,
      to = file.path(
        publish_path,
        basename(outputs$pdf$file)
      ),
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )

    if (!isTRUE(copied)) {
      stop(
        sprintf(
          "Could not copy publication PDF '%s' to '%s'.",
          outputs$pdf$file,
          publish_path
        ),
        call. = FALSE
      )
    }
  }

  writeLines(format(Sys.time(), tz = "UTC",format = "%Y-%m-%dT%H:%M:%OS6Z"),
             file.path(publish_path, ".publish"), useBytes = TRUE)

  project$publish_path = .normalise_project_path(publish_path)
  project$publish_outputs = outputs
  project$published = TRUE
  project
}

.discover_publish_output = function(project, format) {
  type = .project_format_type(
    project = project,
    format = format
  )

  inspection = .inspect_quarto_profile(
    path = project$path,
    profile = format,
    type = type
  )

  config = .yaml_section(
    inspection,
    "config"
  )

  quarto_project = .yaml_section(
    config,
    "project"
  )

  output_dir = .yaml_field(
    quarto_project,
    "output-dir"
  )

  if (is.null(output_dir)) {
    output_dir = .default_quarto_output_dir(type)
  }

  if (
    !is.character(output_dir) ||
      length(output_dir) != 1L ||
      is.na(output_dir) ||
      !nzchar(output_dir)
  ) {
    stop(
      sprintf(
        "Invalid project.output-dir for profile '%s' in project '%s'.",
        format,
        project$name
      ),
      call. = FALSE
    )
  }

  output_path = .resolve_quarto_output_path(
    project_path = project$path,
    output_dir = output_dir
  )

  result = list(
    format = format,
    type = type,
    output_dir = output_dir,
    path = output_path,
    config = config
  )

  if (identical(format, "pdf")) {
    result$file = .discover_publish_pdf(
      project = project,
      output_path = output_path,
      config = config
    )
  }

  result
}

.inspect_quarto_profile = function(path, profile, type = NULL) {
  previous_directory = setwd(path)

  on.exit(
    setwd(previous_directory),
    add = TRUE
  )

  active_profile = profile

  if (!is.null(type)) {
    runtime_profile = .create_quarto_type_profile(
      path = path,
      type = type
    )

    on.exit(
      unlink(runtime_profile$file),
      add = TRUE
    )

    active_profile = paste(
      runtime_profile$name,
      profile,
      sep = ","
    )
  }

  error_file = tempfile(
    pattern = "iasi-quarto-inspect-",
    fileext = ".log"
  )

  on.exit(
    unlink(error_file),
    add = TRUE
  )

  output = system2(
    command = "quarto",
    args = c(
      "inspect",
      "--profile",
      active_profile
    ),
    stdout = TRUE,
    stderr = error_file
  )

  status = attr(
    output,
    "status"
  )

  if (is.null(status)) {
    status = 0L
  }

  if (!identical(as.integer(status), 0L)) {
    details = if (file.exists(error_file)) {
      readLines(
        error_file,
        warn = FALSE,
        encoding = "UTF-8"
      )
    } else {
      character()
    }

    stop(
      paste(
        c(
          sprintf(
            "Quarto inspection failed for profile '%s' with status %s.",
            profile,
            status
          ),
          details
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  tryCatch(
    yaml::yaml.load(
      paste(
        output,
        collapse = "\n"
      )
    ),
    error = function(error) {
      stop(
        sprintf(
          "Could not read Quarto inspection for profile '%s': %s",
          profile,
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
}

.default_quarto_output_dir = function(type) {
  switch(
    type,
    book = "_book",
    website = "_site",
    stop(
      sprintf(
        "Unsupported Quarto project type '%s'.",
        .display_checked_value(type)
      ),
      call. = FALSE
    )
  )
}

.resolve_quarto_output_path = function(project_path, output_dir) {
  is_absolute = grepl(
    "^(/|[A-Za-z]:[/\\\\]|[/\\\\]{2})",
    output_dir
  )

  path = if (is_absolute) {
    output_dir
  } else {
    file.path(
      project_path,
      output_dir
    )
  }

  normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
}

.discover_publish_pdf = function(project, output_path, config) {
  book = .yaml_section(
    config,
    "book"
  )

  output_file = .yaml_field(
    book,
    "output-file"
  )

  if (
    is.character(output_file) &&
      length(output_file) == 1L &&
      !is.na(output_file) &&
      nzchar(output_file)
  ) {
    filename = if (grepl(
      "\\.pdf$",
      output_file,
      ignore.case = TRUE
    )) {
      output_file
    } else {
      paste0(
        output_file,
        ".pdf"
      )
    }

    return(file.path(
      output_path,
      filename
    ))
  }

  if (!dir.exists(output_path)) {
    return(file.path(
      output_path,
      "<pdf>"
    ))
  }

  pdfs = list.files(
    path = output_path,
    pattern = "\\.pdf$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )

  if (length(pdfs) == 1L) {
    return(pdfs[[1L]])
  }

  if (!length(pdfs)) {
    return(file.path(
      output_path,
      "<pdf>"
    ))
  }

  stop(
    sprintf(
      paste0(
        "More than one PDF was found in output directory '%s' for project '%s'. ",
        "Set book.output-file in _quarto.yml to identify the publication PDF."
      ),
      output_path,
      project$name
    ),
    call. = FALSE
  )
}

.check_publish_sources = function(project, outputs, publish_path) {
  if (!length(outputs)) {
    stop(
      sprintf(
        "Project '%s' has no publishable output profiles.",
        project$name
      ),
      call. = FALSE
    )
  }

  publish_path = normalizePath(
    publish_path,
    winslash = "/",
    mustWork = FALSE
  )

  available = list()

  for (name in names(outputs)) {
    output = outputs[[name]]

    exists = dir.exists(output$path)

    if (!exists) {
      warning(
        sprintf(
          paste0(
            "Output for profile '%s' does not exist for project '%s': %s. ",
            "That format will not be published."
          ),
          output$format,
          project$name,
          output$path
        ),
        call. = FALSE
      )

      next
    }

    output_path = normalizePath(
      output$path,
      winslash = "/",
      mustWork = TRUE
    )

    output_key = tolower(output_path)
    publish_key = tolower(publish_path)

    overlaps = identical(
      output_key,
      publish_key
    ) || startsWith(
      paste0(
        output_key,
        "/"
      ),
      paste0(
        publish_key,
        "/"
      )
    ) || startsWith(
      paste0(
        publish_key,
        "/"
      ),
      paste0(
        output_key,
        "/"
      )
    )

    if (overlaps) {
      stop(
        sprintf(
          "Profile '%s' output overlaps publish/ for project '%s'. Use a separate Quarto output directory.",
          output$format,
          project$name
        ),
        call. = FALSE
      )
    }

    if (identical(output$format, "html")) {
      index = file.path(
        output$path,
        "index.html"
      )

      if (!file.exists(index)) {
        warning(
          sprintf(
            paste0(
              "HTML output for project '%s' does not contain index.html: %s. ",
              "HTML will not be published."
            ),
            project$name,
            output$path
          ),
          call. = FALSE
        )

        next
      }
    }

    if (
      identical(output$format, "pdf") &&
        !file.exists(output$file)
    ) {
      warning(
        sprintf(
          paste0(
            "PDF output for project '%s' was not found in %s. ",
            "PDF will not be published."
          ),
          project$name,
          output$path
        ),
        call. = FALSE
      )

      next
    }

    available[[name]] = output
  }

  if (!length(available)) {
    stop(
      sprintf(
        paste0(
          "No publishable artifacts were found for project '%s'. ",
          "Build at least one format before publishing."
        ),
        project$name
      ),
      call. = FALSE
    )
  }

  available
}

.copy_directory_contents = function(from, to) {
  entries = list.files(
    path = from,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  if (!length(entries)) {
    return(invisible(TRUE))
  }

  copied = file.copy(
    from = entries,
    to = to,
    recursive = TRUE,
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!all(copied)) {
    stop(
      sprintf(
        "Could not copy all publication files from '%s' to '%s'.",
        from,
        to
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.report_publish = function(plan) {
  message("IASI Quarto publish")
  message("-------------------")
  message(sprintf(
    "Status  : %s",
    if (isTRUE(plan$published)) {
      "PUBLISHED"
    } else {
      "NOT PUBLISHED"
    }
  ))
  message(sprintf(
    "Projects: %d",
    length(plan$projects)
  ))

  for (project in plan$projects) {
    formats = names(project$publish_outputs)

    message(sprintf(
      "- %s [%s] -> %s",
      project$name,
      paste(
        formats,
        collapse = ", "
      ),
      .relative_path(
        project$publish_path,
        project$path
      )
    ))

    for (format in formats) {
      output = project$publish_outputs[[format]]

      message(sprintf(
        "  %s: %s",
        toupper(format),
        output$output_dir
      ))
    }
  }

  invisible(plan)
}
