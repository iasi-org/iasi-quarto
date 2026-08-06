.check_project = function(project) {
  errors = character()
  warnings = character()

  errors = c(
    errors,
    .check_quarto_configuration(project),
    .check_iasi_configuration(project),
    .check_project_structure(project)
  )

  folders = .discover_content_folders(project)

  folder_results = lapply(
    folders,
    .check_content_folder,
    project = project
  )

  if (length(folder_results)) {
    errors = c(
      errors,
      unlist(
        lapply(
          folder_results,
          `[[`,
          "errors"
        ),
        use.names = FALSE
      )
    )

    warnings = c(
      warnings,
      unlist(
        lapply(
          folder_results,
          `[[`,
          "warnings"
        ),
        use.names = FALSE
      )
    )
  }

  project$folders = lapply(
    folder_results,
    `[[`,
    "folder"
  )

  project$valid = length(errors) == 0L
  project$errors = unique(errors)
  project$warnings = unique(warnings)

  project
}

.check_quarto_configuration = function(project) {
  errors = character()

  quarto_project = project$quarto[["project"]]

  if (is.null(quarto_project)) {
    return(
      "Missing 'project' section in _quarto.yml."
    )
  }

  if (!is.list(quarto_project)) {
    return(
      "The 'project' section in _quarto.yml must be a mapping."
    )
  }

  if (
    !is.character(project$type) ||
      length(project$type) != 1L ||
      is.na(project$type) ||
      !nzchar(project$type)
  ) {
    errors = c(
      errors,
      "Quarto project type must be a non-empty string."
    )
  }

  errors
}

.check_iasi_configuration = function(project) {
  errors = character()

  publication = project$iasi[["publication"]]

  if (!is.null(publication) && !is.list(publication)) {
    return(
      "The 'publication' section in iasi.yml must be a mapping."
    )
  }

  if (is.list(publication)) {
    allowed_keys = c(
      "strategy",
      "content-dir",
      "numbered"
    )

    unknown_keys = setdiff(
      names(publication),
      allowed_keys
    )

    if (length(unknown_keys)) {
      errors = c(
        errors,
        sprintf(
          "Unknown publication keys in iasi.yml: %s.",
          paste(
            unknown_keys,
            collapse = ", "
          )
        )
      )
    }
  }

  if (
    !is.character(project$strategy) ||
      length(project$strategy) != 1L ||
      is.na(project$strategy) ||
      !project$strategy %in% c(
        "regular",
        "structured",
        "direct"
      )
  ) {
    errors = c(
      errors,
      sprintf(
        "Invalid publication strategy: %s.",
        .display_checked_value(project$strategy)
      )
    )
  }

  if (
    !is.character(project$content_dir) ||
      length(project$content_dir) != 1L ||
      is.na(project$content_dir) ||
      !nzchar(project$content_dir)
  ) {
    errors = c(
      errors,
      "Publication content-dir must be a non-empty string."
    )
  }

  if (
    !is.logical(project$numbered) ||
      length(project$numbered) != 1L ||
      is.na(project$numbered)
  ) {
    errors = c(
      errors,
      "Publication numbered must be TRUE or FALSE."
    )
  }

  errors
}

.check_project_structure = function(project) {
  errors = character()

  root_index = file.path(
    project$path,
    "index.qmd"
  )

  if (!file.exists(root_index)) {
    errors = c(
      errors,
      "Missing root index.qmd."
    )
  }

  if (
    is.character(project$content_dir) &&
      length(project$content_dir) == 1L &&
      !is.na(project$content_dir) &&
      nzchar(project$content_dir) &&
      !dir.exists(project$content_path)
  ) {
    errors = c(
      errors,
      sprintf(
        "Content directory does not exist: %s.",
        project$content_dir
      )
    )
  }

  errors
}

.discover_content_folders = function(project) {
  if (
    !is.character(project$content_dir) ||
      length(project$content_dir) != 1L ||
      is.na(project$content_dir) ||
      !nzchar(project$content_dir) ||
      !dir.exists(project$content_path)
  ) {
    return(character())
  }

  folders = list.dirs(
    path = project$content_path,
    recursive = FALSE,
    full.names = TRUE
  )

  if (!length(folders)) {
    return(character())
  }

  folder_names = basename(folders)

  folders = folders[
    !startsWith(
      folder_names,
      "."
    )
  ]

  if (isTRUE(project$numbered)) {
    folders = folders[
      grepl(
        "^[0-9]+-",
        basename(folders)
      )
    ]
  }

  sort(folders)
}

.check_content_folder = function(path, project) {
  folder = .describe_content_folder(
    path = path,
    project = project
  )

  errors = character()
  warnings = character()

  if (identical(project$strategy, "regular")) {
    if (!length(folder$documents)) {
      errors = c(
        errors,
        sprintf(
          "Regular folder contains no source documents: %s.",
          folder$name
        )
      )
    }
  }

  if (identical(project$strategy, "structured")) {
    if (!folder$has_index) {
      errors = c(
        errors,
        sprintf(
          "Structured folder is missing index.qmd: %s.",
          folder$name
        )
      )
    }
  }

  if (identical(project$strategy, "direct")) {
    if (!length(folder$documents)) {
      errors = c(
        errors,
        sprintf(
          "Direct folder contains no source documents: %s.",
          folder$name
        )
      )
    }
  }

  list(
    folder = folder,
    errors = errors,
    warnings = warnings
  )
}

.describe_content_folder = function(path, project) {
  all_documents = sort(list.files(
    path = path,
    pattern = "\\.qmd$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  ))

  source_documents = all_documents[
    tolower(basename(all_documents)) != "index.qmd"
  ]

  if (isTRUE(project$numbered)) {
    source_documents = source_documents[
      grepl(
        "^[0-9]+-.*\\.qmd$",
        basename(source_documents),
        ignore.case = TRUE
      )
    ]
  }

  list(
    name = basename(path),
    path = .normalise_project_path(path),
    has_index = file.exists(
      file.path(
        path,
        "index.qmd"
      )
    ),
    documents = if (length(source_documents)) {
      vapply(
        source_documents,
        .normalise_project_path,
        character(1)
      )
    } else {
      character()
    }
  )
}

.report_check = function(plan) {
  message("IASI Quarto check")
  message("-----------------")
  message(sprintf(
    "Status  : %s",
    if (isTRUE(plan$valid)) {
      "VALID"
    } else {
      "INVALID"
    }
  ))
  message(sprintf(
    "Projects: %d",
    length(plan$projects)
  ))

  for (project in plan$projects) {
    message(sprintf(
      "- %s [%s]",
      project$name,
      if (isTRUE(project$valid)) {
        "VALID"
      } else {
        "INVALID"
      }
    ))

    if (length(project$errors)) {
      for (error in project$errors) {
        message(sprintf(
          "  ERROR: %s",
          error
        ))
      }
    }

    if (length(project$warnings)) {
      for (warning in project$warnings) {
        message(sprintf(
          "  WARNING: %s",
          warning
        ))
      }
    }
  }

  invisible(plan)
}

.display_checked_value = function(value) {
  if (is.null(value)) {
    return("NULL")
  }

  if (!length(value)) {
    return("<empty>")
  }

  paste(
    as.character(value),
    collapse = ", "
  )
}
