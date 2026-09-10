#' Check discovered IASI Quarto publications
#'
#' Checks the configuration and structure of every publication discovered in
#' an IASI Quarto plan.
#'
#' @param plan An `iasi_quarto_plan` returned by `.discover()`.
#'
#' @return Invisibly returns the checked plan.
#'
#' @noRd
.check = function(plan) {
  .assert_discovered_plan(plan)

  plan$projects = lapply(
    plan$projects,
    .check_project
  )

  plan$valid = all(vapply(
    plan$projects,
    function(project) {
      isTRUE(project$valid)
    },
    logical(1)
  ))

  .report_check(plan)

  invisible(plan)
}

.check_project = function(project) {
  errors = character()
  warnings = character()

  errors = c(
    errors,
    .check_quarto_configuration(project),
    .check_iasi_configuration(project),
    .check_project_structure(project)
  )

  root_documents = .discover_root_documents(project)
  front_matter = .discover_book_section(
    path = project$front_matter_path,
    project = project
  )
  folders = .discover_content_folders(project)
  back_matter = .discover_book_section(
    path = project$back_matter_path,
    project = project
  )

  errors = c(
    errors,
    .check_top_level_numbering(
      project = project,
      root_documents = root_documents,
      folders = folders
    )
  )

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

  project$root_documents = root_documents
  project$front_matter = front_matter
  project$folders = lapply(
    folder_results,
    `[[`,
    "folder"
  )

  project$back_matter = back_matter

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
  config_name = basename(project$iasi_file)

  source = project$iasi_source
  publication = if (is.list(source)) source[["publication"]] else NULL
  paths = if (is.list(source)) source[["paths"]] else NULL

  errors = c(
    errors,
    .check_iasi_paths(
      paths = paths,
      config_name = config_name
    )
  )

  if (!is.null(publication) && !is.list(publication)) {
    errors = c(
      errors,
      sprintf("The 'publication' section in %s must be a mapping.", config_name)
    )
  }

  exclude = if (is.list(source)) source[["exclude"]] else NULL

  if (!.valid_exclude(exclude)) {
    errors = c(
      errors,
      sprintf(
        "The 'exclude' section in %s must be a list of non-empty directory names.",
        config_name
      )
    )
  }

  html = if (is.list(publication)) {
    publication[["html"]]
  } else {
    NULL
  }

  if (!is.null(html) && !is.list(html)) {
    errors = c(
      errors,
      sprintf("The 'publication.html' section in %s must be a mapping.", config_name)
    )
  }

  if (
    !is.logical(project$html_landing_page) ||
      length(project$html_landing_page) != 1L ||
      is.na(project$html_landing_page)
  ) {
    errors = c(
      errors,
      "Publication html.landing-page must be TRUE or FALSE."
    )
  }

  if (
    isTRUE(project$html_landing_page) &&
      !identical(project$type, "book")
  ) {
    errors = c(
      errors,
      "Publication html.landing-page is only supported for book projects."
    )
  }

  supported_strategies = .supported_publication_strategies(project)

  if (
    !is.character(project$strategy) ||
      length(project$strategy) != 1L ||
      is.na(project$strategy) ||
      !project$strategy %in% supported_strategies
  ) {
    errors = c(
      errors,
      sprintf(
        "Invalid publication strategy for project type '%s': %s. Supported strategies are: %s.",
        .display_checked_value(project$type),
        .display_checked_value(project$strategy),
        paste(
          supported_strategies,
          collapse = ", "
        )
      )
    )
  }

  if (.uses_content_folders(project)) {
    if (
      !is.character(project$front_matter_dir) ||
        length(project$front_matter_dir) != 1L ||
        is.na(project$front_matter_dir) ||
        !nzchar(project$front_matter_dir)
    ) {
      errors = c(
        errors,
        "Publication front-matter must be a non-empty string."
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
      !is.character(project$back_matter_dir) ||
        length(project$back_matter_dir) != 1L ||
        is.na(project$back_matter_dir) ||
        !nzchar(project$back_matter_dir)
    ) {
      errors = c(
        errors,
        "Publication back-matter must be a non-empty string."
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
  }

  errors
}


# `paths` is intentionally extensible: other IASI components may define
# additional path roles. iasi.quarto only validates conventions that affect
# publication engineering.
#
# A project has one current release materialisation, not a local release
# history. The canonical key is consequently `paths.release`.
.check_iasi_paths = function(paths, config_name) {
  if (is.null(paths)) {
    return(character())
  }

  if (!is.list(paths)) {
    return(
      sprintf(
        "The 'paths' section in %s must be a mapping.",
        config_name
      )
    )
  }

  errors = character()
  path_names = names(paths)

  if (!is.null(path_names) && "releases" %in% path_names) {
    errors = c(
      errors,
      sprintf(
        "Path 'paths.releases' in %s is obsolete; use 'paths.release'.",
        config_name
      )
    )
  }

  release = paths[["release"]]

  if (
    !is.null(release) &&
      (
        !is.character(release) ||
          length(release) != 1L ||
          is.na(release) ||
          !nzchar(release)
      )
  ) {
    errors = c(
      errors,
      sprintf(
        "Path 'paths.release' in %s must be a non-empty string.",
        config_name
      )
    )
  }

  errors
}


.supported_publication_strategies = function(project) {
  switch(
    project$type,
    website = "regular",
    book = c(
      "regular",
      "structured",
      "parted",
      "direct"
    ),
    character()
  )
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
    .uses_content_folders(project) &&
      is.character(project$content_dir) &&
      length(project$content_dir) == 1L &&
      !is.na(project$content_dir) &&
      nzchar(project$content_dir) &&
      !.is_excluded_directory(project$content_path, project) &&
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

.uses_content_folders = function(project) {
  identical(project$type, "book")
}

.valid_exclude = function(value) {
  if (is.null(value)) {
    return(TRUE)
  }

  if (is.character(value)) {
    return(all(!is.na(value) & nzchar(value)))
  }

  if (!is.list(value)) {
    return(FALSE)
  }

  if (!length(value)) {
    return(TRUE)
  }

  all(vapply(
    value,
    function(item) {
      is.character(item) &&
        length(item) == 1L &&
        !is.na(item) &&
        nzchar(item)
    },
    logical(1)
  ))
}

.is_excluded_directory = function(path, project) {
  if (!length(project$exclude)) {
    return(FALSE)
  }

  basename(path) %in% project$exclude
}

.filter_excluded_directories = function(paths, project) {
  if (!length(paths) || !length(project$exclude)) {
    return(paths)
  }

  paths[!vapply(
    paths,
    .is_excluded_directory,
    logical(1),
    project = project
  )]
}

.discover_root_documents = function(project) {
  if (!identical(project$type, "book")) {
    return(character())
  }

  documents = sort(list.files(
    path = project$path,
    pattern = "\\.qmd$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  ))

  documents = documents[
    tolower(basename(documents)) != "index.qmd"
  ]

  if (isTRUE(project$numbered)) {
    documents = documents[
      grepl(
        "^[0-9]+-.*\\.qmd$",
        basename(documents),
        ignore.case = TRUE
      )
    ]

    documents = .order_numbered_paths(documents)
  }

  if (!length(documents)) {
    return(character())
  }

  unname(vapply(
    documents,
    .normalise_project_path,
    character(1)
  ))
}

.numbered_prefix = function(path) {
  name = basename(path)
  match = regexpr(
    "^[0-9]+",
    name
  )

  if (match[[1L]] == -1L) {
    return(NA_integer_)
  }

  as.integer(
    regmatches(
      name,
      match
    )
  )
}

.order_numbered_paths = function(paths) {
  if (!length(paths)) {
    return(paths)
  }

  names = basename(paths)
  prefixes = vapply(
    paths,
    .numbered_prefix,
    integer(1)
  )

  paths[
    order(
      prefixes,
      tolower(names),
      names
    )
  ]
}

.check_top_level_numbering = function(project,
                                       root_documents,
                                       folders) {
  if (
    !identical(project$type, "book") ||
      !isTRUE(project$numbered)
  ) {
    return(character())
  }

  c(
    .check_numbered_block_duplicates(root_documents),
    .check_numbered_block_duplicates(folders)
  )
}

.check_numbered_block_duplicates = function(items) {
  if (!length(items)) {
    return(character())
  }

  prefixes = vapply(
    items,
    .numbered_prefix,
    integer(1)
  )

  duplicated_prefixes = unique(
    prefixes[duplicated(prefixes)]
  )

  if (!length(duplicated_prefixes)) {
    return(character())
  }

  vapply(
    duplicated_prefixes,
    function(prefix) {
      names = basename(
        items[prefixes == prefix]
      )

      sprintf(
        "Duplicate top-level numeric prefix %d: %s.",
        prefix,
        paste(
          names,
          collapse = ", "
        )
      )
    },
    character(1)
  )
}

.discover_content_folders = function(project) {
  if (!.uses_content_folders(project)) {
    return(character())
  }

  if (
    !is.character(project$content_dir) ||
      length(project$content_dir) != 1L ||
      is.na(project$content_dir) ||
      !nzchar(project$content_dir) ||
      !dir.exists(project$content_path) ||
      .is_excluded_directory(project$content_path, project)
  ) {
    return(character())
  }

  folders = list.dirs(
    path = project$content_path,
    recursive = FALSE,
    full.names = TRUE
  )

  folders = .filter_excluded_directories(
    folders,
    project
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

    return(.order_numbered_paths(folders))
  }

  sort(folders)
}

.check_content_folder = function(path, project) {
  if (identical(project$strategy, "parted")) {
    return(
      .check_parted_folder(
        path = path,
        project = project
      )
    )
  }

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
