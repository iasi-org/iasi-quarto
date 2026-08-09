.validate_path = function(path = ".") {
  root = .normalise_project_path(path)

  if (.is_iasi_publication(root)) {
    return(.new_validation_plan(path = root, current = TRUE, books = root))
  }

  books = .find_iasi_publications(root)

  if (!length(books)) return(NULL)

  .new_validation_plan(
    path = root,
    current = FALSE,
    books = books
  )
}

.is_iasi_publication = function(path) {
  quarto_file = file.path(path, "_quarto.yml")
  iasi_file = file.path(path, "_iasi.yml")

  if (!file.exists(quarto_file)) {
    return(FALSE)
  }

  if (!file.exists(iasi_file)) {
    return(FALSE)
  }

  type = .quarto_project_type(path)
  required_files = .required_iasi_files(type)

  if (!length(required_files)) {
    return(FALSE)
  }

  if (!all(
    file.exists(
      file.path(
        path,
        required_files
      )
    )
  )) {
    return(FALSE)
  }

  .has_required_iasi_profiles(
    path = path,
    type = type
  )
}

.quarto_project_type = function(path) {
  quarto = yaml::read_yaml(
    file.path(path, "_quarto.yml")
  )

  type = quarto$project$type

  if (is.null(type) || !length(type)) {
    return("default")
  }

  as.character(type)
}

.required_iasi_files = function(type) {
  switch(
    type,
    website = c(
      "_quarto.yml",
      "_iasi.yml"
    ),
    book = c(
      "_quarto.yml",
      "_iasi.yml"
    ),
    character()
  )
}

.iasi_profile_files = function(type) {
  switch(
    type,
    website = "_quarto-html.yml",
    book = c(
      "_quarto-html.yml",
      "_quarto-pdf.yml"
    ),
    character()
  )
}

.has_required_iasi_profiles = function(path, type) {
  profile_files = .iasi_profile_files(type)

  if (!length(profile_files)) {
    return(FALSE)
  }

  available = file.exists(
    file.path(
      path,
      profile_files
    )
  )

  switch(
    type,
    website = all(available),
    book = any(available),
    FALSE
  )
}

.find_iasi_publications = function(path) {
  files = list.files(
    path = path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE,
    include.dirs = FALSE
  )

  iasi_files = files[
    basename(files) == "_iasi.yml"
  ]

  if (!length(iasi_files)) {
    return(character())
  }

  books = unique(dirname(iasi_files))

  books = books[
    vapply(
      books,
      .is_iasi_publication,
      logical(1)
    )
  ]

  if (!length(books)) {
    return(character())
  }

  sort(
    normalizePath(
      books,
      winslash = "/",
      mustWork = TRUE
    )
  )
}

.new_validation_plan = function(path, current, books) {
  plan = list(
    path = path,
    current = current,
    books = books
  )

  class(plan) = c(
    "iasi_quarto_plan",
    "list"
  )

  plan
}

.report_validation = function(path, plan) {
  path = .normalise_project_path(path)

  message("IASI Quarto validation")
  message("----------------------")
  message(sprintf("Path         : %s", path))

  if (is.null(plan)) {
    quarto_file = file.path(path, "_quarto.yml")
    iasi_file = file.path(path, "_iasi.yml")

    if (!file.exists(quarto_file)) {
      message("Status       : Not a Quarto project")
      message("Reason       : Missing _quarto.yml")

      return(invisible(NULL))
    }

    if (!file.exists(iasi_file)) {
      message("Status       : Not an IASI Quarto project")
      message("Reason       : Missing _iasi.yml")

      return(invisible(NULL))
    }

    type = .quarto_project_type(path)
    required_files = .required_iasi_files(type)

    message(sprintf("Project type : %s", type))

    if (!length(required_files)) {
      message("Status       : Unsupported Quarto project type")
      message("Supported    : website, book")

      return(invisible(NULL))
    }

    missing_files = required_files[
      !file.exists(file.path(path, required_files))
    ]

    profile_files = .iasi_profile_files(type)
    available_profiles = profile_files[
      file.exists(file.path(path, profile_files))
    ]

    message("Status       : Incomplete IASI Quarto project")

    if (length(missing_files)) {
      message("Missing      :")

      for (file in missing_files) {
        message(sprintf("  - %s", file))
      }
    }

    if (!length(available_profiles)) {
      message("Output       : Missing Quarto output profile")
      message(sprintf(
        "Expected     : %s",
        paste(
          profile_files,
          collapse = " or "
        )
      ))
    }

    return(invisible(NULL))
  }

  if (isTRUE(plan$current)) {
    type = .quarto_project_type(plan$path)

    message(sprintf("Project type : %s", type))
    message("Status       : IASI Quarto publication found")

    return(invisible(plan))
  }

  books = vapply(
    plan$books,
    .relative_path,
    character(1),
    root = plan$path
  )

  message("Status       : IASI Quarto multiproject found")
  message(sprintf("Publications : %d", length(books)))

  for (book in books) {
    message(sprintf("- %s", book))
  }

  invisible(plan)
}

.normalise_project_path = function(path) {
  normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )
}
