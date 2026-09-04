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

.iasi_config_names = function() {
  c("_iasi.yml", ".iasi.yml")
}

.iasi_config_file = function(path, required = FALSE) {
  candidates = file.path(
    path,
    .iasi_config_names()
  )

  existing = candidates[file.exists(candidates)]

  if (length(existing) > 1L) {
    stop(
      sprintf(
        "IASI configuration is ambiguous in '%s': both _iasi.yml and .iasi.yml exist. Keep only one.",
        path
      ),
      call. = FALSE
    )
  }

  if (!length(existing)) {
    if (isTRUE(required)) {
      stop(
        sprintf(
          "Missing IASI configuration in '%s': expected _iasi.yml or .iasi.yml.",
          path
        ),
        call. = FALSE
      )
    }

    return(NULL)
  }

  existing[[1L]]
}

.is_iasi_publication = function(path) {
  quarto_file = file.path(path, "_quarto.yml")

  if (!file.exists(quarto_file)) {
    return(FALSE)
  }

  iasi_file = .iasi_config_file(path)

  if (is.null(iasi_file)) {
    return(FALSE)
  }

  type = .quarto_project_type(path)
  type %in% c("website", "book")
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

.find_iasi_publications = function(path) {
  files = list.files(
    path = path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    include.dirs = FALSE
  )

  iasi_files = files[basename(files) %in% .iasi_config_names()]

  if (!length(iasi_files)) return(character())

  relative = vapply(
    iasi_files,
    .relative_path,
    character(1),
    root = path
  )

  ignored = grepl(
    "(^|/)(tests|\\.git|\\.iasi)(/|$)",
    relative
  )

  iasi_files = iasi_files[!ignored]

  if (!length(iasi_files)) return(character())

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
    iasi_file = .iasi_config_file(path)

    if (!file.exists(quarto_file)) {
      message("Status       : Not a Quarto project")
      message("Reason       : Missing _quarto.yml")

      return(invisible(NULL))
    }

    if (is.null(iasi_file)) {
      message("Status       : Not an IASI Quarto project")
      message("Reason       : Missing _iasi.yml or .iasi.yml")

      return(invisible(NULL))
    }

    type = .quarto_project_type(path)

    message(sprintf("Project type : %s", type))

    if (!type %in% c("website", "book")) {
      message("Status       : Unsupported Quarto project type")
      message("Supported    : website, book")

      return(invisible(NULL))
    }

    message("Status       : Incomplete IASI Quarto project")
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

