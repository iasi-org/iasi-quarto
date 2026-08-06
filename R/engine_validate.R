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
  required_files = c(
    "_quarto.yml",
    "_quarto-html.yml",
    "_quarto-pdf.yml",
    "iasi.yml"
  )

  all(
    file.exists(
      file.path(
        path,
        required_files
      )
    )
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
    basename(files) == "iasi.yml"
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
  message(sprintf("Path   : %s", path))

  if (is.null(plan)) {
    message("Status : Not an IASI Quarto project")
    message(
      "Reason : No folder contains both _quarto.yml and iasi.yml"
    )

    return(invisible(NULL))
  }

  if (isTRUE(plan$current)) {
    message("Status : IASI Quarto publication found")

    return(invisible(plan))
  }

  books = vapply(
    plan$books,
    .relative_path,
    character(1),
    root = plan$path
  )

  message("Status : IASI Quarto multiproject found")
  message(sprintf("Books  : %d", length(books)))

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

