.validate_project <- function(project) {
  .assert_project(project)

  errors <- character()
  warnings <- character()

  if (identical(project$type, "incoherent")) {
    errors <- c(
      errors,
      "The publication structure is incoherent."
    )
  }

  conflicting_folders <- Filter(
    function(folder) {
      isTRUE(folder$has_index) &&
        isTRUE(folder$has_00_index) &&
        !isTRUE(folder$marker)
    },
    project$folders
  )

  if (length(conflicting_folders)) {
    folder_names <- vapply(
      conflicting_folders,
      `[[`,
      character(1),
      "name"
    )

    errors <- c(
      errors,
      sprintf(
        "Folders containing both index.qmd and 00-index.qmd: %s.",
        paste(folder_names, collapse = ", ")
      )
    )
  }

  unclassified_folders <- Filter(
    function(folder) identical(folder$strategy, "unclassified"),
    project$folders
  )

  if (length(unclassified_folders)) {
    folder_names <- vapply(
      unclassified_folders,
      `[[`,
      character(1),
      "name"
    )

    errors <- c(
      errors,
      sprintf(
        "Folders without an index declaration: %s.",
        paste(folder_names, collapse = ", ")
      )
    )
  }

  direct_folders <- Filter(
    function(folder) identical(folder$strategy, "direct"),
    project$folders
  )

  if (length(direct_folders)) {
    folder_names <- vapply(
      direct_folders,
      `[[`,
      character(1),
      "name"
    )

    warnings <- c(
      warnings,
      sprintf(
        paste0(
          "Folders processed directly because they contain ",
          "index.txt or 00-index.txt: %s."
        ),
        paste(folder_names, collapse = ", ")
      )
    )
  }

  project$valid <- length(errors) == 0L
  project$errors <- unique(errors)
  project$warnings <- unique(warnings)

  project
}
