.validate_project <- function(project) {

  if (!inherits(project, "iasi_quarto_project")) {
    stop(
      "'project' must be an object returned by discover().",
      call. = FALSE
    )
  }

  errors <- character()
  warnings <- character()

  #
  # Global project type
  #

  if (identical(project$type, "incoherent")) {
    errors <- c(
      errors,
      "The publication structure is incoherent."
    )
  }

  #
  # Folder-level conflicts
  #

  conflicting_folders <- Filter(
    function(folder) {
      isTRUE(folder$has_index) &&
        isTRUE(folder$has_00_index) &&
        !isTRUE(folder$marker)
    },
    project$folders
  )

  if (length(conflicting_folders) > 0L) {

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

  #
  # Unclassified folders
  #

  unclassified_folders <- Filter(
    function(folder) {
      identical(folder$strategy, "unclassified")
    },
    project$folders
  )

  if (length(unclassified_folders) > 0L) {

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

  #
  # Explicit as-is folders
  #

  as_is_folders <- Filter(
    function(folder) {
      identical(folder$strategy, "as-is")
    },
    project$folders
  )

  if (length(as_is_folders) > 0L) {

    folder_names <- vapply(
      as_is_folders,
      `[[`,
      character(1),
      "name"
    )

    warnings <- c(
      warnings,
      sprintf(
        "Folders processed as-is because they contain index.txt: %s.",
        paste(folder_names, collapse = ", ")
      )
    )
  }

  #
  # Enrich the existing project object
  #

  project$valid <- length(errors) == 0L
  project$errors <- unique(errors)
  project$warnings <- unique(warnings)

  project
}