.validate_project <- function(project) {
  if (!inherits(project, "iasi_quarto_project")) {
    stop("'project' must be an object returned by discover().", call. = FALSE)
  }

  errors <- character()
  warnings <- character()

  if (identical(project$type, "incoherent")) {
    errors <- c(
      errors,
      "The project mixes incompatible publication structures."
    )
  }

  local_conflicts <- vapply(
    project$folders,
    function(folder) folder$has_index && folder$has_00_index && !folder$marker,
    logical(1)
  )

  if (any(local_conflicts)) {
    names <- vapply(project$folders[local_conflicts], `[[`, character(1), "name")
    errors <- c(
      errors,
      sprintf(
        "Folders containing both index.qmd and 00-index.qmd: %s.",
        paste(names, collapse = ", ")
      )
    )
  }

  if (project$counts$`as-is` > 0L) {
    warnings <- c(
      warnings,
      sprintf(
        "%d folder(s) use index.txt and will be processed as-is.",
        project$counts$`as-is`
      )
    )
  }

  if (identical(project$type, "mixed")) {
    warnings <- c(
      warnings,
      "No dominant publication model could be inferred because every numbered folder is marked as-is."
    )
  }

  if (identical(project$type, "paper") && !length(project$root_documents)) {
    errors <- c(errors, "A paper project requires at least one root-level .qmd document.")
  }

  if (identical(project$type, "paper") &&
      !file.exists(file.path(project$path, "index.qmd")) &&
      !file.exists(file.path(project$path, "00-index.qmd"))) {
    errors <- c(
      errors,
      "A paper project requires index.qmd or 00-index.qmd in the project directory."
    )
  }

  result <- list(
    valid = length(errors) == 0L,
    type = project$type,
    project = project,
    warnings = warnings,
    errors = errors
  )
  class(result) <- "iasi_quarto_validation"
  result
}

.assert_valid <- function(validation) {
  if (!inherits(validation, "iasi_quarto_validation")) {
    stop("'validation' must be an object returned by validate().", call. = FALSE)
  }

  if (!validation$valid) {
    stop(
      paste(c("Invalid IASI Quarto project:", paste0("- ", validation$errors)), collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
