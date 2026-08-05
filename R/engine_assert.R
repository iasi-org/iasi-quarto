.assert_project <- function(project) {
  if (!inherits(project, "iasi_quarto_project")) {
    stop(
      "'project' must be an object returned by discover().",
      call. = FALSE
    )
  }

  invisible(project)
}

.assert_valid_project <- function(project) {
  .assert_project(project)

  if (!isTRUE(project$valid)) {
    stop(
      paste(
        c(
          "Project validation failed:",
          paste0("- ", project$errors)
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  invisible(project)
}

.assert_publication <- function(publication) {
  if (!inherits(publication, "iasi_quarto_publication")) {
    stop(
      "'publication' must be an object returned by build().",
      call. = FALSE
    )
  }

  invisible(publication)
}
