#' Build an IASI Quarto publication
#'
#' Discovers and validates a project when necessary, generates the artifacts
#' required by the publication, and returns an independent publication model.
#' It does not invoke Quarto.
#'
#' @param project An optional `iasi_quarto_project`.
#' @param path Directory containing `_quarto.yml`, used when `project` is `NULL`.
#'
#' @return Invisibly returns an `iasi_quarto_publication`.
#' @export
build <- function(project = NULL, path = ".") {

  if (is.null(project)) {
    project <- discover(path)
  }

  if (!inherits(project, "iasi_quarto_project")) {
    stop(
      "'project' must be an object returned by discover().",
      call. = FALSE
    )
  }

  if (is.null(project$valid)) {
    project <- .validate_project(project)
  }

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

  project <- switch(
    project$type,
    structured = .build_structured(project),
    regular = .build_regular(project),
    paper = .build_paper(project),
    mixed = stop(
      "'mixed' projects are not yet supported by build().",
      call. = FALSE
    ),
    stop(
      sprintf(
        "Unsupported publication type '%s'.",
        project$type
      ),
      call. = FALSE
    )
  )

  publication <- project$publication

  if (!inherits(publication, "iasi_quarto_publication")) {
    stop(
      "The build engine did not create a valid publication.",
      call. = FALSE
    )
  }

  invisible(publication)
}
