#' Validate an IASI Quarto project
#'
#' Enriches a project discovered by [discover()] with validation status,
#' errors and warnings.
#'
#' @param project An object returned by [discover()].
#' @param path Directory containing `_quarto.yml`, used when `project` is NULL.
#'
#' @return Invisibly returns the same `iasi_quarto_project`, enriched with
#'   `valid`, `errors` and `warnings`.
#' @export
validate <- function(project = NULL, path = ".") {
  if (is.null(project)) {
    project <- discover(path)
  }

  project <- .validate_project(project)
  print(project)
  invisible(project)
}
