#' Build an IASI Quarto project
#'
#' Discovers and validates the current project, then generates the Quarto
#' artifacts required by its publication model.
#'
#' @param project An optional object returned by [discover()].
#' @param path Directory containing `_quarto.yml`, used only when `project` is
#'   `NULL`.
#'
#' @return Invisibly returns the discovered project.
#' @export
build <- function(project = NULL, path = ".") {
  if (is.null(project)) project <- discover(path)
  validation <- .validate_project(project)
  .assert_valid(validation)

  .build_project(project)
  invisible(project)
}
