#' Discover an IASI Quarto project
#'
#' Inspects the Quarto project in `path` and describes the publication model
#' found on disk. Discovery is descriptive: it does not decide whether the
#' project is valid.
#'
#' @param path Directory containing `_quarto.yml`.
#'
#' @return An object of class `iasi_quarto_project`.
#' @export
discover <- function(path = ".") {
  .discover_project(path)
}
