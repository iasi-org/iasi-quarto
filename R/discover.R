#' Discover an IASI Quarto project
#'
#' Inspects the Quarto project in `path` and describes the publication model
#' found on disk. Discovery is descriptive: it does not decide whether the
#' project is valid.
#'
#' @param path Directory containing `_quarto.yml`. Defaults to the current
#'   working directory.
#'
#' @return An object of class `iasi_quarto_project`.
#' @export
discover <- function(path = ".") {
  .discover_project(path)
}

#' @export
print.iasi_quarto_project <- function(x, ...) {
  cat("IASI Quarto project\n")
  cat("-------------------\n")
  cat(sprintf("Path : %s\n", x$path))
  cat(sprintf("Type : %s\n", x$type))

  if (length(x$folders)) {
    cat("\nFolders\n")
    for (folder in x$folders) {
      cat(sprintf("- %-24s %s\n", folder$name, folder$strategy))
    }
  }

  invisible(x)
}
