#' Validate an IASI Quarto project
#'
#' Applies the IASI Quarto structural rules to a discovered project. When no
#' project object is supplied, the project in the current directory is
#' discovered first.
#'
#' @param project An object returned by [discover()].
#' @param path Directory containing `_quarto.yml`, used only when `project` is
#'   `NULL`.
#'
#' @return Invisibly returns a validation result of class
#'   `iasi_quarto_validation`.
#' @export
validate <- function(project = NULL, path = ".") {
  if (is.null(project)) project <- discover(path)
  result <- .validate_project(project)
  print(result)
  invisible(result)
}

#' @export
print.iasi_quarto_validation <- function(x, ...) {
  cat("IASI Quarto validation\n")
  cat("-----------------------\n")
  cat(sprintf("Project type : %s\n", x$type))
  cat(sprintf("Status       : %s\n", if (x$valid) "VALID" else "INVALID"))

  if (length(x$warnings)) {
    cat("\nWarnings\n")
    cat(paste0("- ", x$warnings, collapse = "\n"), "\n", sep = "")
  }

  if (length(x$errors)) {
    cat("\nErrors\n")
    cat(paste0("- ", x$errors, collapse = "\n"), "\n", sep = "")
  }

  invisible(x)
}
