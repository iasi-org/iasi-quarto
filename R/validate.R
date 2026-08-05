#' Validate an IASI Quarto project
#'
#' Applies the structural validation rules to a project previously discovered
#' with [discover()]. If no project is supplied, the current Quarto project is
#' discovered automatically.
#'
#' @param project An object returned by [discover()]. If `NULL`, the project in
#'   `path` is discovered.
#' @param path Directory containing `_quarto.yml`. Used only when `project` is
#'   `NULL`.
#'
#' @return Invisibly returns an object of class `iasi_quarto_validation`.
#'
#' @export
validate <- function(project = NULL, path = ".") {

  if (is.null(project)) {
    project <- discover(path)
  }

  validation <- .validate_project(project)

  print(validation)

  invisible(validation)
}


#' @export
print.iasi_quarto_validation <- function(x, ...) {

  cat("\n")
  cat("IASI Quarto Validator\n")
  cat("======================\n\n")

  cat("Project type : ", x$type, "\n", sep = "")
  cat(
    "Status       : ",
    if (x$valid) "VALID" else "INVALID",
    "\n",
    sep = ""
  )

  if (length(x$warnings) > 0L) {
    cat("\nWarnings\n")
    cat("--------\n")

    for (warning in x$warnings) {
      cat("- ", warning, "\n", sep = "")
    }
  }

  if (length(x$errors) > 0L) {
    cat("\nErrors\n")
    cat("------\n")

    for (error in x$errors) {
      cat("- ", error, "\n", sep = "")
    }
  }

  invisible(x)
}