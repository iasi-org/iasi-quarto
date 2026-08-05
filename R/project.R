#' Print an IASI Quarto project
#'
#' @param x An `iasi_quarto_project`.
#' @param ... Additional arguments, currently unused.
#'
#' @return `x`, invisibly.
#' @export
print.iasi_quarto_project <- function(x, ...) {
  cat("IASI Quarto project\n")
  cat("-------------------\n")
  cat(sprintf("Path : %s\n", x$path))
  cat(sprintf("Type : %s\n", x$type))

  if (!is.null(x$valid)) {
    cat(sprintf(
      "Status: %s\n",
      if (isTRUE(x$valid)) "VALID" else "INVALID"
    ))
  }

  if (length(x$folders)) {
    cat("\nFolders\n")
    for (folder in x$folders) {
      cat(sprintf("- %-24s %s\n", folder$name, folder$strategy))
    }
  }

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
