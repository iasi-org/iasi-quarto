#' Print an IASI Quarto publication
#'
#' @param x An `iasi_quarto_publication`.
#' @param ... Additional arguments, currently unused.
#'
#' @return `x`, invisibly.
#' @export
print.iasi_quarto_publication <- function(x, ...) {
  cat("IASI Quarto publication\n")
  cat("------------------------\n")
  cat("Path      : ", x$path, "\n", sep = "")
  cat("Type      : ", x$type, "\n", sep = "")
  cat("Source    : ", x$source, "\n", sep = "")
  cat("Chapters  : ", length(x$chapters), "\n", sep = "")
  cat("Artifacts : ", length(x$artifacts), "\n", sep = "")
  cat("Changed   : ", if (isTRUE(x$changed)) "yes" else "no", "\n", sep = "")
  cat("Rendered  : ", if (isTRUE(x$rendered)) "yes" else "no", "\n", sep = "")

  if (length(x$profiles)) {
    cat("Profiles  : ", paste(x$profiles, collapse = ", "), "\n", sep = "")
  }

  invisible(x)
}

.new_publication <- function(path,
                             type,
                             source,
                             chapters = character(),
                             artifacts = character(),
                             changed = FALSE) {
  publication <- list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    type = type,
    source = source,
    chapters = chapters,
    artifacts = artifacts,
    changed = isTRUE(changed),
    rendered = FALSE,
    profiles = character()
  )

  class(publication) <- c("iasi_quarto_publication", "list")
  publication
}
