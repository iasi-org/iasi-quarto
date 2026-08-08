#' Print an IASI Quarto publication
#'
#' @param x An `iasi_quarto_publication`.
#' @param ... Additional arguments, currently unused.
#'
#' @return `x`, invisibly.
#' @export
print.iasi_quarto_publication = function(x, ...) {
  cat("IASI Quarto publication\n")
  cat("------------------------\n")
  cat(sprintf("Path      : %s\n", x$path))
  cat(sprintf("Type      : %s\n", x$type))
  cat(sprintf("Strategy  : %s\n", x$strategy))
  if (identical(x$type, "book")) {
    cat(sprintf("Chapters  : %d\n", length(x$chapters)))
  }

  cat(sprintf("Artifacts : %d\n", length(x$artifacts)))
  cat(sprintf(
    "Changed   : %s\n",
    if (isTRUE(x$changed)) {
      "yes"
    } else {
      "no"
    }
  ))

  invisible(x)
}

.new_publication = function(path,
                            type,
                            strategy,
                            chapters = character(),
                            artifacts = character(),
                            changed = FALSE) {
  publication = list(
    path = .normalise_project_path(path),
    type = type,
    strategy = strategy,
    chapters = chapters,
    artifacts = artifacts,
    changed = isTRUE(changed),
    rendered = FALSE,
    profiles = character()
  )

  class(publication) = c(
    "iasi_quarto_publication",
    "list"
  )

  publication
}
