#' Print an IASI Quarto project
#'
#' @param x An `iasi_quarto_project`.
#' @param ... Additional arguments, currently unused.
#'
#' @return `x`, invisibly.
#' @export
print.iasi_quarto_project = function(x, ...) {
  cat("IASI Quarto project\n")
  cat("-------------------\n")
  cat(sprintf("Name       : %s\n", x$name))
  cat(sprintf("Path       : %s\n", x$path))
  cat(sprintf(
    "Type       : %s\n",
    .display_discovered_value(x$type)
  ))
  cat(sprintf(
    "Strategy   : %s\n",
    .display_discovered_value(x$strategy)
  ))
  cat(sprintf(
    "Content dir: %s\n",
    .display_discovered_value(x$content_dir)
  ))
  cat(sprintf(
    "Numbered   : %s\n",
    .display_discovered_value(x$numbered)
  ))

  invisible(x)
}
