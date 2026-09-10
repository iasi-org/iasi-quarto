#' Prepare IASI Quarto publications
#'
#' Validates an IASI Quarto workspace and materializes the derived artifacts
#' required by one or more selected publications without rendering them.
#'
#' `prepare()` runs the same recognition, discovery, checking, and preparation
#' stages used by [build()], but stops before invoking Quarto. It is intended
#' for workflows where generated artifacts such as `_book-structure.yml` or
#' `_generated/navbar-left.yml` need to be reviewed before rendering.
#'
#' @param book Publication or publications to prepare. A publication can be
#'   selected by its complete directory name, its name without the numeric
#'   prefix, or its numeric prefix. Use `"all"` or `NULL` to prepare every
#'   publication. This argument is ignored when `path` is itself a publication.
#' @param path IASI Quarto publication or multiproject directory.
#'
#' @return Invisibly returns the prepared `iasi_quarto_plan`. Returns `NULL`
#'   when `path` does not appear to be an IASI Quarto workspace.
#'
#' @export
prepare = function(book = NULL, path = ".") {
  plan = validate(path)

  if (is.null(plan)) {
    return(invisible(NULL))
  }

  message("Preparando...")

  plan = .select_build_books(
    plan = plan,
    book = book
  )

  plan = .discover(plan)
  plan = .check(plan)
  plan = .prepare(plan)

  invisible(plan)
}
