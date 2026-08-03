#' Generate the Quarto book structure.
#'
#' Discovers the project structure and generates the
#' `_book-structure.yml` file required by Quarto.
#'
#' @param layout Book organization strategy. Supported values are
#'   `"parts"` (default) and `"chapters"`.
#'
#' @return
#' Invisibly returns `TRUE` when the book structure has been generated
#' successfully.
#'
#' @examples
#' \dontrun{
#' generate_book_structure()
#' generate_book_structure("chapters")
#' }
#'
#' @export
generate_book_structure = function(layout = "parts") {

  layout = match.arg(
    layout,
    choices = c("parts", "chapters")
  )

  project = .discover_project()

  structure = .create_structure(
    project = project,
    layout = layout
  )

  .write_structure(structure)

  invisible(TRUE)

}
