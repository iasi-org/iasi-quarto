#' Render a Quarto project.
#'
#' Generates the dynamic book structure and renders the selected output
#' profile.
#'
#' @param profile Output profile to render. Supported values are
#'   `"all"` (default), `"html"` and `"pdf"`.
#'
#' @return
#' Invisibly returns `TRUE` when the rendering process completes
#' successfully.
#'
#' @examples
#' \dontrun{
#' render()
#' render("html")
#' render("pdf")
#' }
#'
#' @export
render = function(profile = "all") {

  profile = match.arg(
    profile,
    choices = c("all", "html", "pdf")
  )

  generate_book_structure()

  switch(profile
    ,html = .render_html()
    ,pdf = .render_pdf()
    ,all = {
       .render_html()
       .render_pdf()
    }
  )

  invisible(TRUE)
}