#' Render an IASI Quarto project
#'
#' Builds the project and renders the selected Quarto profile.
#'
#' @param profile Output profile. Supported values are `"all"`, `"html"` and
#'   `"pdf"`.
#' @param path Directory containing `_quarto.yml`.
#'
#' @return Invisibly returns `TRUE`.
#' @export
render <- function(profile = "all", path = ".") {
  profile <- match.arg(profile, choices = c("all", "html", "pdf"))

  project <- discover(path)
  build(project)

  switch(
    profile,
    html = .render_profile(project$path, "html"),
    pdf = .render_profile(project$path, "pdf"),
    all = {
      .render_profile(project$path, "html")
      .render_profile(project$path, "pdf")
    }
  )

  invisible(TRUE)
}
