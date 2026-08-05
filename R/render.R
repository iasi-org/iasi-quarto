#' Render an IASI Quarto publication
#'
#' Accepts an existing publication or builds one from a project path, then
#' invokes Quarto for the selected profile.
#'
#' For backward compatibility, `render("html")` and `render("pdf")` are
#' interpreted as profile selections.
#'
#' @param publication An optional `iasi_quarto_publication`.
#' @param profile One of `"all"`, `"html"` or `"pdf"`.
#' @param path Directory containing `_quarto.yml` when `publication` is NULL.
#'
#' @return Invisibly returns the rendered `iasi_quarto_publication`.
#' @export
render = function(publication = NULL, profile = "all", path = ".") {
  if (is.character(publication) &&
      length(publication) == 1L &&
      publication %in% c("all", "html", "pdf") &&
      identical(profile, "all")) {
      profile <- publication
      publication <- NULL
  }

  profile = match.arg(profile, c("all", "html", "pdf"))

  if (is.null(publication)) publication = prepare(path = path)

  .assert_publication(publication)

  profiles = if (identical(profile, "all")) {
                 c("html", "pdf")
             } else {
                 profile
             }

  for (selected_profile in profiles) {
    .render_profile(publication$path, selected_profile)
  }

  publication$rendered <- TRUE
  publication$profiles <- profiles

  invisible(publication)
}
