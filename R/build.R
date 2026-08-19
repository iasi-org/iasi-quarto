#' Build IASI Quarto publications
#'
#' Validates an IASI Quarto workspace and runs the complete build pipeline for
#' one or more selected publications and output formats.
#'
#' `build()` is the main entry point for generating IASI Quarto publications.
#' It recognises the workspace, selects publications, discovers and checks
#' their configuration, prepares derived artifacts, and invokes Quarto for
#' every selected publication and format.
#'
#' @param book Publication or publications to build. A publication can be
#'   selected by its complete directory name, its name without the numeric
#'   prefix, or its numeric prefix. Use `"all"` or `NULL` to build every
#'   publication. This argument is ignored when `path` is itself a publication.
#' @param format Output format or formats. Supported values are `"html"`,
#'   `"pdf"`, `"typst"`, `"epub"`, `"doc"`, `"odt"`, `"git"`, and `"gfm"`. Use
#'   `"all"` or `NULL` for the default friendly set: HTML, PDF, EPUB, DOC,
#'   and Git.
#' @param path IASI Quarto publication or multiproject directory.
#'
#' @return Invisibly returns the completed `iasi_quarto_plan`. Returns `NULL`
#'   when `path` does not appear to be an IASI Quarto workspace.
#'
#' @export
build = function(book = NULL, format = NULL, path = ".") {
  started_at = Sys.time()

  quiet_missing = identical(.normalise_build_selection(format, "format"), "all")
  formats = .resolve_build_formats(format)

  plan = validate(path)

  if (is.null(plan)) {
    return(invisible(NULL))
  }

  plan = .select_build_books(
    plan = plan,
    book = book
  )

  plan = .discover(plan)
  plan = .check(plan)
  plan = .prepare(plan)

  plan$projects = lapply(
    plan$projects,
    .render_build_project,
    formats = formats,
    quiet_missing = quiet_missing
  )

  plan$rendered = all(vapply(
    plan$projects,
    function(project) {
      isTRUE(project$publication$rendered)
    },
    logical(1)
  ))

  plan$formats = unique(unlist(
    lapply(
      plan$projects,
      `[[`,
      "render_formats"
    ),
    use.names = FALSE
  ))

  plan$elapsed = as.numeric(
    difftime(
      Sys.time(),
      started_at,
      units = "secs"
    )
  )

  .report_build(plan)

  invisible(plan)
}
