#' Assemble IASI Quarto publications for deployment
#'
#' Creates a deployable `publish/` directory from the output artifacts already
#' rendered by Quarto.
#'
#' `publish()` does not render publications. It discovers the output formats
#' declared by the available Quarto profiles, resolves each profile's effective
#' `project.output-dir`, checks that the corresponding artifacts exist, and
#' assembles them under `publish/`.
#'
#' When HTML is available, its complete output directory is copied. When PDF is
#' available, the generated book PDF is copied to the root of `publish/` so a
#' Quarto `book.downloads: [pdf]` link can resolve normally.
#'
#' @param book Publication or publications to publish. A publication can be
#'   selected by its complete directory name, its name without the numeric
#'   prefix, or its numeric prefix. Use `"all"` or `NULL` to publish every
#'   publication. This argument is ignored when `path` is itself a publication.
#' @param path IASI Quarto publication or multiproject directory.
#'
#' @return Invisibly returns the completed `iasi_quarto_plan`. Returns `NULL`
#'   when `path` does not appear to be an IASI Quarto workspace.
#'
#' @export
publish = function(book = NULL, path = ".") {
  started_at = Sys.time()

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

  .assert_checked_plan(plan)

  numbered_project = isTRUE(plan$current) && grepl(
    "^[0-9]+-",
    basename(plan$path)
  )

  if (numbered_project) {
    plan = .publish_numbered_project(plan)
  } else if (isTRUE(plan$current)) {
    plan$projects = lapply(
      plan$projects,
      .publish_project
    )
  } else {
    plan = .publish_multiproject(plan)
  }

  plan$published = all(vapply(
    plan$projects,
    function(project) {
      isTRUE(project$published)
    },
    logical(1)
  ))

  plan$elapsed = as.numeric(
    difftime(
      Sys.time(),
      started_at,
      units = "secs"
    )
  )

  .report_publish(plan)

  invisible(plan)
}
