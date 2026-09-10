#' Prepare IASI Quarto outputs for deployment
#'
#' Materialises previously built outputs into one workspace-level publication
#' tree without rendering sources again.
#'
#' For a single publication, the default destination is `<publication>/_publish`.
#' For a multiproject workspace, all selected publications share one
#' `<workspace>/_publish` root and each publication is written below a stable
#' child directory derived from its folder name with any leading numeric prefix
#' removed.
#'
#' Publishing one child publication replaces only that child directory. Sibling
#' publications already present below `_publish` are preserved.
#'
#' HTML contents are moved to the publication root while other formats keep
#' their own subdirectories. Export links and publication metadata are then
#' normalised against the resulting tree.
#'
#' @param book Publication or publications to publish. A publication can be
#'   selected by its complete directory name, its name without the numeric
#'   prefix, or its numeric prefix. Use `"all"` or `NULL` to publish every
#'   publication. This argument is ignored when `path` is itself a publication.
#' @param source Optional build-output root to publish. Relative paths are
#'   resolved against each publication root and absolute paths are accepted.
#'   When `NULL`, the common root of the configured output profiles is used.
#' @param path IASI Quarto publication or multiproject directory.
#' @param force Reserved for API symmetry with [build()] and [deploy()].
#'   `publish()` already performs the requested publication pass
#'   unconditionally.
#'
#' @return Invisibly returns the completed `iasi_quarto_plan`. Returns `NULL`
#'   when `path` does not appear to be an IASI Quarto workspace.
#'
#' @export
publish = function(book = NULL,
                   source = NULL,
                   path = ".",
                   force = FALSE) {
  started_at = Sys.time()

  plan = validate(path)

  if (is.null(plan)) {
    return(invisible(NULL))
  }

  message("Publicando...")

  plan = .select_build_books(
    plan = plan,
    book = book
  )

  plan = .discover(plan)
  plan = .check(plan)

  .assert_checked_plan(plan)

  destinations = .publish_destinations(plan)

  for (i in seq_along(plan$projects)) {
    project = .publish_project_to(
      project = plan$projects[[i]],
      source = source,
      destination = destinations[[i]],
      clean = TRUE
    )

    .record_publish_state(
      project = project,
      source = source,
      destination = destinations[[i]]
    )

    plan$projects[[i]] = project
  }

  plan$published = all(vapply(
    plan$projects,
    function(project) {
      isTRUE(project$published)
    },
    logical(1)
  ))

  plan$publish_root = normalizePath(
    .publish_root(plan),
    winslash = "/",
    mustWork = FALSE
  )

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
