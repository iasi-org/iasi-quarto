#' Prepare IASI Quarto outputs for deployment
#'
#' Prepares a rendered output tree as a deployable publication package. HTML
#' contents are placed directly in the publication root, while every other
#' rendered format keeps its own subdirectory. The source can be selected
#' explicitly, while the destination is always resolved relative to each
#' publication root.
#'
#' After copying, `publish()` scans every `index.html` inside the published tree.
#' When an IASI export anchor is present, it is replaced by an Export dropdown
#' containing links to the export artifacts that actually exist.
#'
#' Supported anchors are `<!-- IASI_EXPORT -->`, `<div class="iasi-export"></div>`
#' and `<div class="iasi-export-anchor"></div>`.
#'
#' @param book Publication or publications to publish. A publication can be
#'   selected by its complete directory name, its name without the numeric
#'   prefix, or its numeric prefix. Use `"all"` or `NULL` to publish every
#'   publication. This argument is ignored when `path` is itself a publication.
#' @param source Optional output directory to copy. Relative paths are resolved
#'   against each publication root and absolute paths are accepted. When `NULL`,
#'   infers the common root of the declared Quarto profile outputs.
#' @param dest Publication directory relative to each publication root. It must
#'   start with `_` so Quarto ignores the generated tree as project input.
#'   Defaults to `_publish`.
#' @param path IASI Quarto publication or multiproject directory.
#' @param force Explicitly requests a complete publication pass. `publish()`
#'   already republishes the selected output unconditionally; this argument is
#'   provided so callers can propagate a common force policy. Defaults to
#'   `FALSE`.
#'
#' @return Invisibly returns the completed `iasi_quarto_plan`. Returns `NULL`
#'   when `path` does not appear to be an IASI Quarto workspace.
#'
#' @export
publish = function(book = NULL, source = NULL, dest = "_publish", path = ".", force = FALSE) {
  started_at = Sys.time()
  dest = .validate_publish_dest(dest)
  plan = validate(path)
  if (is.null(plan)) return(invisible(NULL))

  message("Publicando...")

  plan = .select_build_books(plan, book)
  plan = .discover(plan)
  plan = .check(plan)
  .assert_checked_plan(plan)

  plan$projects = lapply(
    plan$projects,
    .publish_project,
    source = source,
    dest = dest
  )

  plan$published = all(vapply(plan$projects, function(project) isTRUE(project$published), logical(1)))

  plan$projects = lapply(
    plan$projects,
    function(project) {
      .record_publish_state(
        project,
        source = source,
        dest = dest
      )
      project
    }
  )

  plan$elapsed = as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  
  .report_publish(plan)
  invisible(plan)
}
