#' Prepare IASI Quarto outputs for deployment
#'
#' Prepares a rendered output tree as a deployable publication package. HTML
#' contents are placed directly in the publication root, while every other
#' rendered format keeps its own subdirectory. The source and destination roots
#' are resolved from the effective IASI project configuration. Another source
#' directory can still be selected explicitly.
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
#'   uses the project's configured IASI documentation output path.
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
publish = function(book = NULL, source = NULL, path = ".", force = FALSE) {
  started_at = Sys.time()
  plan = validate(path)
  if (is.null(plan)) return(invisible(NULL))

  message("Publicando...")

  plan = .select_build_books(plan, book)
  plan = .discover(plan)
  plan = .check(plan)
  .assert_checked_plan(plan)

  numbered_project = isTRUE(plan$current) && grepl("^[0-9]+-", basename(plan$path))
  if (numbered_project) plan = .publish_numbered_project(plan, source)
  else if (isTRUE(plan$current)) plan$projects = lapply(plan$projects, .publish_project, source = source)
  else plan = .publish_multiproject(plan, source)

  plan$published = all(vapply(plan$projects, function(project) isTRUE(project$published), logical(1)))

  plan$projects = lapply(
    plan$projects,
    function(project) {
      .record_publish_state(
        project,
        source = source
      )
      project
    }
  )

  plan$elapsed = as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  
  .report_publish(plan)
  invisible(plan)
}
