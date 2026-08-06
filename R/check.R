#' Check discovered IASI Quarto publications
#'
#' Performs semantic and structural checks on every publication previously
#' discovered by [discover()].
#'
#' `check()` never writes project files. It records errors and warnings in each
#' `iasi_quarto_project` and adds an aggregate `valid` flag to the plan.
#'
#' For a `regular` publication, a folder-level `index.qmd` is optional because
#' it may not have been generated yet. When present, it is ignored as a source
#' document.
#'
#' @param plan An `iasi_quarto_plan` returned by [discover()].
#'
#' @return Invisibly returns the same `iasi_quarto_plan`, enriched with
#'   validation results.
#' @export
check = function(plan) {
  .assert_discovered_plan(plan)

  plan$projects = lapply(
    plan$projects,
    .check_project
  )

  plan$valid = all(vapply(
    plan$projects,
    function(project) {
      isTRUE(project$valid)
    },
    logical(1)
  ))

  .report_check(plan)

  invisible(plan)
}
