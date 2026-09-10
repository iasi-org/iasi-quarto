#' Prepare IASI Quarto publications
#'
#' Generates the derived files required by every checked publication.
#'
#' `prepare()` only accepts a valid plan returned by [check()]. It does not
#' discover, check or render publications.
#'
#' For book publications, preparation depends on the IASI publication strategy.
#' `regular`, `structured`, `parted`, and `direct` books generate the derived book
#' structure required by Quarto.
#'
#' Website publications currently require no derived artifacts and preparation
#' only creates the runtime publication descriptor used by the render stage.
#'
#' @param plan An `iasi_quarto_plan` returned by [check()].
#'
#' @return Invisibly returns the same `iasi_quarto_plan`, enriched with one
#'   `iasi_quarto_publication` in each project and aggregate `prepared` and
#'   `changed` flags.

.prepare = function(plan) {
  .assert_checked_plan(plan)

  plan$projects = lapply(
    plan$projects,
    .prepare_project
  )

  plan$prepared = TRUE

  plan$changed = any(vapply(
    plan$projects,
    function(project) {
      isTRUE(project$publication$changed)
    },
    logical(1)
  ))

  .report_prepare(plan)

  invisible(plan)
}
