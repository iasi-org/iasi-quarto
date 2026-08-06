#' Prepare IASI Quarto publications
#'
#' Generates the derived files required by every checked publication.
#'
#' `prepare()` only accepts a valid plan returned by [check()]. It does not
#' discover, check or render publications.
#'
#' For `regular` publications, it generates one folder-level `index.qmd` per
#' content folder and an `_book-structure.yml` file.
#'
#' For `structured` publications, it preserves the author-maintained
#' folder-level `index.qmd` files and generates `_book-structure.yml`.
#'
#' For `direct` publications, it generates a flat `_book-structure.yml`.
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
