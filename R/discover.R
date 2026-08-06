#' Discover IASI Quarto publications
#'
#' Reads the Quarto and IASI configuration of every publication previously
#' recognised by [validate()].
#'
#' Missing IASI configuration keys are completed with their default values.
#' Malformed YAML files produce an error that identifies the affected file.
#' Values that are valid YAML but invalid for IASI are preserved and must be
#' checked by the semantic validation step.
#'
#' @param plan An `iasi_quarto_plan` returned by [validate()].
#'
#' @return Invisibly returns the same `iasi_quarto_plan`, enriched with a
#'   `projects` list containing one `iasi_quarto_project` per publication.
#' @export
discover = function(plan) {
  .assert_plan(plan)

  plan$projects = lapply(
    plan$books,
    .discover_project
  )

  .report_discovery(plan)

  invisible(plan)
}
