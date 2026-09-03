#' Build and publish IASI Quarto publications only when required
#'
#' Deploys IASI Quarto publications incrementally, one publication at a time.
#' Each publication keeps its operational state in `.iasi/quarto/state.yml`.
#' Source and build output fingerprints are used independently for every
#' selected publication to decide whether it needs a build, a publish, both,
#' or no work at all.
#'
#' For each publication, `deploy()` first checks whether a build is required.
#' After any required build completes, it checks the resulting output again and
#' publishes only that publication when its output differs from the last
#' published output. This keeps build and publish decisions local to each
#' publication instead of batching all builds before all publishes.
#'
#' @param book Publication or publications to deploy. Uses the same selection
#'   rules as [build()] and [publish()].
#' @param format Output format or formats to keep current. Uses the same
#'   selection rules as [build()].
#' @param path IASI Quarto publication or multiproject directory.
#' @param outputs Optional output directory name or path passed to [build()].
#' @param releases Optional release directory name or path passed to [publish()].
#' @param iasi When `TRUE`, applies the IASI defaults for `outputs` and
#'   `releases` when they are not supplied. Defaults to `FALSE`.
#' @param force When `TRUE`, skips build/publish freshness decisions and runs
#'   both operations for every selected publication. Validation and execution
#'   errors are still enforced. Defaults to `FALSE`.
#'
#' @return Invisibly returns the plan produced by the last operation executed.
#'   When no build or publish is required, returns the checked deployment plan.
#'   Returns `NULL` when `path` does not appear to be an IASI Quarto workspace.
#'
#' @seealso [build()], [publish()]
#'
#' @examples
#' \dontrun{
#' deploy()
#' }
#'
#' @export
deploy = function(book = NULL, format = NULL, path = ".", outputs = NULL, releases = NULL, iasi = FALSE, force = FALSE) {
  parameters = .resolve_public_parameters(iasi = iasi, parameters = list(outputs = outputs, releases = releases), path = path)
  formats = .resolve_build_formats(format)

  plan = validate(path)

  if (is.null(plan)) {
    return(invisible(NULL))
  }

  workspace_books = plan$books

  plan = .select_build_books(
    plan = plan,
    book = book
  )

  plan = .discover(plan)
  plan = .apply_plan_parameters(plan, parameters)
  plan = .check(plan)
  .assert_checked_plan(plan)

  result = plan
  built = 0L
  published = 0L

  for (i in seq_along(plan$projects)) {
    project = plan$projects[[i]]

    project_formats = .resolve_project_build_formats(
      project,
      formats,
      warn = TRUE
    )

    if (!length(project_formats)) next

    build_required = isTRUE(force) || .build_required(project, formats = project_formats)

    if (build_required) {
      build_result = build(
        format = project_formats,
        path = project$path,
        outputs = parameters$outputs,
        releases = parameters$releases,
        iasi = FALSE,
        force = force
      )

      if (is.null(build_result)) {
        return(invisible(NULL))
      }

      project = .apply_project_parameters(build_result$projects[[1L]], parameters)
      build_result$projects[[1L]] = project
      result = build_result
      built = built + 1L
    }

    publish_required = isTRUE(force) || .publish_required(project)

    if (publish_required) {

      if (isTRUE(plan$current)) {
        publish_result = publish(
          path = project$path,
          outputs = parameters$outputs,
          releases = parameters$releases,
          iasi = FALSE,
          force = force
        )

        if (is.null(publish_result)) {
          return(invisible(NULL))
        }

        project = .apply_project_parameters(publish_result$projects[[1L]], parameters)
        publish_result$projects[[1L]] = project
        result = publish_result
      } else {
        started_at = Sys.time()

        project = .publish_deploy_project(
          project = project,
          root = plan$path,
          books = workspace_books
        )

        project = .record_publish_state(project)

        publish_result = plan
        publish_result$projects = list(project)
        publish_result$published = isTRUE(project$published)
        publish_result$publish_path = .normalise_project_path(project$paths$publish)
        publish_result$elapsed = as.numeric(
          difftime(
            Sys.time(),
            started_at,
            units = "secs"
          )
        )

        .report_publish(publish_result)
        result = publish_result
      }

      published = published + 1L
    }

    plan$projects[[i]] = project
  }

  if (built == 0L && published == 0L) {
    message("")
    message("IASI Quarto deploy")
    message("------------------")
    message("Status  : UP TO DATE")
    message(sprintf(
      "Projects: %d",
      length(plan$projects)
    ))
  } else {
    message("")
    message("IASI Quarto deploy")
    message("------------------")
    message("Status   : COMPLETED")
    message(sprintf("Projects : %d", length(plan$projects)))
    message(sprintf("Built    : %d", built))
    message(sprintf("Published: %d", published))
  }

  invisible(result)
}
