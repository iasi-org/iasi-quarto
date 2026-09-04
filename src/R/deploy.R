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
#' @param dest Publication directory relative to each selected Quarto project.
#'   The first path component must start with `_`. Defaults to `_publish`.
#' @param path IASI Quarto publication or multiproject directory.
#' @param force When `TRUE`, freshness is still evaluated but both build and
#'   publish are executed for every selected publication. Validation and
#'   execution errors are still enforced. Defaults to `FALSE`.
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
deploy = function(book = NULL, format = NULL, dest = "_publish", path = ".", force = FALSE) {
  dest = .validate_publish_dest(dest)
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

    requested_formats = if (identical(formats, "all")) {
      .project_declared_formats(project)
    } else {
      formats
    }

    state_formats = intersect(project_formats, requested_formats)
    build_required = .build_required(project, formats = state_formats)
    build_execute = isTRUE(force) || build_required

    if (build_execute) {
      build_result = build(
        format = state_formats,
        path = project$path,
        force = force
      )

      if (is.null(build_result)) {
        return(invisible(NULL))
      }

      project = build_result$projects[[1L]]
      result = build_result
      built = built + 1L
    }

    publish_required = .publish_required(project, dest = dest)
    publish_execute = isTRUE(force) || publish_required

    if (publish_execute) {
      publish_result = publish(
        path = project$path,
        dest = dest,
        force = force
      )

      if (is.null(publish_result)) {
        return(invisible(NULL))
      }

      project = publish_result$projects[[1L]]
      result = publish_result
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
