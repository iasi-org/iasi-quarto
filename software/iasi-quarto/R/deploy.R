#' Build and publish IASI Quarto publications only when required
#'
#' Keeps selected publications current using local fingerprints. Each
#' publication is evaluated independently: `deploy()` builds it only when its
#' source or requested build output is stale, then publishes it only when the
#' resulting build tree differs from the last successful publication.
#'
#' In a multiproject workspace, publication destinations are resolved once from
#' the workspace root. This preserves the single `_publish` tree even though an
#' individual build is executed from the child publication directory.
#'
#' @param book Publication or publications to deploy. Uses the same selection
#'   rules as [build()] and [publish()].
#' @param format Output format or formats to keep current. Uses the same
#'   selection rules as [build()].
#' @param path IASI Quarto publication or multiproject directory.
#' @param force When `TRUE`, both build and publish are executed for every
#'   selected publication even when fingerprints are current. Validation and
#'   execution errors are still enforced. Defaults to `FALSE`.
#'
#' @return Invisibly returns the final deployment `iasi_quarto_plan`, including
#'   the updated project objects and deployment counters. Returns `NULL` when
#'   `path` does not appear to be an IASI Quarto workspace.
#'
#' @seealso [build()], [publish()]
#'
#' @examples
#' \dontrun{
#' deploy()
#' }
#'
#' @export
deploy = function(book = NULL,
                  format = NULL,
                  path = ".",
                  force = FALSE) {
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

  destinations = .publish_destinations(plan)

  built = 0L
  published = 0L

  for (i in seq_along(plan$projects)) {
    project = plan$projects[[i]]

    project_formats = .resolve_project_build_formats(
      project,
      formats,
      warn = TRUE
    )

    if (!length(project_formats)) {
      next
    }

    requested_formats = if (identical(formats, "all")) {
      .project_declared_formats(project)
    } else {
      formats
    }

    state_formats = intersect(
      project_formats,
      requested_formats
    )

    build_required = .build_required(
      project,
      formats = state_formats
    )

    if (isTRUE(force) || build_required) {
      build_result = build(
        format = state_formats,
        path = project$path,
        force = force
      )

      if (is.null(build_result)) {
        return(invisible(NULL))
      }

      project = build_result$projects[[1L]]
      built = built + 1L
    }

    destination = destinations[[i]]

    publish_required = .publish_required(
      project,
      destination = destination
    )

    if (isTRUE(force) || publish_required) {
      project = .publish_project_to(
        project = project,
        destination = destination,
        clean = TRUE
      )

      .record_publish_state(
        project = project,
        destination = destination
      )

      published = published + 1L
    }

    plan$projects[[i]] = project
  }

  plan$deploy_built = built
  plan$deploy_published = published
  plan$publish_root = normalizePath(
    .publish_root(plan),
    winslash = "/",
    mustWork = FALSE
  )

  message("")
  message("IASI Quarto deploy")
  message("------------------")

  if (built == 0L && published == 0L) {
    message("Status   : UP TO DATE")
  } else {
    message("Status   : COMPLETED")
  }

  message(
    sprintf(
      "Projects : %d",
      length(plan$projects)
    )
  )

  message(
    sprintf(
      "Built    : %d",
      built
    )
  )

  message(
    sprintf(
      "Published: %d",
      published
    )
  )

  invisible(plan)
}
