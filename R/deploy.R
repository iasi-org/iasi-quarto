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
deploy = function(book = NULL, format = NULL, path = ".") {
  quiet_missing = identical(
    .normalise_build_selection(
      format,
      "format"
    ),
    "all"
  )

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
  plan = .check(plan)
  .assert_checked_plan(plan)

  result = plan
  built = 0L
  published = 0L

  for (i in seq_along(plan$projects)) {
    project = plan$projects[[i]]

    build_required = .build_required(
      project,
      formats = formats,
      quiet_missing = quiet_missing
    )

    message(sprintf(
      "DEBUG deploy | project = %s | build_required = %s",
      project$name,
      build_required
    ))

    if (build_required) {
      message(sprintf(
        "DEBUG deploy | project = %s | build = RUN",
        project$name
      ))

      build_result = build(
        format = format,
        path = project$path
      )

      if (is.null(build_result)) {
        return(invisible(NULL))
      }

      project = build_result$projects[[1L]]
      result = build_result
      built = built + 1L
    } else {
      message(sprintf(
        "DEBUG deploy | project = %s | build = SKIP",
        project$name
      ))
    }

    publish_required = .publish_required(project)

    message(sprintf(
      "DEBUG deploy | project = %s | publish_required = %s",
      project$name,
      publish_required
    ))

    if (publish_required) {
      message(sprintf(
        "DEBUG deploy | project = %s | publish = RUN",
        project$name
      ))

      if (isTRUE(plan$current)) {
        publish_result = publish(
          path = project$path
        )

        if (is.null(publish_result)) {
          return(invisible(NULL))
        }

        project = publish_result$projects[[1L]]
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
        publish_result$publish_path = .normalise_project_path(
          file.path(plan$path, "publish")
        )
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
    } else {
      message(sprintf(
        "DEBUG deploy | project = %s | publish = SKIP",
        project$name
      ))
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
