#' Build and publish IASI Quarto publications only when required
#'
#' Deploys IASI Quarto publications incrementally. Each publication keeps its
#' operational state in `.iasi/quarto/state.yml`. Source and build output
#' fingerprints are used to decide whether deployment needs a new build, only
#' a publish, or no work at all.
#'
#' A build is required when the current sources no longer match the state
#' recorded for one or more selected formats, or when `_outputs/` has drifted
#' from the output produced by the last build. After a successful build,
#' `publish()` is only run when the resulting output fingerprint differs from
#' the one already published.
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

  plan = .select_build_books(
    plan = plan,
    book = book
  )

  plan = .discover(plan)
  plan = .check(plan)
  .assert_checked_plan(plan)

  build_required = vapply(
    plan$projects,
    .build_required,
    logical(1),
    formats = formats,
    quiet_missing = quiet_missing
  )

  message(sprintf(
    "DEBUG deploy | build_required = %s",
    paste(build_required, collapse = ", ")
  ))

  result = plan

  if (any(build_required)) {
    if (isTRUE(plan$current)) {
      result = build(
        format = format,
        path = plan$path
      )
    } else {
      selected = plan$projects[build_required]
      selected_books = vapply(
        selected,
        function(project) {
          .relative_path(
            project$path,
            plan$path
          )
        },
        character(1)
      )

      result = build(
        book = selected_books,
        format = format,
        path = plan$path
      )
    }
  }

  publish_required = any(vapply(
    plan$projects,
    .publish_required,
    logical(1)
  ))

  message(sprintf(
    "DEBUG deploy | publish_required = %s",
    publish_required
  ))

  if (publish_required) {
    result = publish(
      book = book,
      path = plan$path
    )
  }

  if (!any(build_required) && !publish_required) {
    message("")
    message("IASI Quarto deploy")
    message("------------------")
    message("Status  : UP TO DATE")
    message(sprintf(
      "Projects: %d",
      length(plan$projects)
    ))
  } else if (any(build_required) && !publish_required) {
    message("")
    message("IASI Quarto deploy")
    message("------------------")
    message("Publish : skipped; build output unchanged")
  }

  invisible(result)
}
