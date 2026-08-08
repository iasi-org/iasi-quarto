.prepare_web_project = function(project) {
  if (!identical(project$strategy, "regular")) {
    stop(
      sprintf(
        "Unsupported website publication strategy '%s'.",
        .display_checked_value(project$strategy)
      ),
      call. = FALSE
    )
  }

  project$publication = .new_publication(
    path = project$path,
    type = project$type,
    strategy = project$strategy,
    chapters = character(),
    artifacts = character(),
    changed = FALSE
  )

  project
}
