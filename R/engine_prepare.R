.prepare_project = function(project) {
  if (!identical(project$type, "book")) {
    stop(
      sprintf(
        "Unsupported Quarto project type '%s'.",
        .display_checked_value(project$type)
      ),
      call. = FALSE
    )
  }

  project = switch(
    project$strategy,
    regular = .prepare_regular_project(project),
    structured = .prepare_structured_project(project),
    direct = .prepare_direct_project(project),
    stop(
      sprintf(
        "Unsupported publication strategy '%s'.",
        .display_checked_value(project$strategy)
      ),
      call. = FALSE
    )
  )

  project
}

.report_prepare = function(plan) {
  message("IASI Quarto preparation")
  message("------------------------")
  message(sprintf(
    "Status  : %s",
    if (isTRUE(plan$prepared)) {
      "PREPARED"
    } else {
      "NOT PREPARED"
    }
  ))
  message(sprintf(
    "Projects: %d",
    length(plan$projects)
  ))
  message(sprintf(
    "Changed : %s",
    if (isTRUE(plan$changed)) {
      "yes"
    } else {
      "no"
    }
  ))

  for (project in plan$projects) {
    publication = project$publication

    message(sprintf(
      "- %s [%s, %d artifacts]",
      project$name,
      project$strategy,
      length(publication$artifacts)
    ))
  }

  invisible(plan)
}
