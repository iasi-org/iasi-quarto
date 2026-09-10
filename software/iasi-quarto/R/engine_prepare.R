.prepare_project = function(project) {
  switch(
    project$type,
    book = .prepare_book_project(project),
    website = .prepare_web_project(project),
    stop(
      sprintf(
        "Unsupported Quarto project type '%s'.",
        .display_checked_value(project$type)
      ),
      call. = FALSE
    )
  )
}

.prepare_book_project = function(project) {
  switch(
    project$strategy,
    regular = .prepare_regular_project(project),
    structured = .prepare_structured_project(project),
    parted = .prepare_parted_project(project),
    direct = .prepare_direct_project(project),
    stop(
      sprintf(
        "Unsupported publication strategy '%s'.",
        .display_checked_value(project$strategy)
      ),
      call. = FALSE
    )
  )
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
      "- %s [%s, %s, %d artifacts]",
      project$name,
      project$type,
      project$strategy,
      length(publication$artifacts)
    ))
  }

  invisible(plan)
}
