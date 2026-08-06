.assert_plan = function(plan) {
  if (!inherits(plan, "iasi_quarto_plan")) {
    stop(
      "`plan` must be an object returned by validate().",
      call. = FALSE
    )
  }

  invisible(plan)
}

.assert_discovered_plan = function(plan) {
  .assert_plan(plan)

  if (
    is.null(plan$projects) ||
      !is.list(plan$projects) ||
      !length(plan$projects)
  ) {
    stop(
      "`plan` must be an object returned by discover().",
      call. = FALSE
    )
  }

  valid_projects = vapply(
    plan$projects,
    inherits,
    logical(1),
    what = "iasi_quarto_project"
  )

  if (!all(valid_projects)) {
    stop(
      "`plan` must be an object returned by discover().",
      call. = FALSE
    )
  }

  invisible(plan)
}
