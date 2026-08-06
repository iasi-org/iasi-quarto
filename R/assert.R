.assert_plan = function(plan) {
  if (!inherits(plan, "iasi_quarto_plan")) {
    stop(
      "`plan` must be an object returned by validate().",
      call. = FALSE
    )
  }

  invisible(plan)
}
