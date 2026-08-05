fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}

folder_strategy <- function(project, name) {
  matches <- vapply(project$folders, function(x) identical(x$name, name), logical(1))

  if (sum(matches) != 1L) {
    stop(sprintf("Expected exactly one folder named '%s'.", name), call. = FALSE)
  }

  project$folders[[which(matches)]]$strategy
}
