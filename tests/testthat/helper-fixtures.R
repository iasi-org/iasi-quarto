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

copy_fixture_project <- function(name) {

  source <- fixture_path(name)

  temp_root <- withr::local_tempdir(
    .local_envir = parent.frame()
  )

  copied <- file.copy(
    source,
    temp_root,
    recursive = TRUE,
    copy.date = TRUE
  )

  testthat::expect_true(copied)

  project_root <- file.path(
    temp_root,
    basename(source)
  )

  testthat::expect_true(
    dir.exists(project_root)
  )

  project_root
}