test_that("structured project validates", {
  project <- validate(discover(fixture_path("structured")))

  expect_s3_class(project, "iasi_quarto_project")
  expect_true(project$valid)
  expect_identical(project$type, "structured")
  expect_length(project$errors, 0L)
})

test_that("regular project validates", {
  project <- validate(discover(fixture_path("regular")))

  expect_true(project$valid)
  expect_identical(project$type, "regular")
  expect_length(project$errors, 0L)
})

test_that("direct project validates", {
  project <- validate(discover(fixture_path("direct")))

  expect_true(project$valid)
  expect_identical(project$type, "direct")
  expect_length(project$errors, 0L)
})

test_that("incoherent publication is rejected", {
  project <- validate(discover(fixture_path("incoherent-models")))

  expect_false(project$valid)
  expect_identical(project$type, "incoherent")
  expect_gt(length(project$errors), 0L)
})

test_that("conflicting indexes are rejected", {
  project <- validate(discover(fixture_path("incoherent-same-folder")))

  expect_false(project$valid)
  expect_true(
    any(grepl(
      "both index.qmd and 00-index.qmd",
      project$errors,
      fixed = TRUE
    ))
  )
})

test_that("folders without declaration are rejected", {
  project <- validate(discover(fixture_path("incoherent-unclassified")))

  expect_false(project$valid)
  expect_gt(length(project$errors), 0L)
})

test_that("direct folders generate warnings in a regular project", {
  project <- validate(discover(fixture_path("regular-with-direct")))

  expect_true(project$valid)
  expect_identical(project$type, "regular")
  expect_length(project$warnings, 1L)
  expect_match(project$warnings, "processed directly", fixed = TRUE)
})

test_that("projects containing only direct folders remain valid", {
  project <- validate(discover(fixture_path("direct-folders")))

  expect_true(project$valid)
  expect_identical(project$type, "direct")
  expect_gt(length(project$warnings), 0L)
})

test_that("validate discovers current project", {
  project <- withr::with_dir(
    fixture_path("regular"),
    validate()
  )

  expect_true(project$valid)
  expect_identical(project$type, "regular")
})
