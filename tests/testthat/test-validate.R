test_that("structured project validates", {

  project <- discover(fixture_path("structured"))

  project <- validate(project)

  expect_s3_class(project, "iasi_quarto_project")
  expect_true(project$valid)
  expect_identical(project$type, "structured")
  expect_length(project$errors, 0L)

})

test_that("regular project validates", {

  project <- discover(fixture_path("regular"))

  project <- validate(project)

  expect_true(project$valid)
  expect_identical(project$type, "regular")
  expect_length(project$errors, 0L)

})

test_that("paper project validates", {

  project <- discover(fixture_path("paper"))

  project <- validate(project)

  expect_true(project$valid)
  expect_identical(project$type, "paper")
  expect_length(project$errors, 0L)

})

test_that("incoherent publication is rejected", {

  project <- discover(fixture_path("incoherent-models"))

  project <- validate(project)

  expect_false(project$valid)
  expect_identical(project$type, "incoherent")
  expect_gt(length(project$errors), 0L)

})

test_that("conflicting indexes are rejected", {

  project <- discover(fixture_path("incoherent-same-folder"))

  project <- validate(project)

  expect_false(project$valid)

  expect_true(
    any(
      grepl(
        "both index.qmd and 00-index.qmd",
        project$errors,
        fixed = TRUE
      )
    )
  )

})

test_that("folders without declaration are rejected", {

  project <- discover(fixture_path("incoherent-unclassified"))

  project <- validate(project)

  expect_false(project$valid)
  expect_gt(length(project$errors), 0L)

})

test_that("manual folders generate warnings", {

  project <- discover(fixture_path("regular-with-as-is"))

  project <- validate(project)

  expect_true(project$valid)
  expect_length(project$warnings, 1L)

})

test_that("mixed projects remain valid", {

  project <- discover(fixture_path("mixed"))

  project <- validate(project)

  expect_true(project$valid)
  expect_identical(project$type, "mixed")
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