test_that("validate accepts a structured project", {
  project <- discover(fixture_path("structured"))

  expect_output(
    validation <- validate(project),
    "Status       : VALID",
    fixed = TRUE
  )

  expect_s3_class(validation, "iasi_quarto_validation")
  expect_true(validation$valid)
  expect_identical(validation$type, "structured")
  expect_length(validation$errors, 0L)
})

test_that("validate accepts a regular project", {
  project <- discover(fixture_path("regular"))

  expect_output(
    validation <- validate(project),
    "Status       : VALID",
    fixed = TRUE
  )

  expect_true(validation$valid)
  expect_identical(validation$type, "regular")
  expect_length(validation$errors, 0L)
})

test_that("validate accepts a paper project", {
  project <- discover(fixture_path("paper"))

  expect_output(
    validation <- validate(project),
    "Status       : VALID",
    fixed = TRUE
  )

  expect_true(validation$valid)
  expect_identical(validation$type, "paper")
  expect_length(validation$errors, 0L)
})

test_that("validate rejects incompatible publication models", {
  project <- discover(fixture_path("incoherent-models"))

  expect_output(
    validation <- validate(project),
    "Status       : INVALID",
    fixed = TRUE
  )

  expect_false(validation$valid)
  expect_identical(validation$type, "incoherent")
  expect_true(length(validation$errors) > 0L)
})

test_that("validate rejects conflicting indexes in the same folder", {
  project <- discover(fixture_path("incoherent-same-folder"))

  expect_output(
    validation <- validate(project),
    "Status       : INVALID",
    fixed = TRUE
  )

  expect_false(validation$valid)
  expect_true(
    any(grepl(
      "both index.qmd and 00-index.qmd",
      validation$errors,
      fixed = TRUE
    ))
  )
})

test_that("validate rejects folders without an index declaration", {
  project <- discover(fixture_path("incoherent-unclassified"))

  expect_output(
    validation <- validate(project),
    "Status       : INVALID",
    fixed = TRUE
  )

  expect_false(validation$valid)
  expect_true(length(validation$errors) > 0L)
})

test_that("validate warns about folders processed as-is", {
  project <- discover(fixture_path("regular-with-as-is"))

  expect_output(
    validation <- validate(project),
    "Status       : VALID",
    fixed = TRUE
  )

  expect_true(validation$valid)
  expect_length(validation$warnings, 1L)
  expect_match(validation$warnings, "index.txt", fixed = TRUE)
})

test_that("validate reports projects containing only as-is folders", {
  project <- discover(fixture_path("mixed"))

  expect_output(
    validation <- validate(project),
    "Status       : VALID",
    fixed = TRUE
  )

  expect_true(validation$valid)
  expect_identical(validation$type, "mixed")
  expect_true(length(validation$warnings) > 0L)
})

test_that("validate can discover the current project itself", {
  validation <- withr::with_dir(
    fixture_path("regular"),
    {
      expect_output(
        result <- validate(),
        "Status       : VALID",
        fixed = TRUE
      )
      result
    }
  )

  expect_true(validation$valid)
  expect_identical(validation$type, "regular")
})