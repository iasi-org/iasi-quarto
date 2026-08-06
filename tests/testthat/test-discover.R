test_that("discover requires a validated plan", {
  expect_error(
    discover(list()),
    "`plan` must be an object returned by validate().",
    fixed = TRUE
  )
})

test_that("discover reads an explicitly configured publication", {
  root = testthat::test_path(
    "fixtures",
    "discover",
    "complete"
  )

  plan = discover(
    validate(root)
  )

  expect_s3_class(
    plan,
    "iasi_quarto_plan"
  )

  expect_length(
    plan$projects,
    1L
  )

  project = plan$projects[[1L]]

  expect_s3_class(
    project,
    "iasi_quarto_project"
  )

  expect_identical(
    project$type,
    "book"
  )

  expect_identical(
    project$strategy,
    "structured"
  )

  expect_identical(
    project$content_dir,
    "sections"
  )

  expect_false(
    project$numbered
  )
})

test_that("discover applies defaults to an empty iasi.yml", {
  root = testthat::test_path(
    "fixtures",
    "discover",
    "empty"
  )

  project = discover(
    validate(root)
  )$projects[[1L]]

  expect_identical(
    project$strategy,
    "regular"
  )

  expect_identical(
    project$content_dir,
    "chapters"
  )

  expect_true(
    project$numbered
  )
})

test_that("discover applies defaults only to missing keys", {
  root = testthat::test_path(
    "fixtures",
    "discover",
    "partial"
  )

  project = discover(
    validate(root)
  )$projects[[1L]]

  expect_identical(
    project$strategy,
    "direct"
  )

  expect_identical(
    project$content_dir,
    "chapters"
  )

  expect_true(
    project$numbered
  )
})

test_that("discover fails when iasi.yml is malformed", {
  root = testthat::test_path(
    "fixtures",
    "discover",
    "malformed-iasi"
  )

  expect_error(
    discover(
      validate(root)
    ),
    "Invalid YAML file"
  )

  expect_error(
    discover(
      validate(root)
    ),
    "iasi.yml",
    fixed = TRUE
  )
})

test_that("discover fails when _quarto.yml is malformed", {
  root = testthat::test_path(
    "fixtures",
    "discover",
    "malformed-quarto"
  )

  expect_error(
    discover(
      validate(root)
    ),
    "Invalid YAML file"
  )

  expect_error(
    discover(
      validate(root)
    ),
    "_quarto.yml",
    fixed = TRUE
  )
})

test_that("discover preserves semantically invalid values", {
  root = testthat::test_path(
    "fixtures",
    "discover",
    "invalid-values"
  )

  project = discover(
    validate(root)
  )$projects[[1L]]

  expect_identical(
    project$strategy,
    "banana"
  )

  expect_identical(
    project$content_dir,
    42L
  )

  expect_identical(
    project$numbered,
    "sometimes"
  )

  expect_null(
    project$content_path
  )
})
