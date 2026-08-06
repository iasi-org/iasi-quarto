.validation_fixture = function(...) {
  testthat::test_path(
    "fixtures",
    "validate",
    ...
  )
}

test_that("validate recognises the current IASI publication", {
  root = .validation_fixture("publication")

  expect_message(
    validate(root),
    "IASI Quarto publication found"
  )

  plan = suppressMessages(
    validate(root)
  )

  expect_s3_class(
    plan,
    "iasi_quarto_plan"
  )

  expect_true(plan$current)

  expect_identical(
    plan$books,
    normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    )
  )
})

test_that("validate recognises an IASI multiproject recursively", {
  root = .validation_fixture("multiproject")

  first = file.path(
    root,
    "01-user-guide"
  )

  second = file.path(
    root,
    "group",
    "02-developer-guide"
  )

  expect_message(
    validate(root),
    "IASI Quarto multiproject found"
  )

  plan = suppressMessages(
    validate(root)
  )

  expect_s3_class(
    plan,
    "iasi_quarto_plan"
  )

  expect_false(plan$current)

  expect_identical(
    plan$books,
    sort(normalizePath(
      c(first, second),
      winslash = "/",
      mustWork = TRUE
    ))
  )
})

test_that("validate ignores incomplete or absent IASI signatures", {
  root = .validation_fixture("not-iasi")

  paths = c(
    file.path(root, "neither"),
    file.path(root, "quarto-only"),
    file.path(root, "iasi-only")
  )

  for (path in paths) {
    expect_message(
      validate(path),
      "Not an IASI Quarto project"
    )

    expect_null(
      suppressMessages(
        validate(path)
      )
    )
  }
})

test_that("validate treats the current publication as the workspace", {
  root = .validation_fixture("current")

  plan = suppressMessages(
    validate(root)
  )

  expect_true(plan$current)

  expect_identical(
    plan$books,
    normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    )
  )
})
