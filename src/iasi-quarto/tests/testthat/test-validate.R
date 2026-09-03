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

test_that("validate recognises a book with any supported output profile", {
  root = tempfile("iasi-validate-profile-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  writeLines(c("project:", "  type: book"), file.path(root, "_quarto.yml"))
  writeLines("publication: {}", file.path(root, "_iasi.yml"))
  file.create(file.path(root, "_quarto-odt.yml"))

  expect_true(.is_iasi_publication(root))
})

test_that("recursive discovery ignores tests directories", {
  root = tempfile("iasi-validate-ignore-tests-")
  book = file.path(root, "book")
  fixture = file.path(root, "tests", "testthat", "fixtures", "malformed")
  dir.create(book, recursive = TRUE)
  dir.create(fixture, recursive = TRUE)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  writeLines(c("project:", "  type: book"), file.path(book, "_quarto.yml"))
  writeLines("publication: {}", file.path(book, "_iasi.yml"))
  file.create(file.path(book, "_quarto-html.yml"))

  writeLines(c("project:", "  type: [book"), file.path(fixture, "_quarto.yml"))
  writeLines("publication: {}", file.path(fixture, "_iasi.yml"))
  file.create(file.path(fixture, "_quarto-html.yml"))

  plan = suppressMessages(validate(root))
  expect_identical(plan$books, normalizePath(book, winslash = "/", mustWork = TRUE))
})
