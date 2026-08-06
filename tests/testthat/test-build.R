test_that("build returns NULL for a non-IASI directory", {
  root = .copy_test_fixture(
    "build",
    "not-iasi"
  )

  testthat::local_mocked_bindings(
    .render = function(publication, format) {
      fail("render must not be called")
    },
    .package = "iasi.quarto"
  )

  expect_null(
    build(path = root)
  )
})

test_that("build renders one publication in every format by default", {
  root = .copy_test_fixture(
    "build",
    "single"
  )

  calls = new.env(
    parent = emptyenv()
  )

  calls$books = character()
  calls$formats = character()

  testthat::local_mocked_bindings(
    .render = function(publication, format) {
      calls$books = c(
        calls$books,
        basename(publication$path)
      )

      calls$formats = c(
        calls$formats,
        format
      )

      publication$rendered = TRUE
      publication$profiles = unique(c(
        publication$profiles,
        format
      ))

      publication
    },
    .package = "iasi.quarto"
  )

  plan = build(path = root)

  expect_true(plan$rendered)

  expect_identical(
    calls$formats,
    c(
      "html",
      "pdf"
    )
  )

  expect_identical(
    plan$formats,
    c(
      "html",
      "pdf"
    )
  )

  expect_identical(
    plan$projects[[1L]]$publication$profiles,
    c(
      "html",
      "pdf"
    )
  )
})

test_that("build accepts one selected format", {
  root = .copy_test_fixture(
    "build",
    "single"
  )

  calls = new.env(
    parent = emptyenv()
  )

  calls$formats = character()

  testthat::local_mocked_bindings(
    .render = function(publication, format) {
      calls$formats = c(
        calls$formats,
        format
      )

      publication$rendered = TRUE
      publication$profiles = unique(c(
        publication$profiles,
        format
      ))

      publication
    },
    .package = "iasi.quarto"
  )

  plan = build(
    path = root,
    format = "html"
  )

  expect_identical(
    calls$formats,
    "html"
  )

  expect_identical(
    plan$formats,
    "html"
  )
})

test_that("build renders every publication in a multiproject", {
  root = .copy_test_fixture(
    "build",
    "multiproject"
  )

  calls = new.env(
    parent = emptyenv()
  )

  calls$books = character()
  calls$formats = character()

  testthat::local_mocked_bindings(
    .render = function(publication, format) {
      calls$books = c(
        calls$books,
        basename(publication$path)
      )

      calls$formats = c(
        calls$formats,
        format
      )

      publication$rendered = TRUE
      publication$profiles = unique(c(
        publication$profiles,
        format
      ))

      publication
    },
    .package = "iasi.quarto"
  )

  plan = build(path = root)

  expect_true(plan$rendered)

  expect_identical(
    calls$books,
    c(
      "01-user-guide",
      "01-user-guide",
      "02-developer-guide",
      "02-developer-guide"
    )
  )

  expect_identical(
    calls$formats,
    c(
      "html",
      "pdf",
      "html",
      "pdf"
    )
  )
})

test_that("build selects a book by name without its numeric prefix", {
  root = .copy_test_fixture(
    "build",
    "multiproject"
  )

  calls = new.env(
    parent = emptyenv()
  )

  calls$books = character()

  testthat::local_mocked_bindings(
    .render = function(publication, format) {
      calls$books = c(
        calls$books,
        basename(publication$path)
      )

      publication$rendered = TRUE
      publication$profiles = unique(c(
        publication$profiles,
        format
      ))

      publication
    },
    .package = "iasi.quarto"
  )

  plan = build(
    path = root,
    book = "user-guide",
    format = "html"
  )

  expect_identical(
    calls$books,
    "01-user-guide"
  )

  expect_length(
    plan$projects,
    1L
  )
})

test_that("build selects a book by numeric prefix", {
  root = .copy_test_fixture(
    "build",
    "multiproject"
  )

  calls = new.env(
    parent = emptyenv()
  )

  calls$books = character()

  testthat::local_mocked_bindings(
    .render = function(publication, format) {
      calls$books = c(
        calls$books,
        basename(publication$path)
      )

      publication$rendered = TRUE
      publication$profiles = unique(c(
        publication$profiles,
        format
      ))

      publication
    },
    .package = "iasi.quarto"
  )

  build(
    path = root,
    book = "2",
    format = "pdf"
  )

  expect_identical(
    calls$books,
    "02-developer-guide"
  )
})

test_that("build warns about missing books and builds resolved books", {
  root = .copy_test_fixture(
    "build",
    "multiproject"
  )

  calls = new.env(
    parent = emptyenv()
  )

  calls$books = character()

  testthat::local_mocked_bindings(
    .render = function(publication, format) {
      calls$books = c(
        calls$books,
        basename(publication$path)
      )

      publication$rendered = TRUE
      publication$profiles = unique(c(
        publication$profiles,
        format
      ))

      publication
    },
    .package = "iasi.quarto"
  )

  expect_warning(
    build(
      path = root,
      book = c(
        "user-guide",
        "missing"
      ),
      format = "html"
    ),
    'Books not found: "missing".',
    fixed = TRUE
  )

  expect_identical(
    calls$books,
    "01-user-guide"
  )
})

test_that("build rejects unsupported formats before rendering", {
  root = .copy_test_fixture(
    "build",
    "single"
  )

  testthat::local_mocked_bindings(
    .render = function(publication, format) {
      fail("render must not be called")
    },
    .package = "iasi.quarto"
  )

  expect_error(
    build(
      path = root,
      format = "docx"
    ),
    'Unsupported format: "docx".',
    fixed = TRUE
  )
})

test_that("build ignores book selection for a current publication", {
  root = .copy_test_fixture(
    "build",
    "single"
  )

  testthat::local_mocked_bindings(
    .render = function(publication, format) {
      publication$rendered = TRUE
      publication$profiles = unique(c(
        publication$profiles,
        format
      ))

      publication
    },
    .package = "iasi.quarto"
  )

plan = NULL

expect_warning(
  {
    plan = build(
      path = root,
      book = "something-else",
      format = "html"
    )
  },
  regexp = "The `book` selection will be ignored.",
  fixed = TRUE
)

expect_length(
  plan$projects,
  1L
)

})

test_that("build stops before rendering an invalid publication", {
  root = .copy_test_fixture(
    "build",
    "invalid"
  )

  testthat::local_mocked_bindings(
    .render = function(publication, format) {
      fail("render must not be called")
    },
    .package = "iasi.quarto"
  )

  expect_error(
    build(
      path = root,
      format = "html"
    ),
    "IASI Quarto check failed:",
    fixed = TRUE
  )
})
