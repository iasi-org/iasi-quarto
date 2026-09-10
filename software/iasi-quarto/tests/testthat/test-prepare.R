test_that("public prepare runs the complete preparation pipeline", {
  root = .copy_test_fixture(
    "prepare",
    "regular"
  )

  plan = prepare(
    path = root
  )

  expect_true(plan$prepared)
  expect_true(plan$valid)

  expect_true(
    file.exists(
      file.path(
        root,
        "_book-structure.yml"
      )
    )
  )
})

test_that("internal prepare requires a checked plan", {
  root = .copy_test_fixture(
    "prepare",
    "regular"
  )

  plan = .discover(
    validate(root)
  )

  expect_error(
    .prepare(plan),
    "`plan` must be an object returned by check().",
    fixed = TRUE
  )
})

test_that("internal prepare is stable when artifacts have not changed", {
  root = .copy_test_fixture(
    "prepare",
    "regular"
  )

  checked = .check(
    .discover(
      validate(root)
    )
  )

  first = .prepare(checked)
  second = .prepare(first)

  expect_true(first$changed)
  expect_false(second$changed)
})

test_that("prepare preserves structured author indexes", {
  root = .copy_test_fixture(
    "prepare",
    "structured"
  )

  index_path = file.path(
    root,
    "chapters",
    "01-intro",
    "index.qmd"
  )

  original = readLines(
    index_path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  plan = .prepare(
    .check(
      .discover(
        validate(root)
      )
    )
  )

  expect_identical(
    readLines(
      index_path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    original
  )

  expect_true(plan$valid)
})

test_that("prepare writes a flat direct structure", {
  root = .copy_test_fixture(
    "prepare",
    "direct"
  )

  plan = .prepare(
    .check(
      .discover(
        validate(root)
      )
    )
  )

  expect_identical(
    plan$projects[[1L]]$publication$chapters,
    c(
      "index.qmd",
      "chapters/01-intro/01-first.qmd",
      "chapters/02-more/01-second.qmd"
    )
  )
})
