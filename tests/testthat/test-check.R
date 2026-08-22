test_that("check requires a discovered plan", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "regular-without-index"
  )

  expect_error(
    check(
      validate(root)
    ),
    "`plan` must be an object returned by discover().",
    fixed = TRUE
  )
})

test_that("check accepts a regular publication without generated indexes", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "regular-without-index"
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  expect_true(plan$valid)
  expect_true(plan$projects[[1L]]$valid)
  expect_length(
    plan$projects[[1L]]$errors,
    0L
  )

  folder = plan$projects[[1L]]$folders[[1L]]

  expect_false(folder$has_index)

  expect_identical(
    basename(folder$documents),
    "01-introduction.qmd"
  )
})

test_that("check accepts a regular publication with generated indexes", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "regular-with-index"
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  expect_true(plan$valid)

  folder = plan$projects[[1L]]$folders[[1L]]

  expect_true(folder$has_index)

  expect_identical(
    basename(folder$documents),
    "01-introduction.qmd"
  )
})

test_that("check requires an author index in structured folders", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "structured-missing-index"
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  expect_false(plan$valid)

  expect_match(
    plan$projects[[1L]]$errors,
    "Structured folder is missing index.qmd"
  )
})

test_that("check accepts structured folders with index.qmd", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "structured-valid"
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  expect_true(plan$valid)
  expect_true(
    plan$projects[[1L]]$folders[[1L]]$has_index
  )
})

test_that("check rejects invalid IASI values", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "invalid-values"
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  errors = plan$projects[[1L]]$errors

  expect_false(plan$valid)

errors_text = paste(
  errors,
  collapse = "\n"
)

expect_match(
  errors_text,
  "Invalid publication strategy"
)

expect_match(
  errors_text,
  "content-dir must be a non-empty string"
)

expect_match(
  errors_text,
  "numbered must be TRUE or FALSE"
)  
    
})

test_that("check tolerates unknown IASI publication keys", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "unknown-key"
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  expect_true(plan$valid)
  expect_length(
    plan$projects[[1L]]$errors,
    0L
  )
})

test_that("check accepts extension publication keys such as version", {
  root = .copy_test_fixture(
    "check",
    "regular-without-index"
  )

  iasi_file = file.path(root, "_iasi.yml")
  iasi = readLines(iasi_file, warn = FALSE)

  writeLines(
    c(
      iasi,
      '  version: "0.1.0"'
    ),
    iasi_file
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  expect_true(plan$valid)
  expect_length(
    plan$projects[[1L]]$errors,
    0L
  )
})

test_that("check requires root index.qmd", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "missing-root-index"
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  expect_false(plan$valid)

  expect_match(
    plan$projects[[1L]]$errors,
    "Missing root index.qmd"
  )
})

test_that("check requires the declared content directory", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "missing-content-dir"
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  expect_false(plan$valid)

  expect_match(
    plan$projects[[1L]]$errors,
    "Content directory does not exist"
  )
})

test_that("check applies numbered to folders and source documents", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "numbered"
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  project = plan$projects[[1L]]

  expect_true(plan$valid)

  expect_identical(
    vapply(
      project$folders,
      `[[`,
      character(1),
      "name"
    ),
    "01-intro"
  )

  expect_identical(
    basename(
      project$folders[[1L]]$documents
    ),
    "01-included.qmd"
  )
})

test_that("check includes arbitrary folders and files when numbered is false", {
  root = testthat::test_path(
    "fixtures",
    "check",
    "not-numbered"
  )

  plan = check(
    discover(
      validate(root)
    )
  )

  project = plan$projects[[1L]]

  expect_true(plan$valid)

  expect_identical(
    vapply(
      project$folders,
      `[[`,
      character(1),
      "name"
    ),
    "introduction"
  )

  expect_identical(
    sort(
      basename(
        project$folders[[1L]]$documents
      )
    ),
    c(
      "overview.qmd",
      "what-is.qmd"
    )
  )
})
