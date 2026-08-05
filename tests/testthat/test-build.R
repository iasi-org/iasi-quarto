test_that("build creates the structure of a structured publication", {

  source <- fixture_path("structured")
  root <- withr::local_tempdir()

  file.copy(
    source,
    root,
    recursive = TRUE,
    copy.date = TRUE
  )

  project_root <- file.path(root, basename(source))

  project <- discover(project_root)
  validation <- validate(project)

  expect_true(validation$valid)

  result <- build(validation)

  output <- file.path(project_root, "_book-structure.yml")

  expect_true(file.exists(output))
  expect_true(result$built)
  expect_true(result$changed)
  expect_identical(
    result$generated,
    "_book-structure.yml"
  )

  content <- readLines(
    output,
    warn = FALSE,
    encoding = "UTF-8"
  )

  expect_identical(
    content,
    c(
      "book:",
      "  chapters:",
      "    - \"index.qmd\"",
      "    - \"01-manifesto.qmd\"",
      "    - \"02-principles.qmd\"",
      "    - part: \"chapters/01-intro/index.qmd\"",
      "      chapters:",
      "        - \"chapters/01-intro/01-context.qmd\"",
      "        - \"chapters/01-intro/02-purpose.qmd\"",
      "    - part: \"chapters/02-engineering/index.qmd\"",
      "      chapters:",
      "        - \"chapters/02-engineering/01-foundations.qmd\""
    )
  )
})

test_that("build is idempotent for a structured publication", {

  source <- fixture_path("structured")
  root <- withr::local_tempdir()

  file.copy(
    source,
    root,
    recursive = TRUE,
    copy.date = TRUE
  )

  project_root <- file.path(root, basename(source))

  validation <- validate(discover(project_root))

  first <- build(validation)
  second <- build(validation)

  expect_true(first$changed)
  expect_false(second$changed)

  expect_identical(
    first$generated,
    second$generated
  )
})
