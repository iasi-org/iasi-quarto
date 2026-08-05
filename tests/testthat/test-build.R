test_that("build returns a structured publication", {

  publication_root <- copy_fixture_project("structured")

  project <- validate(
    discover(publication_root)
  )

  publication <- build(project)

  expect_s3_class(
    publication,
    "iasi_quarto_publication"
  )
  expect_identical(
    publication$path,
    normalizePath(
      publication_root,
      winslash = "/",
      mustWork = TRUE
    )
  )
  expect_identical(publication$type, "book")
  expect_identical(publication$source, "structured")
  expect_true(publication$changed)

  expect_identical(
    publication$chapters,
    c(
      "index.qmd",
      "01-manifesto.qmd",
      "02-principles.qmd",
      "chapters/01-intro/index.qmd",
      "chapters/01-intro/01-first.qmd",
      "chapters/02-core/index.qmd"
    )
  )

  expect_identical(
    publication$artifacts,
    "_book-structure.yml"
  )
})


test_that("build writes the structured book YAML", {

  publication_root <- copy_fixture_project("structured")

  publication <- build(
    validate(
      discover(publication_root)
    )
  )

  output <- file.path(
    publication$path,
    "_book-structure.yml"
  )

  expect_true(file.exists(output))

  expect_identical(
    readLines(
      output,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    c(
      "book:",
      "  chapters:",
      "    - \"index.qmd\"",
      "    - \"01-manifesto.qmd\"",
      "    - \"02-principles.qmd\"",
      "    - part: \"chapters/01-intro/index.qmd\"",
      "      chapters:",
      "        - \"chapters/01-intro/01-first.qmd\"",
      "    - part: \"chapters/02-core/index.qmd\""
    )
  )
})


test_that("structured build is idempotent", {

  publication_root <- copy_fixture_project("structured")

  project <- validate(
    discover(publication_root)
  )

  first <- build(project)
  second <- build(project)

  expect_true(first$changed)
  expect_false(second$changed)

  expect_identical(
    first$artifacts,
    second$artifacts
  )
})


test_that("build returns a regular publication", {

  publication_root <- copy_fixture_project("regular")

  publication <- build(
    validate(
      discover(publication_root)
    )
  )

  expect_s3_class(
    publication,
    "iasi_quarto_publication"
  )
  expect_identical(publication$type, "book")
  expect_identical(publication$source, "regular")
  expect_true(publication$changed)

  expect_identical(
    publication$chapters,
    c(
      "index.qmd",
      "chapters/01-intro/index.qmd",
      "chapters/02-install/index.qmd"
    )
  )

  expect_identical(
    publication$artifacts,
    c(
      "chapters/01-intro/index.qmd",
      "chapters/02-install/index.qmd",
      "_book-structure.yml"
    )
  )
})


test_that("regular build creates chapter indexes", {

  publication_root <- copy_fixture_project("regular")

  publication <- build(
    validate(
      discover(publication_root)
    )
  )

  intro_index <- file.path(
    publication$path,
    "chapters",
    "01-intro",
    "index.qmd"
  )

  install_index <- file.path(
    publication$path,
    "chapters",
    "02-install",
    "index.qmd"
  )

  expect_true(file.exists(intro_index))
  expect_true(file.exists(install_index))

  expect_identical(
    readLines(
      intro_index,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    c(
      "{{< include 00-index.qmd >}}",
      "",
      "{{< include 01-what.qmd >}}"
    )
  )

  expect_identical(
    readLines(
      install_index,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    "{{< include 00-index.qmd >}}"
  )
})


test_that("regular build writes the book structure", {

  publication_root <- copy_fixture_project("regular")

  publication <- build(
    validate(
      discover(publication_root)
    )
  )

  output <- file.path(
    publication$path,
    "_book-structure.yml"
  )

  expect_true(file.exists(output))

  expect_identical(
    readLines(
      output,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    c(
      "book:",
      "  chapters:",
      "    - \"index.qmd\"",
      "    - \"chapters/01-intro/index.qmd\"",
      "    - \"chapters/02-install/index.qmd\""
    )
  )
})


test_that("regular build is idempotent", {

  publication_root <- copy_fixture_project("regular")

  project <- validate(
    discover(publication_root)
  )

  first <- build(project)
  second <- build(project)

  expect_true(first$changed)
  expect_false(second$changed)

  expect_identical(
    first$artifacts,
    second$artifacts
  )
})

test_that("build returns a direct publication without generating artifacts", {
  publication_root <- copy_fixture_project("direct")

  publication <- build(
    validate(
      discover(publication_root)
    )
  )

  expect_s3_class(publication, "iasi_quarto_publication")
  expect_identical(publication$type, "book")
  expect_identical(publication$source, "direct")
  expect_false(publication$changed)
  expect_length(publication$artifacts, 0L)

  expect_identical(
    publication$chapters,
    c("index.qmd", "notes.qmd")
  )

  expect_false(
    file.exists(
      file.path(publication$path, "_book-structure.yml")
    )
  )
})


test_that("direct folder markers are not included as content", {
  publication_root <- copy_fixture_project("direct-folders")

  publication <- build(
    validate(
      discover(publication_root)
    )
  )

  expect_identical(publication$source, "direct")
  expect_false(any(grepl("index\\.txt$", publication$chapters)))
  expect_false(any(grepl("00-index\\.txt$", publication$chapters)))
  expect_true("chapters/01-draft/draft.qmd" %in% publication$chapters)
})
