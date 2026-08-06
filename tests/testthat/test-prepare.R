test_that("prepare returns a structured publication", {

  publication_root = copy_fixture_project("structured")

  project = validate(
    discover(publication_root)
  )

  publication = prepare(project)

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


test_that("prepare writes the structured book YAML", {

  publication_root = copy_fixture_project("structured")

  publication = prepare(
    validate(
      discover(publication_root)
    )
  )

  output = file.path(
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


test_that("structured prepare is idempotent", {

  publication_root = copy_fixture_project("structured")

  project = validate(
    discover(publication_root)
  )

  first = prepare(project)
  second = prepare(project)

  expect_true(first$changed)
  expect_false(second$changed)

  expect_identical(
    first$artifacts,
    second$artifacts
  )
})


test_that("prepare returns a regular publication", {

  publication_root = copy_fixture_project("regular")

  publication = prepare(
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
      "chapters/01-intro/00-index.qmd",
      "chapters/01-intro/01-what.qmd",
      "chapters/02-install/00-index.qmd"
    )
  )

  expect_identical(
    publication$artifacts,
    "_book-structure.yml"
  )
})


test_that("regular prepare does not create chapter indexes", {

  publication_root = copy_fixture_project("regular")

  publication = prepare(
    validate(
      discover(publication_root)
    )
  )

  intro_index = file.path(
    publication_root,
    "chapters",
    "01-intro",
    "index.qmd"
  )

  install_index = file.path(
    publication_root,
    "chapters",
    "02-install",
    "index.qmd"
  )

  expect_false(file.exists(intro_index))
  expect_false(file.exists(install_index))

  expect_identical(
    publication$artifacts,
    "_book-structure.yml"
  )
})


test_that("regular prepare writes the book structure", {

  publication_root = copy_fixture_project("regular")

  publication = prepare(
    validate(
      discover(publication_root)
    )
  )

  output = file.path(
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
      "    - \"chapters/01-intro/00-index.qmd\"",
      "    - \"chapters/01-intro/01-what.qmd\"",
      "    - \"chapters/02-install/00-index.qmd\""
    )
  )
})


test_that("regular prepare is idempotent", {

  publication_root = copy_fixture_project("regular")

  project = validate(
    discover(publication_root)
  )

  first = prepare(project)
  second = prepare(project)

  expect_true(first$changed)
  expect_false(second$changed)

  expect_identical(
    first$artifacts,
    second$artifacts
  )
})


test_that("prepare returns a direct publication without generating artifacts", {

  publication_root = copy_fixture_project("direct")

  publication = prepare(
    validate(
      discover(publication_root)
    )
  )

  expect_s3_class(
    publication,
    "iasi_quarto_publication"
  )

  expect_identical(publication$type, "book")
  expect_identical(publication$source, "direct")
  expect_false(publication$changed)
  expect_length(publication$artifacts, 0L)

  expect_identical(
    publication$chapters,
    c(
      "index.qmd",
      "notes.qmd"
    )
  )

  expect_false(
    file.exists(
      file.path(
        publication$path,
        "_book-structure.yml"
      )
    )
  )
})


test_that("direct folder markers are not included as content", {

  publication_root = copy_fixture_project("direct-folders")

  publication = prepare(
    validate(
      discover(publication_root)
    )
  )

  expect_identical(publication$source, "direct")
  expect_false(any(grepl("index\\.txt$", publication$chapters)))
  expect_false(any(grepl("00-index\\.txt$", publication$chapters)))
  expect_true(
    "chapters/01-draft/draft.qmd" %in%
      publication$chapters
  )
})