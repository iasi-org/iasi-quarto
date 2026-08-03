test_that("Parts layout generates a valid structure", {

    root = test_path(
        "fixtures",
        "book-parts",
        "chapters"
    )

    project = iasi.quarto:::.discover_project(root)
    structure = iasi.quarto:::.create_structure(project, "parts")
    
    expect_true(is.list(structure))
    expect_named(structure, "book")

})

test_that("Parts layout preserves part index and chapter documents", {

  root = test_path(
    "fixtures",
    "book-parts",
    "chapters"
  )

  project = iasi.quarto:::.discover_project(root)

  structure = iasi.quarto:::.create_structure(
    project = project,
    layout = "parts"
  )

  first_part = structure$book$chapters[[1]]

  expect_equal(
    basename(first_part$part),
    "index.qmd"
  )

  expect_setequal(
    basename(first_part$chapters),
    c(
      "01-problem.qmd",
      "02-evolution.qmd"
    )
  )
})

test_that("Chapters layout generates one index per chapter directory", {

  root = test_path(
    "fixtures",
    "book-chapters",
    "chapters"
  )

  project = iasi.quarto:::.discover_project(root)

  structure = iasi.quarto:::.create_structure(
    project = project,
    layout = "chapters"
  )

  expect_equal(
    basename(structure$book$chapters),
    c(
      "index.qmd",
      "index.qmd"
    )
  )

  expect_equal(
    dirname(structure$book$chapters),
    c(
      file.path(root, "01-origin"),
      file.path(root, "02-architecture")
    )
  )
})

test_that("Chapters layout generates one index per chapter directory", {

  root = test_path(
    "fixtures",
    "book-chapters",
    "chapters"
  )

  project = iasi.quarto:::.discover_project(root)

  structure = iasi.quarto:::.create_structure(
    project = project,
    layout = "chapters"
  )

  expect_equal(
    structure$book$chapters,
    c(
      file.path(root, "01-origin", "index.qmd"),
      file.path(root, "02-architecture", "index.qmd")
    )
  )
})

test_that("Chapters layout creates index files with includes", {

  fixture = test_path(
    "fixtures",
    "book-chapters"
  )

  root = file.path(
    fixture,
    "chapters"
  )

  project = iasi.quarto:::.discover_project(root)

  structure = iasi.quarto:::.create_structure(
    project = project,
    layout = "chapters"
  )

  iasi.quarto:::.write_chapter_indexes(project)

  first_index = file.path(
    root,
    "01-origin",
    "index.qmd"
  )

  expect_true(
    file.exists(first_index)
  )

  content = readLines(
    first_index,
    warn = FALSE
  )

  expect_equal(
    content,
    c(
      "{{< include 01-problem.qmd >}}",
      "",
      "{{< include 02-evolution.qmd >}}",
      "",
      "{{< include 03-conclusion.qmd >}}"
    )
  )
})