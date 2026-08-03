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