test_that("generate_book_structure creates the book YAML", {

  fixture = test_path(
    "fixtures",
    "book-parts"
  )

  project = withr::local_tempdir()

  file.copy(
    from = list.files(
      fixture,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    ),
    to = project,
    recursive = TRUE
  )

  withr::local_dir(project)

  result = generate_book_structure("parts")

  expect_true(result)
  expect_true(file.exists("_book-structure.yml"))

  structure = yaml::read_yaml("_book-structure.yml")

  expect_length(
    structure$book$chapters,
    2
  )
})

test_that("generate_book_structure creates chapter indexes", {

  fixture = test_path(
    "fixtures",
    "book-chapters"
  )

  project = withr::local_tempdir()

  file.copy(
    from = list.files(
      fixture,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    ),
    to = project,
    recursive = TRUE
  )

  withr::local_dir(project)

  result = generate_book_structure("chapters")

  expect_true(result)
  expect_true(file.exists("_book-structure.yml"))

  first_index = file.path(
    "chapters",
    "01-origin",
    "index.qmd"
  )

  expect_true(file.exists(first_index))

  expect_equal(
    readLines(first_index, warn = FALSE),
    c(
      "{{< include 01-problem.qmd >}}",
      "",
      "{{< include 02-evolution.qmd >}}",
      "",
      "{{< include 03-conclusion.qmd >}}"
    )
  )

  structure = yaml::read_yaml("_book-structure.yml")

  expect_equal(
    structure$book$chapters,
    c(
      "chapters/01-origin/index.qmd",
      "chapters/02-architecture/index.qmd"
    )
  )
})