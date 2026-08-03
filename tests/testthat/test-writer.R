test_that("Book structure is written as YAML", {

  structure = list(
    book = list(
      chapters = c(
        "index.qmd",
        "chapters/01-origin/index.qmd"
      )
    )
  )

  output = tempfile(fileext = ".yml")

  result = iasi.quarto:::.write_structure(
    structure = structure,
    output = output
  )

  expect_true(file.exists(output))
  expect_equal(result, output)

  yaml = yaml::read_yaml(output)

  expect_equal(
    yaml$book$chapters,
    structure$book$chapters
  )
})