test_that("render rejects objects that are not publications", {

  expect_error(
    render(publication = list()),
    "must be an object returned by build",
    fixed = TRUE
  )
})


test_that("publication stores the project path required by render", {

  publication_root <- copy_fixture_project("structured")

  publication <- build(
    validate(
      discover(publication_root)
    )
  )

  expect_true(dir.exists(publication$path))
  expect_true(
    file.exists(
      file.path(
        publication$path,
        "_quarto.yml"
      )
    )
  )
})
