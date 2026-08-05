test_that("discover requires _quarto.yml in the selected directory", {
  expect_error(
    discover(fixture_path("missing-quarto")),
    "missing _quarto.yml",
    fixed = TRUE
  )
})

test_that("discover identifies a structured project", {
  project <- discover(fixture_path("structured"))

  expect_s3_class(project, "iasi_quarto_project")
  expect_identical(project$type, "structured")
  expect_identical(project$counts$structured, 2L)
  expect_identical(project$counts$regular, 0L)
  expect_identical(
    vapply(project$folders, `[[`, character(1), "name"),
    c("01-intro", "02-core")
  )
})

test_that("discover identifies a regular project", {
  project <- discover(fixture_path("regular"))

  expect_identical(project$type, "regular")
  expect_identical(project$counts$regular, 2L)
  expect_identical(folder_strategy(project, "01-intro"), "regular")
})

test_that("a generated index does not make a regular folder incoherent", {
  project <- discover(fixture_path("regular-generated-index"))
  folder <- project$folders[[1L]]

  expect_identical(project$type, "regular")
  expect_true(folder$generated_index)
  expect_identical(folder$strategy, "regular")
})

test_that("discover identifies a direct project without numbered folders", {
  project <- discover(fixture_path("direct"))

  expect_identical(project$type, "direct")
  expect_length(project$folders, 0L)
  expect_true(any(grepl("index\\.qmd$", project$root_documents)))
})

test_that("projects made only of direct folders are direct", {
  project <- discover(fixture_path("direct-folders"))

  expect_identical(project$type, "direct")
  expect_identical(project$counts$direct, 2L)
  expect_true(
    all(vapply(project$folders, `[[`, character(1), "strategy") == "direct")
  )
})

test_that("index.txt and 00-index.txt are equivalent direct markers", {
  project <- discover(fixture_path("direct-folders"))

  expect_identical(folder_strategy(project, "01-draft"), "direct")
  expect_identical(folder_strategy(project, "02-notes"), "direct")
  expect_identical(project$counts$direct, 2L)
})

test_that("a direct folder does not change the dominant regular model", {
  project <- discover(fixture_path("regular-with-direct"))

  expect_identical(project$type, "regular")
  expect_identical(folder_strategy(project, "01-intro"), "regular")
  expect_identical(folder_strategy(project, "02-draft"), "direct")
  expect_identical(project$counts$regular, 1L)
  expect_identical(project$counts$direct, 1L)
})

test_that("structured and regular folders produce an incoherent result", {
  project <- discover(fixture_path("incoherent-models"))

  expect_identical(project$type, "incoherent")
  expect_identical(project$counts$structured, 1L)
  expect_identical(project$counts$regular, 1L)
})

test_that("index.qmd and 00-index.qmd in the same folder are discovered as incoherent", {
  project <- discover(fixture_path("incoherent-same-folder"))

  expect_identical(project$type, "incoherent")
  expect_identical(folder_strategy(project, "01-broken"), "incoherent")
})

test_that("a numbered folder without a declaration is discovered as incoherent", {
  project <- discover(fixture_path("incoherent-unclassified"))

  expect_identical(project$type, "incoherent")
  expect_identical(folder_strategy(project, "01-orphan"), "unclassified")
})

test_that("00-prefixed and unnumbered folders are ignored", {
  project <- discover(fixture_path("ignored-folders"))
  names <- vapply(project$folders, `[[`, character(1), "name")

  expect_identical(project$type, "regular")
  expect_identical(names, "01-content")
})

test_that("discover works from a project directory that is not a repository root", {
  project <- withr::with_dir(
    fixture_path("regular"),
    discover()
  )

  expect_identical(project$type, "regular")
  expect_true(endsWith(project$path, "/regular"))
})
