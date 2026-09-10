test_that("output directories default below _outputs", {
  expect_identical(
    .resolve_output_dir(
      quarto = list(),
      profile_quarto = list(),
      profile = "html"
    ),
    file.path("_outputs", "html")
  )

  expect_identical(
    .resolve_output_dir(
      quarto = list(),
      profile_quarto = list(),
      profile = "pdf"
    ),
    file.path("_outputs", "pdf")
  )
})

test_that("profile output-dir overrides project and default output roots", {
  expect_identical(
    .resolve_output_dir(
      quarto = list(
        project = list(
          `output-dir` = "build"
        )
      ),
      profile_quarto = list(
        project = list(
          `output-dir` = "custom/html"
        )
      ),
      profile = "html"
    ),
    "custom/html"
  )
})

test_that("project output-dir acts as a common output root", {
  expect_identical(
    .resolve_output_dir(
      quarto = list(
        project = list(
          `output-dir` = "build"
        )
      ),
      profile_quarto = list(),
      profile = "pdf"
    ),
    file.path("build", "pdf")
  )
})

test_that("profile config materialises the default output path", {
  root = tempfile("iasi-paths-")
  dir.create(root)
  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  project = list(
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    quarto = list(),
    type = "book",
    html_landing_page = FALSE
  )

  config = .profile_config(
    project,
    "html"
  )

  expect_identical(
    config$output_dir,
    file.path("_outputs", "html")
  )

  expect_identical(
    config$output_path,
    file.path(
      project$path,
      "_outputs",
      "html"
    )
  )
})
