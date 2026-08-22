test_that("profile output-file has priority over the common book output-file", {
  root = tempfile("iasi-output-file-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  writeLines(
    c(
      "book:",
      '  output-file: "profile-name"'
    ),
    file.path(root, "_quarto-pdf.yml")
  )

  project = list(
    name = "default-name",
    path = root,
    quarto = list(
      book = list(
        "output-file" = "common-name"
      )
    )
  )

  expect_identical(
    .resolve_export_output_file(project, "pdf"),
    "profile-name"
  )
})

test_that("common book output-file is used when the profile does not declare one", {
  root = tempfile("iasi-output-file-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  writeLines(
    c(
      "project:",
      "  output-dir: _outputs/pdf",
      "",
      "format:",
      "  pdf: default"
    ),
    file.path(root, "_quarto-pdf.yml")
  )

  project = list(
    name = "default-name",
    path = root,
    quarto = list(
      book = list(
        "output-file" = "common-name"
      )
    )
  )

  expect_identical(
    .resolve_export_output_file(project, "pdf"),
    "common-name"
  )
})

test_that("project name is the final output-file fallback", {
  root = tempfile("iasi-output-file-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  project = list(
    name = "default-name",
    path = root,
    quarto = list()
  )

  expect_identical(
    .resolve_export_output_file(project, "pdf"),
    "default-name"
  )
})
