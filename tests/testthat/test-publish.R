test_that("multiproject publish creates a landing page when 00-* is absent", {
  root = tempfile("iasi-publish-multiproject-")
  dir.create(root, recursive = TRUE)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  html_outputs = file.path(root, "rendered")
  dir.create(file.path(html_outputs, "user-guide"), recursive = TRUE)
  dir.create(file.path(html_outputs, "technical-guide"), recursive = TRUE)
  writeLines("user", file.path(html_outputs, "user-guide", "index.html"))
  writeLines("technical", file.path(html_outputs, "technical-guide", "index.html"))

  projects = lapply(
    c("01-user-guide", "02-technical-guide"),
    function(name) {
      path = file.path(root, name)
      dir.create(path)
      file.create(file.path(path, "_quarto-html.yml"))
      structure(
        list(
          name = name,
          path = normalizePath(path, winslash = "/"),
          type = "website",
          format_types = c(html = "website")
        ),
        class = c("iasi_quarto_project", "list")
      )
    }
  )

  testthat::local_mocked_bindings(
    .discover_publish_output = function(project, format) {
      slug = sub("^[0-9]+-", "", project$name)
      list(path = file.path(html_outputs, slug))
    },
    .check_publish_sources = function(project, outputs, publish_path) outputs,
    .package = "iasi.quarto"
  )

  result = .publish_multiproject(list(path = root, projects = projects))
  landing = readLines(file.path(root, "publish", "index.html"), warn = FALSE)

  expect_true(any(grepl('href="user-guide/"', landing)))
  expect_true(any(grepl('href="technical-guide/"', landing)))
  expect_true(file.exists(file.path(root, "publish", "user-guide", "index.html")))
  expect_true(file.exists(file.path(root, "publish", "technical-guide", "index.html")))
  expect_true(file.exists(file.path(root, "publish", ".publish")))
  expect_true(file.exists(file.path(root, "publish", ".nojekyll")))
  expect_true(all(vapply(result$projects, function(project) project$published, logical(1))))
})
