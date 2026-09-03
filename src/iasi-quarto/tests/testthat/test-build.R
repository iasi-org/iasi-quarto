test_that("build returns NULL for a non-IASI directory", {
  root = .copy_test_fixture("build", "not-iasi")
  testthat::local_mocked_bindings(.render_profile = function(...) fail("render must not be called"), .package = "iasi.quarto")
  expect_null(build(path = root))
})

test_that("all expands to the friendly IASI formats", {
  expect_identical(.resolve_build_formats("all"), c("html", "pdf", "epub", "doc", "git"))
  expect_identical(.resolve_build_formats(NULL), c("html", "pdf", "epub", "doc", "git"))
})

test_that("explicit formats preserve names and remove literal duplicates", {
  expect_identical(.resolve_build_formats(c("odt", "doc", "odt", "typst", "gfm", "git", "gfm")), c("odt", "doc", "typst", "gfm", "git"))
})


test_that("typst is supported explicitly without changing all", {
  expect_identical(.resolve_build_formats("typst"), "typst")
  expect_false("typst" %in% .resolve_build_formats("all"))
})

test_that("unsupported formats fail before rendering", {
  expect_error(.resolve_build_formats(c("html", "docx")), 'Unsupported format: "docx".', fixed = TRUE)
})

test_that("project format resolution uses exact profiles", {
  root = tempfile("iasi-formats-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  file.create(file.path(root, c("_quarto-doc.yml", "_quarto-odt.yml", "_quarto-git.yml", "_quarto-gfm.yml")))

  project = list(name = "book", path = root, type = "book", format_types = .publication_format_types("book"))
  expect_identical(.resolve_project_build_formats(project, c("doc", "odt", "git", "gfm")), c("doc", "odt", "git", "gfm"))
})

test_that("project format resolution does not substitute missing aliases", {
  root = tempfile("iasi-formats-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  file.create(file.path(root, c("_quarto-odt.yml", "_quarto-gfm.yml")))

  project = list(name = "book", path = root, type = "book", format_types = .publication_format_types("book"))
  expect_identical(.resolve_project_build_formats(project, c("doc", "odt", "git", "gfm"), quiet = TRUE), c("odt", "gfm"))
})

test_that("explicit missing profiles are ignored with a warning", {
  root = tempfile("iasi-formats-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  project = list(name = "book", path = root, type = "book", format_types = .publication_format_types("book"))
  result = NULL
  expect_warning({ result = .resolve_project_build_formats(project, "odt") }, "Ignorando 'odt' porque falta '_quarto-odt.yml'.", fixed = TRUE)
  expect_identical(result, character())
})

test_that("all ignores missing profiles without warning", {
  root = tempfile("iasi-formats-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  file.create(file.path(root, "_quarto-html.yml"))

  project = list(name = "book", path = root, type = "book", format_types = .publication_format_types("book"))
  result = NULL
  expect_no_warning({ result = .resolve_project_build_formats(project, .resolve_build_formats("all"), quiet = TRUE) })
  expect_identical(result, "html")
})

test_that("build renders configured formats with their Quarto formats", {
  root = .copy_test_fixture("build", "single")
  calls = new.env(parent = emptyenv())
  calls$profiles = character()
  calls$to = character()

  testthat::local_mocked_bindings(
    .render_profile = function(path, profile, to, type) {
      calls$profiles = c(calls$profiles, profile)
      calls$to = c(calls$to, to)
      invisible(TRUE)
    },
    .package = "iasi.quarto"
  )

  plan = NULL
  expect_no_warning({ plan = build(path = root) })
  expect_identical(calls$profiles, c("html", "pdf"))
  expect_identical(calls$to, c("html", "pdf"))
  expect_identical(plan$formats, c("html", "pdf"))
  expect_true(plan$rendered)
})


test_that("build warns for an explicitly requested missing profile", {
  root = .copy_test_fixture("build", "single")
  testthat::local_mocked_bindings(.render_profile = function(...) fail("render must not be called"), .package = "iasi.quarto")
  plan = NULL
  expect_warning({ plan = build(path = root, format = "odt") }, "Ignorando 'odt' porque falta '_quarto-odt.yml'.", fixed = TRUE)
  expect_identical(plan$formats, character())
  expect_false(plan$rendered)
})

test_that("build accepts one selected format", {
  root = .copy_test_fixture("build", "single")
  calls = new.env(parent = emptyenv())
  calls$profiles = character()

  testthat::local_mocked_bindings(
    .render_profile = function(path, profile, to, type) {
      calls$profiles = c(calls$profiles, profile)
      invisible(TRUE)
    },
    .package = "iasi.quarto"
  )

  plan = build(path = root, format = "html")
  expect_identical(calls$profiles, "html")
  expect_identical(plan$formats, "html")
})


test_that("typst uses its own profile and Quarto format", {
  publication = list(path = tempdir(), type = "book", rendered = FALSE, profiles = character())
  calls = new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    .render_profile = function(path, profile, to, type) {
      calls$profile = profile
      calls$to = to
      invisible(TRUE)
    },
    .package = "iasi.quarto"
  )

  result = .render_typst(publication)
  expect_identical(calls$profile, "typst")
  expect_identical(calls$to, "typst")
  expect_identical(result$profiles, "typst")
})

test_that("doc delegates to ODT, keeps profile doc, and renames only its output", {
  root = tempfile("iasi-doc-")
  dir.create(file.path(root, "_outputs", "odt"), recursive = TRUE)
  writeLines("existing", file.path(root, "_outputs", "odt", "existing.txt"))
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  publication = list(path = root, type = "book", rendered = FALSE, profiles = character())

  testthat::local_mocked_bindings(
    .render_profile = function(path, profile, to, type) {
      expect_identical(profile, "doc")
      expect_identical(to, "odt")
      dir.create(file.path(path, "_outputs", "odt"), recursive = TRUE)
      writeLines("doc", file.path(path, "_outputs", "odt", "document.odt"))
      invisible(TRUE)
    },
    .package = "iasi.quarto"
  )

  result = .render_doc(publication)
  expect_true(file.exists(file.path(root, "_outputs", "doc", "document.odt")))
  expect_true(file.exists(file.path(root, "_outputs", "odt", "existing.txt")))
  expect_identical(result$profiles, "doc")
})

test_that("git delegates to GFM and keeps profile git", {
  root = tempfile("iasi-git-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  publication = list(path = root, type = "book", rendered = FALSE, profiles = character())

  testthat::local_mocked_bindings(
    .render_profile = function(path, profile, to, type) {
      expect_identical(profile, "git")
      expect_identical(to, "gfm")
      dir.create(file.path(path, "_outputs", "gfm"), recursive = TRUE)
      writeLines("# Git", file.path(path, "_outputs", "gfm", "index.md"))
      invisible(TRUE)
    },
    .package = "iasi.quarto"
  )

  result = .render_git(publication)
  expect_true(file.exists(file.path(root, "_outputs", "git", "index.md")))
  expect_identical(result$profiles, "git")
})

test_that("build selects a book by name without its numeric prefix", {
  root = .copy_test_fixture("build", "multi")
  testthat::local_mocked_bindings(.render_profile = function(...) invisible(TRUE), .package = "iasi.quarto")
  plan = build(path = root, book = "user-guide", format = "html")
  expect_length(plan$projects, 1L)
  expect_identical(plan$projects[[1L]]$name, "01-user-guide")
})

test_that("build ignores book selection for a current publication", {
  root = .copy_test_fixture("build", "single")
  testthat::local_mocked_bindings(.render_profile = function(...) invisible(TRUE), .package = "iasi.quarto")
  plan = NULL
  expect_warning({ plan = build(path = root, book = "something-else", format = "html") }, "The `book` selection will be ignored.", fixed = TRUE)
  expect_length(plan$projects, 1L)
})

test_that("build stops before rendering an invalid publication", {
  root = .copy_test_fixture("build", "invalid")
  testthat::local_mocked_bindings(.render_profile = function(...) fail("render must not be called"), .package = "iasi.quarto")
  expect_error(build(path = root, format = "html"), "IASI Quarto check failed:", fixed = TRUE)
})
