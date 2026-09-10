test_that("build selection defaults to all", {
  expect_identical(
    .resolve_build_formats(NULL),
    "all"
  )

  expect_identical(
    .resolve_build_formats("all"),
    "all"
  )
})

test_that("explicit build formats preserve order and remove duplicates", {
  expect_identical(
    .resolve_build_formats(
      c(
        "pdf",
        "html",
        "pdf"
      )
    ),
    c(
      "pdf",
      "html"
    )
  )
})

test_that("project format resolution follows declared profiles", {
  root = tempfile("iasi-formats-")
  dir.create(root)

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  file.create(
    file.path(
      root,
      c(
        "_quarto-html.yml",
        "_quarto-pdf.yml"
      )
    )
  )

  project = list(
    name = "book",
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    quarto = list(
      profile = list(
        group = list(
          c(
            "html",
            "pdf"
          )
        )
      )
    )
  )

  expect_identical(
    .resolve_project_build_formats(
      project,
      "all"
    ),
    c(
      "html",
      "pdf"
    )
  )
})

test_that("explicit undeclared formats are ignored with a warning", {
  root = tempfile("iasi-formats-")
  dir.create(root)

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  file.create(
    file.path(
      root,
      "_quarto-html.yml"
    )
  )

  project = list(
    name = "book",
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    quarto = list(
      profile = list(
        group = list(
          "html"
        )
      )
    )
  )

  result = NULL

  expect_warning(
    {
      result = .resolve_project_build_formats(
        project,
        "pdf"
      )
    },
    "no está declarado",
    fixed = TRUE
  )

  expect_identical(
    result,
    character()
  )
})

test_that("book selection accepts a name without its numeric prefix", {
  root = tempfile("iasi-books-")
  first = file.path(
    root,
    "docs",
    "01-user-guide"
  )
  second = file.path(
    root,
    "docs",
    "02-reference"
  )

  dir.create(
    first,
    recursive = TRUE
  )
  dir.create(
    second,
    recursive = TRUE
  )

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  plan = list(
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    current = FALSE,
    books = sort(normalizePath(
      c(
        first,
        second
      ),
      winslash = "/",
      mustWork = TRUE
    ))
  )

  selected = .select_build_books(
    plan,
    "user-guide"
  )

  expect_identical(
    basename(selected$books),
    "01-user-guide"
  )
})

test_that("build returns NULL for a non-IASI directory", {
  root = tempfile("iasi-not-workspace-")
  dir.create(root)

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  expect_null(
    build(
      path = root
    )
  )
})

test_that("build removes the configured release tree from rendered output", {
  root = tempfile("iasi-build-release-")
  output = file.path(
    root,
    "_outputs",
    "html"
  )

  dir.create(
    file.path(
      output,
      "release",
      "LICENSES"
    ),
    recursive = TRUE
  )

  writeLines(
    "home",
    file.path(
      output,
      "index.html"
    )
  )

  writeLines(
    "license",
    file.path(
      output,
      "release",
      "LICENSES",
      "license.txt"
    )
  )

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
    iasi = list(
      paths = list(
        release = "release"
      )
    )
  )

  .remove_build_output_exclusions(
    project = project,
    output_path = output
  )

  expect_true(
    file.exists(
      file.path(
        output,
        "index.html"
      )
    )
  )

  expect_false(
    dir.exists(
      file.path(
        output,
        "release"
      )
    )
  )
})


test_that("build does not prune a release path outside the project", {
  root = tempfile("iasi-build-release-root-")
  outside = tempfile("iasi-build-release-outside-")
  output = file.path(
    root,
    "_outputs",
    "html"
  )

  dir.create(
    file.path(
      output,
      "release"
    ),
    recursive = TRUE
  )

  dir.create(
    outside,
    recursive = TRUE
  )

  withr::defer({
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )

    unlink(
      outside,
      recursive = TRUE,
      force = TRUE
    )
  })

  project = list(
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    iasi = list(
      paths = list(
        release = normalizePath(
          outside,
          winslash = "/",
          mustWork = TRUE
        )
      )
    )
  )

  .remove_build_output_exclusions(
    project = project,
    output_path = output
  )

  expect_true(
    dir.exists(
      file.path(
        output,
        "release"
      )
    )
  )
})
