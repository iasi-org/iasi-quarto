test_that("current publication publishes directly below _publish", {
  root = tempfile("iasi-publish-current-")
  dir.create(root)
  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  project = list(
    name = "iasi-book-I",
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    )
  )

  plan = list(
    path = project$path,
    current = TRUE,
    projects = list(project)
  )

  expect_identical(
    .publish_destination(
      plan,
      project
    ),
    file.path(
      plan$path,
      "_publish"
    )
  )
})

test_that("multiproject publish uses one root and removes numeric prefixes", {
  root = tempfile("iasi-publish-multi-")
  project_path = file.path(
    root,
    "docs",
    "01-user-guide"
  )

  dir.create(
    project_path,
    recursive = TRUE
  )

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  project = list(
    name = "01-user-guide",
    path = normalizePath(
      project_path,
      winslash = "/",
      mustWork = TRUE
    )
  )

  plan = list(
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    current = FALSE,
    projects = list(project)
  )

  expect_identical(
    .publish_destination(
      plan,
      project
    ),
    file.path(
      plan$path,
      "_publish",
      "user-guide"
    )
  )
})

test_that("publish destination collisions fail before writing", {
  root = tempfile("iasi-publish-collision-")
  first = file.path(
    root,
    "docs-a",
    "01-guide"
  )
  second = file.path(
    root,
    "docs-b",
    "02-guide"
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

  projects = list(
    list(
      name = "01-guide",
      path = normalizePath(
        first,
        winslash = "/",
        mustWork = TRUE
      )
    ),
    list(
      name = "02-guide",
      path = normalizePath(
        second,
        winslash = "/",
        mustWork = TRUE
      )
    )
  )

  plan = list(
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    current = FALSE,
    projects = projects
  )

  expect_error(
    .publish_destinations(plan),
    "same publish destination",
    fixed = TRUE
  )
})

test_that("publish ignores empty format directories", {
  root = tempfile("iasi-publish-formats-")
  dir.create(
    file.path(
      root,
      "html"
    ),
    recursive = TRUE
  )
  dir.create(
    file.path(
      root,
      "pdf"
    ),
    recursive = TRUE
  )

  writeLines(
    "pdf",
    file.path(
      root,
      "pdf",
      "book.pdf"
    )
  )

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  expect_identical(
    .publish_format_directories(root),
    "pdf"
  )
})

test_that("publish materialises HTML at root and preserves other formats", {
  root = tempfile("iasi-publish-tree-")
  source = file.path(
    root,
    "_outputs"
  )
  destination = file.path(
    root,
    "_publish"
  )

  dir.create(
    file.path(
      source,
      "html"
    ),
    recursive = TRUE
  )
  dir.create(
    file.path(
      source,
      "pdf"
    ),
    recursive = TRUE
  )

  writeLines(
    c(
      "<html>",
      "<body>",
      '<span id="iasi-publish-date"></span>',
      "</body>",
      "</html>"
    ),
    file.path(
      source,
      "html",
      "index.html"
    )
  )

  writeLines(
    "pdf",
    file.path(
      source,
      "pdf",
      "book.pdf"
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
    name = "book",
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    strategy = "none",
    version = "1.0.0"
  )

  result = .publish_project_to(
    project = project,
    source = "_outputs",
    destination = destination
  )

  expect_true(
    file.exists(
      file.path(
        destination,
        "index.html"
      )
    )
  )

  expect_false(
    dir.exists(
      file.path(
        destination,
        "html"
      )
    )
  )

  expect_true(
    file.exists(
      file.path(
        destination,
        "pdf",
        "book.pdf"
      )
    )
  )

  expect_identical(
    sort(result$publish_outputs),
    c(
      "html",
      "pdf"
    )
  )

  expect_true(result$published)
})

test_that("publish rewrites export links after HTML is moved to root", {
  root = tempfile("iasi-publish-export-")
  source = file.path(
    root,
    "_outputs"
  )
  destination = file.path(
    root,
    "_publish"
  )

  dir.create(
    file.path(
      source,
      "html"
    ),
    recursive = TRUE
  )
  dir.create(
    file.path(
      source,
      "pdf"
    ),
    recursive = TRUE
  )

  writeLines(
    c(
      "<nav>",
      '<div class="iasi-export"></div>',
      "</nav>"
    ),
    file.path(
      source,
      "html",
      "index.html"
    )
  )

  writeLines(
    "pdf",
    file.path(
      source,
      "pdf",
      "book.pdf"
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
    name = "book",
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    strategy = "none"
  )

  .publish_project_to(
    project = project,
    source = "_outputs",
    destination = destination
  )

  html = paste(
    readLines(
      file.path(
        destination,
        "index.html"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    html,
    'href="pdf/book.pdf"',
    fixed = TRUE
  )

  expect_false(
    grepl(
      "IASI_EXPORT",
      html,
      fixed = TRUE
    )
  )
})

test_that("publish rejects source and destination overlap", {
  root = tempfile("iasi-publish-overlap-")
  source = file.path(
    root,
    "_outputs"
  )

  dir.create(
    file.path(
      source,
      "html"
    ),
    recursive = TRUE
  )

  writeLines(
    "html",
    file.path(
      source,
      "html",
      "index.html"
    )
  )

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  expect_error(
    .check_publish_tree(
      source = source,
      destination = file.path(
        source,
        "_publish"
      ),
      project = "book"
    ),
    "overlaps destination",
    fixed = TRUE
  )
})
