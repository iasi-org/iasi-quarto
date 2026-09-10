test_that("deploy returns NULL for a non-IASI directory", {
  root = tempfile("iasi-deploy-none-")
  dir.create(root)

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  expect_null(
    deploy(
      path = root
    )
  )
})

test_that("publish freshness compares build and destination fingerprints", {
  root = tempfile("iasi-deploy-state-")
  output = file.path(
    root,
    "_outputs",
    "html"
  )
  destination = file.path(
    root,
    "_publish"
  )

  dir.create(
    output,
    recursive = TRUE
  )
  dir.create(
    destination,
    recursive = TRUE
  )

  writeLines(
    "first",
    file.path(
      output,
      "index.html"
    )
  )

  writeLines(
    "first",
    file.path(
      destination,
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

  project = list(
    name = "book",
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    type = "book",
    html_landing_page = FALSE,
    quarto = list(
      profile = list(
        group = list(
          "html"
        )
      )
    ),
    published = TRUE
  )

  .record_publish_state(
    project = project,
    destination = destination
  )

  expect_false(
    .publish_required(
      project = project,
      destination = destination
    )
  )

  writeLines(
    "changed",
    file.path(
      output,
      "index.html"
    )
  )

  expect_true(
    .publish_required(
      project = project,
      destination = destination
    )
  )
})
