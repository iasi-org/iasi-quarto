test_that("website defaults to the web profile", {
  project = list(
    type = "website",
    quarto = list()
  )

  expect_identical(
    .project_declared_formats(project),
    "web"
  )
})

test_that("profile.default is used when no group is declared", {
  project = list(
    type = "website",
    quarto = list(
      profile = list(
        default = "web"
      )
    )
  )

  expect_identical(
    .project_declared_formats(project),
    "web"
  )
})

test_that("missing profile YAML warns but does not suppress the profile", {
  root = tempfile("iasi-web-profile-")
  dir.create(root)

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  project = list(
    name = "website",
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    type = "website",
    quarto = list(
      profile = list(
        default = "web"
      )
    )
  )

  resolved = NULL

  expect_warning(
    {
      resolved = .resolve_project_build_formats(
        project,
        "all"
      )
    },
    "No existe '_quarto-web.yml'",
    fixed = TRUE
  )

  expect_identical(
    resolved,
    "web"
  )
})

test_that("website output defaults to _outputs/web", {
  root = tempfile("iasi-web-output-")
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
    type = "website",
    quarto = list(
      project = list(
        type = "website"
      )
    ),
    html_landing_page = FALSE
  )

  config = .profile_config(
    project,
    "web"
  )

  expect_identical(
    config$output_dir,
    file.path(
      "_outputs",
      "web"
    )
  )
})

test_that("_quarto-web.yml overrides the website output directory", {
  root = tempfile("iasi-web-output-")
  dir.create(root)

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  writeLines(
    c(
      "project:",
      "  output-dir: _outputs/html"
    ),
    file.path(
      root,
      "_quarto-web.yml"
    )
  )

  project = list(
    path = normalizePath(
      root,
      winslash = "/",
      mustWork = TRUE
    ),
    type = "website",
    quarto = list(
      project = list(
        type = "website"
      )
    ),
    html_landing_page = FALSE
  )

  config = .profile_config(
    project,
    "web"
  )

  expect_identical(
    config$output_dir,
    file.path(
      "_outputs",
      "html"
    )
  )
})

test_that("publish root profile priority is web, html, single", {
  root = tempfile("iasi-publish-primary-")
  dir.create(root)

  withr::defer(
    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  )

  for (profile in c("web", "html", "single")) {
    path = file.path(
      root,
      profile
    )

    dir.create(path)

    writeLines(
      profile,
      file.path(
        path,
        paste0(
          profile,
          ".txt"
        )
      )
    )
  }

  selected = .move_publish_primary_to_root(root)

  expect_identical(
    selected,
    "web"
  )

  expect_true(
    file.exists(
      file.path(
        root,
        "web.txt"
      )
    )
  )

  expect_false(
    dir.exists(
      file.path(
        root,
        "web"
      )
    )
  )

  expect_true(
    dir.exists(
      file.path(
        root,
        "html"
      )
    )
  )

  expect_true(
    dir.exists(
      file.path(
        root,
        "single"
      )
    )
  )
})

test_that("publish falls back from web to html and then single", {
  for (profile in c("html", "single")) {
    root = tempfile("iasi-publish-primary-")
    dir.create(root)

    path = file.path(
      root,
      profile
    )

    dir.create(path)
    writeLines(
      profile,
      file.path(
        path,
        paste0(
          profile,
          ".txt"
        )
      )
    )

    selected = .move_publish_primary_to_root(root)

    expect_identical(
      selected,
      profile
    )

    expect_true(
      file.exists(
        file.path(
          root,
          paste0(
            profile,
            ".txt"
          )
        )
      )
    )

    unlink(
      root,
      recursive = TRUE,
      force = TRUE
    )
  }
})
