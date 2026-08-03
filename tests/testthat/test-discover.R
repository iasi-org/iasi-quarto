test_that("A minimal book is discovered correctly", {

  root = test_path(
    "fixtures",
    "book-minimal",
    "chapters"
  )

  project = iasi.quarto:::.discover_project(root)

  expect_equal(project$root, root)

  expect_length(project$chapters, 1)

  expect_equal(
    project$chapters[[1]]$name,
    "01-intro"
  )

  expect_equal(
    basename(project$chapters[[1]]$documents),
    c(
      "hello.qmd",
      "index.qmd"
    )
  )

})

test_that("A parts book is discovered correctly", {

  root = test_path(
    "fixtures",
    "book-parts",
    "chapters"
  )

  project = iasi.quarto:::.discover_project(root)


print(
  vapply(
    project$chapters,
    function(chapter) chapter$name,
    character(1)
  )
)
  
  expect_length(project$chapters, 2)

  expect_equal(project$chapters[[1]]$name,"01-origin")
  expect_equal(project$chapters[[2]]$name,"02-architecture")

  expect_equal(
    basename(project$chapters[[1]]$documents),
    c(
      "01-problem.qmd",
      "02-evolution.qmd",       
      "index.qmd"       
    )
  )

})