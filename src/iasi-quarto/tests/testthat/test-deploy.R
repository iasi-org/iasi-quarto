test_that("deploy builds before publishing", {
   calls = new.env(parent = emptyenv())
   calls$steps = character()
   
   testthat::local_mocked_bindings(
      build = function(...) {
         calls$steps = c(
            calls$steps,
            "build"
         )
         
         list(rendered = TRUE)
      },
      publish = function(...) {
         calls$steps = c(
            calls$steps,
            "publish"
         )
         
         list(published = TRUE)
      },
      .package = "iasi.quarto"
   )
   
   result = deploy()
   
   expect_identical(
      calls$steps,
      c(
         "build",
         "publish"
      )
   )
   
   expect_true(result$published)
})


test_that("deploy does not publish when build fails", {
   calls = new.env(parent = emptyenv())
   calls$published = FALSE
   
   testthat::local_mocked_bindings(
      build = function(...) {
         stop(
            "build failed",
            call. = FALSE
         )
      },
      publish = function(...) {
         calls$published = TRUE
         list(published = TRUE)
      },
      .package = "iasi.quarto"
   )
   
   expect_error(
      deploy(),
      "build failed",
      fixed = TRUE
   )
   
   expect_false(calls$published)
})


test_that("deploy does not publish when build finds no workspace", {
   calls = new.env(parent = emptyenv())
   calls$published = FALSE
   
   testthat::local_mocked_bindings(
      build = function(...) {
         NULL
      },
      publish = function(...) {
         calls$published = TRUE
         list(published = TRUE)
      },
      .package = "iasi.quarto"
   )
   
   result = deploy()
   
   expect_null(result)
   expect_false(calls$published)
})
