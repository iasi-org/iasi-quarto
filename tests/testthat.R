library(testthat)
library(iasi.quarto)

testthat::skip_if_not_installed("yaml")

test_check("iasi.quarto")
