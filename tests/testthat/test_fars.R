library(testthat)
library(farsLinh)

test_that("make_filename works correctly", {
  expect_equal(make_filename(2021), "accident_2021.csv.bz2")
})
