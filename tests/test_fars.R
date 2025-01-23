library(testthat)
library(fars)

test_that("make_filename works correctly", {
  expect_equal(make_filename(2021), "accident_2021.csv.bz2")
})
