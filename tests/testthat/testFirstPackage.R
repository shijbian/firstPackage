library(firstPackage)
library(testthat)

test_that("make_filename can make up the name", {
  expect_equal(make_filename(20), "accident_20.csv")
})
