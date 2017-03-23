library(firstPackage)
library(testthat)

test_that("fars_read is able to read the file", {
   expect_equal(dim(fars_read("./inst/extData/accident_2014.csv"))[1], 30056)
 })

test_that("make_filename can make up the name", {
  expect_equal(make_filename(20), "accident_20.csv")
})
