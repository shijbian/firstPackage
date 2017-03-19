library(firstPackage)
library(testthat)
setwd("~/Dropbox/Coursera/R Dev/R Package/Week4_Build_Own_Package/firstPackage/tests/testthat")

test_that("fars_read is able to read the file", {
   expect_equal(dim(fars_read("accident_2014.csv"))[1], 30056)
 })

test_that("make_filename can make up the name", {
  expect_equal(make_filename(20), "accident_20.csv")
})

test_that("fars_read_years can extract the two columns", {
  expect_equal(dim(fars_read_years(2014)[[1]])[2], 2)
})

