library(blockbuster)
context("Areafying unit_area from PDS data")



# INPUT to test -----------------------------------------------------------

x <- blockbuster_pds[c(1:50, 200000:200050), ]
x <- dplyr::select(x, -unit_area)


# TESTS -------------------------------------------------------------------

test_that("areafy works on blockbuster PDS data with unit_area removed", {
  expect_equal(ncol(x) + 1, ncol(areafy(x)))
  expect_equal(nrow(x), nrow(areafy(x)))
})
