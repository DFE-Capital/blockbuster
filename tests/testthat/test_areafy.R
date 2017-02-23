library(blockbuster)
context("Areafying unit_area from PDS data")



# INPUT to test -----------------------------------------------------------

x <- blockbuster_pds[c(1:50, 200000:200050), ]
y <- dplyr::select(x, -unit_area)


# TESTS -------------------------------------------------------------------

test_that("areafy works on blockbuster PDS data with unit_area removed", {
  expect_equal(ncol(x), ncol(areafy(y)))
  expect_equal(nrow(x), nrow(areafy(y)))
  expect_equal(x$unit_area, areafy(y)$unit_area)  #  should be 888 not 1916
})


