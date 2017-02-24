library(blockbuster)
context("Areafying unit_area from PDS data")



# INPUT to test -----------------------------------------------------------

x <- blockbuster_pds[c(1:500, 200000:200500), ]
y <- dplyr::select(x, -unit_area)


# TESTS -------------------------------------------------------------------

test_that("areafy works on blockbuster PDS data with unit_area removed", {
  expect_equal(ncol(x), ncol(areafy(y)))
  expect_equal(nrow(x), nrow(areafy(y)))
  expect_equal(x$unit_area, areafy(y)$unit_area)  #  robust, tests 1000 rows
})

test_that("areafy makes correct decisions regarding unit_area estimates", {
  expect_equal(areafy(y[1, ])$unit_area, y[1, ]$gifa)
  expect_equal(areafy(y[999, ])$unit_area, y[1, ]$gifa)
})

# Developing areafy2 as an easier to read version of areafy using case_when
# Could switch to this when CDC starts to arrive

test_that("areafy2 checks inputs", {
  expect_warning(areafy2(y[1, ], "CDC"))
})
