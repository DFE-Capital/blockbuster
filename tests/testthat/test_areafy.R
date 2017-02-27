library(blockbuster)
context("Areafying unit_area from PDS data")



# INPUT to test -----------------------------------------------------------

x <- blockbuster_pds[c(1:500, 200000:200500), ]
y <- dplyr::select(x, -unit_area)


# TESTS -------------------------------------------------------------------

test_that("areafy works on blockbuster PDS data with unit_area removed", {
  expect_equal(ncol(x), ncol(areafy2(y)))
  expect_equal(nrow(x), nrow(areafy2(y)))
  expect_equal(x$unit_area, areafy2(y)$unit_area)  #  robust, tests 1000 rows
})


# REMEMBER COMPOSITION ----------------------------------------------------
test_that("areafy makes correct decisions regarding unit_area estimates", {
  expect_equal(areafy2(y[1, ])$unit_area, y[1, ]$ground_gifa * y[1, ]$composition)  #  structure flat roof should be equal to ground gifa
  expect_equal(areafy2(y[999, ])$unit_area, y[999, ]$gifa * y[999, ]$composition)  #  Mechanical Service should be gifa
  expect_equal(areafy2(y[51, ])$unit_area, y[51, ]$windows_doors *
                 y[51, ]$composition)
})

# Developing areafy2 as an easier to read version of areafy using case_when
# Could switch to this when CDC starts to arrive

test_that("areafy2 checks inputs", {
  expect_warning(areafy2(y[1, ], "PDS"), regexp = NA)  #  If NA, asserts that there should be no output, messages, warnings, or errors.
  expect_error(areafy2(y[1, ], "CDC"), regexp = NULL)  #  If NULL, the default, asserts that there should be an output, a messsage, a warning, or an error
})
