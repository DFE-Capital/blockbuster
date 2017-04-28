library(blockbuster)
context("qa - test combination of repair and rebuild by hand for expected behaviour")

# INPUT to test -----------------------------------------------------------
x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472
                   | buildingid == 4487)
y <- x[c(10, 60, 135), ]
z <- blockbuster(y, forecast_horizon = 4,
                 rebuild_monies = c(0, 2440984, 0, 0),
                 repair_monies = c(0, 0, 0, 500))

# TESTS -------------------------------------------------------------------
test_that("QA rebuild one block at year 2", {
  expect_equal(sum(z[[3]]$grade == "N"), 1)
  expect_equal(sum(z[[4]]$grade == "N"), 1)
  expect_equal(length(z[[4]]$grade), 10)
})

#  Problem with costing New build? Should be zero cost, at the moment is NA
#  This was causing that error in the block costing as NA was not being removed.
#  Blockcoster needs to cost N or A as zero if grade is correct
test_that("QA repair consideration", {
  expect_equal(round(sum(z[[3]]$cost), 0), 1)
  expect_equal(blockcoster_lookup(z[[4]][9,]$concated,
                                  grade = z[[4]][9,]$grade),
               0
               )
})