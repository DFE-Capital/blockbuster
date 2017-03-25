library(blockbuster)
context("repair - test internal function of blockbuster")

# INPUT to test -----------------------------------------------------------
x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472
                   | buildingid == 4487)

# TESTS -------------------------------------------------------------------
test_that("Zero investment results in no change", {
  expect_equal(repair(x, repair_monies = 0),
               x)
  expect_false(isTRUE(all.equal(repair(x, repair_monies = 2e6),
                                x)))  #  should not be equal due to repairs
})