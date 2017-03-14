library(blockbuster)
context("rebuild - test internal function of blockbuster")

# INPUT to test -----------------------------------------------------------
x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472
                   | buildingid == 4487)

# TESTS -------------------------------------------------------------------
test_that("Zero investment in rebuild results in no change", {
  expect_equal(rebuild(x, rebuild_monies = 0),
               x)
})

test_that("rebuilding produces all grade N", {
  expect_equal(sum(rebuild(x, rebuild_monies = 5e6)$grade == "N",
                   block_rebuild_cost = 1274),
               nrow(x))
})