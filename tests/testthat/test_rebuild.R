library(blockbuster)
context("rebuild - test internal function of blockbuster")

# INPUT to test -----------------------------------------------------------
x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472)

# TESTS -------------------------------------------------------------------

test_that("rebuilding produces all grade N", {
  expect_equal(sum(rebuild(x)$grade == "N"), nrow(x))
})