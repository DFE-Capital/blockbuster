library(blockbuster)
context("repair - test internal function of blockbuster")

# INPUT to test -----------------------------------------------------------
x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472
                   | buildingid == 4487)
y <- repair(x, 1e6)  #  £333k each block should repair all

# TESTS -------------------------------------------------------------------
test_that("Zero investment results in no change", {
  expect_equal(repair(x, repair_monies = 0),  #  should be equal as no repairs done
               x)
  expect_false(isTRUE(all.equal(repair(x, repair_monies = 2e6),
                                x)))  #  should not be equal due to repairs
})

test_that("what_needs_repair_within_block skips zero cost and repairs what it can afford.", {
  expect_equal(what_needs_repair_within_block(
    tibble::tibble(cost = seq(from = 0, to = 4500, by = 500)),
    1e4)$repair_status,
    c(0, rep(1, 5), rep(0, 4)))  #  we predict what output vector should look like if correct
})

test_that("repair_status added and is zero for zero costs and A grade", {
  expect_equal(ncol(x) + 1, ncol(y))
  expect_equal(sum(y$cost == 0), sum(y$repair_status == 0))
})

