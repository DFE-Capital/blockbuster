library(blockbuster)
context("repair - test internal function of blockbuster")

# INPUT to test -----------------------------------------------------------
x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472
                   | buildingid == 4487)
y <- repair(x, 1e6)  #  £333k each block should repair all
z <- blockbuster(x[1:5, ], 1)[[2]]  #  smaller for QA that unit_area is behaving as expected
z_new <- blockbuster(x, 1, rebuild_monies = 10e6)[[2]]  #  all 3 blocks rebuilt
e <- blockbuster(x[6, ], 3)[[3]]

# TESTS -------------------------------------------------------------------
test_that("Zero investment results in no change", {
  expect_equal(repair(x, repair_monies = 0),  #  should be equal as no repairs done
               x)
  expect_false(isTRUE(all.equal(repair(x, repair_monies = 2e6),
                                x)))  #  should not be equal due to repairs
})

test_that("what_needs_repair_within_block skips zero cost (and E grade) and repairs what it can afford.", {
  expect_equal(what_needs_repair_within_block(
    tibble::tibble(cost = seq(from = 0, to = 4500, by = 500),
                   grade = c("A", "B", "C", "D", "E", "B", "C", "N", "D", "B")),
    1e4)$repair_status,
    c(0, rep(1, 3), 0, 1, 1, rep(0, 3)))  #  we predict what output vector should look like if correct
  expect_equal(as.character(repair(e, 1050)$grade), c("A", "C", "E"))  # shouldn't repair E
})

test_that("repair aggregates the rows correctly on repair", {
  expect_equal(nrow(repair(z, 20e3)), 5)  #  one year deterioration followed by full repairs, all A
  expect_equal(as.character(repair(z, 20e3)$grade),
               rep("A", 5))
  expect_equal(sum(repair(z, 20e3)$unit_area), sum(z$unit_area))
})

test_that("repair ignores N and E, does not repair", {
  expect_equal(z_new, repair(z_new, repair_monies = 1e6))
})
