library(blockbuster)
context("blockbuster rebuild costing")

# INPUT to test -----------------------------------------------------------
x <- dplyr::filter(blockbuster_pds, buildingid == 127617)
two_year_later <- blockbuster(x, 2)  #  default rebuild rate cost
two_year_later_inflation <- blockbuster(x, 2, rebuild_cost_rate = c(1274, 1286.74))


# TESTS -------------------------------------------------------------------

test_that("default rebuild rate is £1274 per m^2", {
  expect_equal(two_year_later[[3]]$gifa * 1274, 
               two_year_later[[3]]$block_rebuild_cost)
  expect_equal(two_year_later_inflation[[3]]$gifa * 1286.74,
               two_year_later_inflation[[3]]$block_rebuild_cost)
})

