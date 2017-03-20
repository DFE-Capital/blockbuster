library(blockbuster)
context("rebuild - test internal function of blockbuster")

# INPUT to test -----------------------------------------------------------
x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472
                   | buildingid == 4487)
y <- blockbuster(x, forecast_horizon = 3, rebuild_monies = c(0, 2440983, 2440984))

z <- blockbuster_pds[1:100, ]

# TESTS -------------------------------------------------------------------
test_that("Zero investment in rebuild is default and results in no change", {
  expect_equal(rebuild(x, rebuild_monies = 0),
               x)
  expect_equal(length(y[[2]]$grade == "N"),
               270)
})

test_that("Rebuilding produces grade N and aggregates", {
  expect_equal(length(y[[3]]$grade == "N"),
               231)
  expect_equal(length(y[[4]]$grade == "N"),
               222)
})

test_that("Check areafy works for all building components in rebuild", {
  expect_equal(blockbuster(z, 1, rebuild_monies = 1),
               1337
  )
})