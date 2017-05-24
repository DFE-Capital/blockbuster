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

test_that("QA blockcoster considerations", {
  expect_equal(sum(round(
    blockcoster_lookup(z[[4]]$elementid, z[[4]]$grade)*z[[4]]$unit_area, 0)),
               259) #  hand checked lookup and multiplications correct
  expect_equal(blockcoster_lookup(z[[4]][9,]$elementid,
                                  z[[4]][9,]$grade),
               0
               )  #  N should give zero cost
})

