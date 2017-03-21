library(blockbuster)
context("rebuild - test internal function of blockbuster")

# INPUT to test -----------------------------------------------------------
x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472
                   | buildingid == 4487)

y <- blockbuster(x, forecast_horizon = 3, rebuild_monies = c(0, 2440983, 2440984))

z <- blockbuster_pds[
  !duplicated(blockbuster_pds[, c("elementid")]),
  ] 

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

test_that("Rebuilding reduces cost to zero", {
    expect_equal(nrow(dplyr::filter(y[[3]], grade == "N" | grade == "A")),
               nrow(dplyr::filter(y[[3]], cost == 0)) - 8)  #  other reasons cost are zero? 
  #  pds_repair costs suggests quite a few buildign components have zero cost at some b, c and d grades
  #  typically these are sensible, such as exposed structure or unpainted ceiling
  #  thus an unpainted wall is cheaper than a painted wall to repair, which is silly!
  #  See for your self by running this:
  #  View(dplyr::filter(y[[3]], cost == 0 & grade != "N" & grade != "A")[,
  #  c("element", "sub_element", "const_type", "grade", "cost")])
})

test_that("Check areafy works for all building components in rebuild", {
  expect_equal(nrow(blockbuster(blockbuster_tibble = z,
                           forecast_horizon = 1, rebuild_monies = 5e6)[[2]]),
               269)
})