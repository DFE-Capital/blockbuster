library(blockbuster)
context("informative attributes detailing inputs are provided in output")

y <- blockbuster(blockbuster_pds[1, ], 1, rebuild_monies = 0,
                repair_monies = 255)

test_that("Custom class", {
  expect_equal(attributes(y)$class, c("blockbuster_list", "list"))
})

test_that("Monies attributes are available", {
  expect_equal(attributes(y)$rebuild_cost_rate, 1274)
  expect_equal(attributes(y)$rebuild_monies, 0)
  expect_equal(attributes(y)$repair_monies, 255)
})