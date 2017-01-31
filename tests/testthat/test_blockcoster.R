library(blockbuster)
library(tidyverse)
context("Looking up and estimating repair costs for a row in a blockbuster tibble.")



# INPUT to test -----------------------------------------------------------

x <- blockbuster_pds[1, ]


# TESTS -------------------------------------------------------------------

test_that("blockcoster_lookup fails to look up the correct values from blockbuster_pds_repair_costs", {
  expect_true(is.numeric(blockcoster_lookup(x)))
  expect_equal(blockcoster_lookup(dplyr::select(x, element, sub_element, const_type, grade)),
               0)
})