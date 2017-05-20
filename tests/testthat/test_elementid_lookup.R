library(blockbuster)
context("elementid lookup - test new method")

# TESTS -------------------------------------------------------------------
test_that("Lookup can handle what used to fail due to duplicates", {
  expect_equal(det_what_tm(blockbuster_pds[96, ])@transitionMatrix["n", "n"],
               0.5)
})