library(blockbuster)
context("blockbuster_sim permits blockbuster to scale by writing to disc")


# INPUT -------------------------------------------------------------------

x <- blockbuster_pds[1, ]

# TESTS -------------------------------------------------------------------

test_that("Files are produced in output dir", {
  expect_equal(0,
               length(list.files("./output")))
})

test_that("Produces same output as blockbuster", {
  expect_equal(blockbuster_sim(x, 1),
               blockbuster(x, 1))
})