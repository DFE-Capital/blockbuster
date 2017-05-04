library(blockbuster)
context("Generic methods for blockbuster_list class object")

# INPUT to test -----------------------------------------------------------
x <- blockbuster(blockbuster_pds[1:10, ], 2)

# TESTS -------------------------------------------------------------------
test_that("blockbuster_list output from blockbuster", {
  expect_equal(class(x),  
               c("blockbuster_list", "list"))
  
})

test_that("plot blockbuster_list works", {
  p <- plot(x)
  expect_equal(max(p$data$timestep), 2)
  
})

test_that("boxplot blockbuster_list works", {
  p <- boxplot(x)
  expect_equal(max(p$data$"timestep"), 2)
  
})

test_that("summary blockbuster_list works", {
  p <- summary(x)
  expect_equal(nrow(p[[5]]), 5)
  
})