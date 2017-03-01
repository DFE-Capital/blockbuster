library(blockbuster)

context("Looking up and estimating repair costs for a row in a blockbuster tibble.")



# INPUT to test -----------------------------------------------------------

x <- dplyr::select(blockbuster_pds[1, ], element, sub_element, const_type, grade)
z_test <- blockbuster(blockbuster_pds[1:10, ], 1)

# TEST NEW DESIGN different input ---------------------------------------------------------
test_that("blockcoster_lookup no longer receives tibble, takes in the four variables it needs only.", {
  expect_true(is.numeric(blockcoster_lookup(blockbuster_pds[3, ]$element, blockbuster_pds[3, ]$sub_element,
                                  blockbuster_pds[3, ]$const_type, blockbuster_pds[3, ]$grade)))
  expect_true(is.numeric(blockcoster_lookup(x$element, x$sub_element, x$const_type, x$grade)))
})

blockcoster_lookup(blockbuster_pds[3, ]$element, blockbuster_pds[3, ]$sub_element, blockbuster_pds[3, ]$const_type, blockbuster_pds[3, ]$grade)

# TEST USE OF THIS FUNCTION IN BLOCKBUSTER --------------------------------
test_that("blockbuster_lookup is not implemented in blockbuster function as expected", {
  expect_equal(z_test[[1]][[3, "cost"]], 3910.23)  #  check the initial costs remain the same for year zero
  expect_equal(round(z_test[[2]][[3, "cost"]]), 4117)  #  check after deterioration of one year costs are produced
})


# when grade == "E" -------------------------------------------------------
# perhaps we just accrue E, to sort it you need rebuild, maintenance can't fix therefore no sense costing it
# for now
test_that("blockbuster_lookup returns NA when grade is E", {
  expect_true(is.na(blockcoster_lookup(element = "Roofs", sub_element = "Coverings and insulation",
                                  const_type = "Flat roof - Flexible sheet; single ply or built up",
                                  grade = "E")))
})

