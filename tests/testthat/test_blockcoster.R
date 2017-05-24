library(blockbuster)

context("Looking up and estimating repair costs for a row in a blockbuster tibble.")



# INPUT to test -----------------------------------------------------------

x <- dplyr::select(blockbuster_pds[1, ], elementid, grade)
z_test <- blockbuster(blockbuster_pds[1:10, ], 2)

# TEST NEW DESIGN different input ---------------------------------------------------------
test_that("blockcoster_lookup no longer receives tibble, receives the cached concated and grade.", {
  expect_true(is.numeric(blockcoster_lookup(x$elementid, x$grade)))
})

#  blockcoster_lookup(blockbuster_pds[3, ]$element, blockbuster_pds[3, ]$sub_element, blockbuster_pds[3, ]$const_type, blockbuster_pds[3, ]$grade)

# TEST USE OF THIS FUNCTION IN BLOCKBUSTER --------------------------------
test_that("blockbuster_lookup is not implemented in blockbuster function as expected", {
  expect_equal(z_test[[1]][[3, "cost"]], 3910.23)  #  check the initial costs remain the same for year zero
  expect_equal(round(sum(z_test[[2]]$cost)), 56121)  #  check after deterioration of one year costs are produced
  #  not the best test, this seemed to change position with cache variable rework
  #  changed to sum of the vector, rather than position dependence
})


# when grade == "E" -------------------------------------------------------
# perhaps we just accrue E, to sort it you need rebuild, maintenance can't fix therefore no sense costing it
# for now, However
# Users requested a costing of E in the same variable
# Adam Bray suggested same cost as D plus 5% for extras
test_that("blockbuster_lookup vectorised and E is 105% of D", {
  expect_equal(blockcoster_lookup(z_test[[3]][z_test[[3]]$grade == "E", ]$elementid,
                                  z_test[[3]][z_test[[3]]$grade == "E", ]$grade),
               blockcoster_lookup(the_elementid = 1714,
                                  the_grade = factor("D", 
                                                     levels = c("N", "A", "B", "C", "D", "E"))) + 
                 blockcoster_lookup(the_elementid = 1714,
                                    the_grade = factor("D", 
                                                       levels = c("N", "A", "B", "C", "D", "E"))) * 0.05)
})

