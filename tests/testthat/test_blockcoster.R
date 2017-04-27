library(blockbuster)

context("Looking up and estimating repair costs for a row in a blockbuster tibble.")



# INPUT to test -----------------------------------------------------------

x <- dplyr::select(blockbuster_pds[1, ], element, sub_element, const_type, grade)
x <- dplyr::mutate(x, concated = gsub(pattern = "[^[:alnum:] ]",
                         replacement = "",
                         paste(element, sub_element, const_type,
                               sep = "")))
z_test <- blockbuster(blockbuster_pds[1:10, ], 1)

# TEST NEW DESIGN different input ---------------------------------------------------------
test_that("blockcoster_lookup no longer receives tibble, receives the cached concated and grade.", {
  expect_true(is.numeric(blockcoster_lookup(x$concated, x$grade)))
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
  expect_equal(blockcoster_lookup(concated = z_test[[2]]$concated,
                                  grade = "E"),
               blockcoster_lookup(concated = z_test[[2]]$concated,
                                  grade = "D") + 
                 blockcoster_lookup(concated = z_test[[2]]$concated,
                                                                    grade = "D") * 0.05)
})

