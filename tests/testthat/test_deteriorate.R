library(blockbuster)
context("det_eriorate blockbuster PDS tibble through one timestep")



# INPUT to test -----------------------------------------------------------

w <- head(dplyr::filter(blockbuster_pds, const_type == "No mechanical ventilation or air conditioning"),
          n = 1L)  #  no det rates for this
x <- blockbuster_pds[255, ]
y <- blockbuster_pds[1337, ]
ye <- y
ye$grade <- factor("E", levels(y$grade))  #  grade set to "E"
z <- blockbuster_pds[40000, ]

# TESTS -------------------------------------------------------------------

test_that("det_what_tm finds the correct markov chain", {
  expect_equal(det_what_tm(blockbuster_pds[1,]),
               blockbuster_mc_list@markovchains[[117]])
  #expect_(det_what_tm(w),  blockbuster_mc_list@markovchains[[]])
  #expect_equal(det_what_tm(x),  blockbuster_mc_list@markovchains[[]])
  expect_equal(det_what_tm(y),  blockbuster_mc_list@markovchains[[79]])
  expect_equal(det_what_tm(z),  blockbuster_mc_list@markovchains[[97]])
})

test_that("det_eriorate outputs two rows if appropriate", {
  #expect_equal(nrow(det_eriorate(w)), 2)
  #expect_equal(nrow(det_eriorate(x)), 2)
  expect_equal(nrow(det_eriorate(y)), 2)
  expect_equal(nrow(det_eriorate(ye)), 1)
  expect_equal(nrow(det_eriorate(z)), 2)
})

test_that("det_eriorate outputs correct variables", {
  expect_equal(ncol(y), ncol(det_eriorate(y)))
  expect_equal(y$unit_area, sum(det_eriorate(y)$unit_area))
  expect_equal(y$timestep + 1, head(det_eriorate(y)$timestep, n = 1L))
})

test_that("det_eriorate outputs correct grades", {
  expect_equal(as.character(head(det_eriorate(y)$grade, 1)), "C")
  expect_equal(as.character(tail(det_eriorate(y)$grade, 1)), "D")
  expect_equal(as.character(tail(det_eriorate(ye)$grade, 1)), "E")
})

v <- blockbuster_pds[1:5, ]

test_that("blockbust() produces correct row number", {
  expect_equal(nrow(blockbust(v)), nrow(v)*2)  #  if grade not E, then row number doubles
  expect_equal(nrow(blockbust(blockbust(v))), nrow(v)*4)  #  through 2 timesteps of deterioration
  expect_equal(blockbust(y), det_eriorate(y))  #  identical for one row
})

test_that("blockbuster produces correct row number by list", {
  expect_equal(NROW(dplyr::mutate(blockbuster(v, forecast_horizon = 2)[[3]],
                      cost = 0, cost_sum = 0)),
               NROW(tibble::as_tibble(aggregate(unit_area ~.,
                                           data = dplyr::mutate(blockbust(blockbust(v)), 
                                                                cost = 0, cost_sum = 0), # we set the cost here to zero as blockbust doesnt do cost
                                           FUN = sum)))
               )  #  demonstrate blockbuster aggregateness
  expect_equivalent(NROW(blockbuster(y, forecast_horizon = 5)[[6]]), 3)  #  One elementid starts at grade C, so C, D, E
})