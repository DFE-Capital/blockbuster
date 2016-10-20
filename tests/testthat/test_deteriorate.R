library(blockbuster)
context("det_eriorate blockbuster tibble through one timestep")



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
               blockbuster_mc_list@markovchains[[116]])
  #expect_(det_what_tm(w),  blockbuster_mc_list@markovchains[[]])
  #expect_equal(det_what_tm(x),  blockbuster_mc_list@markovchains[[]])
  expect_equal(det_what_tm(y),  blockbuster_mc_list@markovchains[[79]])
  expect_equal(det_what_tm(z),  blockbuster_mc_list@markovchains[[97]])
})

test_that("deteriorate outputs two rows if appropriate", {
  #expect_equal(nrow(det_eriorate(w)), 2)
  #expect_equal(nrow(det_eriorate(x)), 2)
  expect_equal(nrow(det_eriorate(y)), 2)
  expect_equal(nrow(det_eriorate(ye)), 1)
  expect_equal(nrow(det_eriorate(z)), 2)
})

test_that("deteriorate outputs correct variables", {
  expect_equal(ncol(y), ncol(det_eriorate(y)))
  expect_equal(y$unit_area, sum(det_eriorate(y)$unit_area))
  expect_equal(y$timestep + 1, head(det_eriorate(y)$timestep, n = 1L))
})