library(blockbuster)
context("Determine that we have associated transition matrices for every unique building component in the PDS")

# INPUT to test -----------------------------------------------------------
#  Identify all missing markovchain objects and transition matrices
#  based on character strings of element-sub_element-constr_type


x <- dplyr::mutate(blockbuster_pds, 
                   concated_pds = paste(element, sub_element, const_type,
                                        sep = "_"))

pds_concat_list <- unique(x$concated_pds)
# remove punctuation
pds_concat_list_alnum <- gsub("[^[:alnum:] ]", "", pds_concat_list)
# make lower case
pds_concat_list_alnum_lower <- sapply(pds_concat_list_alnum, tolower)

det_concat_list <- unique(blockbuster_det_data$concated_det)
det_concat_list_alnum <- gsub("[^[:alnum:] ]", "", det_concat_list)
det_concat_list_alnum_lower <- sapply(det_concat_list_alnum, tolower)


# MATCH -------------------------------------------------------------------
is_it_missing <- match(pds_concat_list_alnum_lower, det_concat_list_alnum_lower)
sum(is.na(is_it_missing))
pds_concat_list_alnum_lower[is.na(is_it_missing)]
#  For original names
dtmc_missing <- pds_concat_list[is.na(is_it_missing)]
#  We need deterioration rates for these combinations


# TESTING -----------------------------------------------------------------
# If all is well we should have an empty dtmc_missing
# if so we have all the det_rates we need to match
# the pds
test_that("There are no missing det_rate dtmc", {
  expect_true(purrr::is_empty(dtmc_missing))
})

