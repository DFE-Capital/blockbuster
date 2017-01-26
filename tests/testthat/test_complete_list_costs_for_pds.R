library(blockbuster)
context("Determine that we have associated repair costs for every unique building component in the PDS")

# INPUT to test -----------------------------------------------------------
#  Identify all missing building repair costs, ignoring if upper or lower CASE,
#  based on character strings of element-sub_element-constr_type


x <- dplyr::mutate(blockbuster_pds, 
                   concated_pds = paste(element, sub_element, const_type,
                                        sep = "_"))

pds_concat_list <- unique(x$concated_pds)
# remove punctuation
pds_concat_list_alnum <- gsub("[^[:alnum:] ]", "", pds_concat_list)
# make lower case
pds_concat_list_alnum_lower <- sapply(pds_concat_list_alnum, tolower)

# Do the same for repair costs but remember the underscores
costs_concat_list <- unique(blockbuster_pds_repair_costs$concated_costs)
costs_concat_list_alnum <- gsub("[^[:alnum:] ]", "", costs_concat_list)
costs_concat_list_alnum_lower <- sapply(costs_concat_list_alnum, tolower)


# MATCH -------------------------------------------------------------------
is_it_missing <- match(pds_concat_list_alnum_lower, costs_concat_list_alnum_lower)
sum(is.na(is_it_missing))
pds_concat_list_alnum_lower[is.na(is_it_missing)]
#  For original names
costs_missing <- pds_concat_list[is.na(is_it_missing)]

# TESTING -----------------------------------------------------------------
# If all is well we should have an empty dtmc_missing
# if so we have all the det_rates we need to match
# the pds
test_that("There are missing repair costs data!", {
  expect_true(purrr::is_empty(costs_missing))
})

# IF TEST FAILS WE NEED THESE REPAIR COSTS
#  We can write as csv for AJJ to complete
#  Convert strings into dataframe, see https://stat.ethz.ch/pipermail/r-help/2011-April/275154.html

# string_splitter <- strsplit(costs_missing,"_")
# 
# costs_missing_for_excel <- tibble::tibble(element = sapply(string_splitter, function(x){x[1]}),
#                                           sub_element = sapply(string_splitter, function(x){x[2]}),
#                                           const_type = sapply(string_splitter, function(x){x[3]})
# )
# 
# readr::write_excel_csv(costs_missing_for_excel, "./data-raw/missing_repair_costs.csv",
#                        append = FALSE)