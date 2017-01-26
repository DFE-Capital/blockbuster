library(blockbuster)
context("Determine that we have associated repair costs for every unique building component in the PDS")

# INPUT to test -----------------------------------------------------------
#  Identify all missing building repair costs, ignoring if upper or lower CASE,
#  based on character strings of element-sub_element-constr_type

x <- dplyr::mutate(blockbuster_pds, 
                   concated_pds = paste(element, sub_element, const_type,
                                        sep = "_"))

# PDS unique building components for which we need costs at D, C, B and A
pds_concat_list <- unique(x$concated_pds)
pds_concat_list_converted <- iconv(unique(x$concated_pds),
                         from = "UTF-8", to = "ASCII", sub = "byte")  #  replace weird hyphen

pds_concat_list_alnum_lower <- pds_concat_list_converted %>%
  stringr::str_replace_all("<e2><80><93>", "-") %>%  #  replace weird hyphen
  stringr::str_replace_all("[[:number:]]", "") %>%
  stringr::str_replace_all("[^[:alnum:]]", "") %>%  #  http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
  stringr::str_replace_all(" ", "") %>%
  stringr::str_to_lower() 

# Do the same for repair costs
# We use the pre_tidy for scouting
# Also test tidy and complete version later
costs_pre_tidy <- readr::read_csv("repair_costs_pre_tidy.csv") %>%
  dplyr::mutate(concated_costs = paste(element, sub_element,
                                const_type,
                                sep = "_"))
costs_concat_list_alnum_lower <- iconv(unique(costs_pre_tidy$concated_costs),
                                       from = "UTF-8", to = "ASCII", sub = "byte") %>%
  stringr::str_replace_all("<e2><80><93>", "-") %>%  #  replace weird hyphen
  stringr::str_replace_all("[[:number:]]", "") %>%
  stringr::str_replace_all("[^[:alnum:]]", "") %>%  #  http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
  stringr::str_replace_all(" ", "") %>%
  stringr::str_to_lower() 

# MATCH -------------------------------------------------------------------
is_it_missing <- match(pds_concat_list_alnum_lower, costs_concat_list_alnum_lower)
# sum(is.na(is_it_missing))
# pds_concat_list_alnum_lower[is.na(is_it_missing)]

#  For original names
costs_missing <- pds_concat_list[is.na(is_it_missing)]

# TESTING -----------------------------------------------------------------
test_that("There are missing repair costs data! Known unknowns.", {
  expect_equal(length(costs_missing), 21)
})

# IF TEST FAILS WE NEED THESE REPAIR COSTS
#  We can write as csv for AJJ to complete
#  Convert strings into dataframe, see https://stat.ethz.ch/pipermail/r-help/2011-April/275154.html
# Turns out we do have all the necessary costs, there was an issue with spacing when reading the strings
# now fixed 2017-01-27 MG

# string_splitter <- strsplit(costs_missing,"_")
# 
# costs_missing_for_excel <- tibble::tibble(element = sapply(string_splitter, function(x){x[1]}),
#                                           sub_element = sapply(string_splitter, function(x){x[2]}),
#                                           const_type = sapply(string_splitter, function(x){x[3]})
# )
# 
# readr::write_excel_csv(costs_missing_for_excel, "./data-raw/missing_repair_costs.csv",
#                        append = FALSE)


# FURTHER TESTING POST REPAIR COST EXTRA EDIT -----------------------------
# Change the weird hyphen for some reason
costs_with_missing <- readr::read_csv("repair_costs_pre_tidy_with_missing_appended.csv") %>%
  dplyr::mutate(concated_costs = paste(element, sub_element,
                                const_type,
                                sep = "_"))
costs_with_missing_concat_list_alnum_lower <- iconv(unique(costs_with_missing$concated_costs),
                                       from = "UTF-8", to = "ASCII", sub = "byte") %>%
  stringr::str_replace_all("<e2><80><93>", "-") %>%  #  replace weird hyphen
  stringr::str_replace_all("[[:number:]]", "") %>%
  stringr::str_replace_all("[^[:alnum:]]", "") %>%  #  http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
  stringr::str_replace_all(" ", "") %>%
  stringr::str_to_lower() 

# MATCH -------------------------------------------------------------------
is_it_missing <- match(pds_concat_list_alnum_lower, costs_with_missing_concat_list_alnum_lower)
# sum(is.na(is_it_missing))
# pds_concat_list_alnum_lower[is.na(is_it_missing)]

#  For original names
costs_missing_2 <- pds_concat_list[is.na(is_it_missing)]

# TEST --------------------------------------------------------------------
# should be fixed with encoding solution, important lesson
# also look up strings is slow, should make a lookup table based on a number or code for simulation

test_that("There are missing repair costs data! Which we don't know.", {
  expect_true(purrr::is_empty(costs_missing_2))
})
