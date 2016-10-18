
# Get deterioration rates into useable format --------------------------------
# take Deterioration Rates.xlsx provided by EC Harris
# using the Overarching Rates sheet
# copy and pasted and then adjusted column names and fixed NA coding
# extract for each unique element_sub_element_con_type


# CAVEAT ------------------------------------------------------------------
# The overarching rates are assumed to be the averages irrespective of building type
# We added a new column for N to A, new to condition A.
# EC Harris had no data on this, newness probably depends on the element.
# For simplicity and testing we fix it here as x years/
# FLOOR AND STAIRS was NA, we got this from modified EC Harris data for
# the modal (by records) building type, post-1976.
#


# GET DATA ----------------------------------------------------------------
# this should be replaced later by getting the det rates from
# real data, rather than relying on the estimates of expert opinion

# INPUT -------------------------------------------------------------------

library(tidyverse)

det_data <- read_tsv(file  = "./data-raw/det_data.txt",
                       col_names = TRUE) %>%
  mutate(concated_det = paste(element, sub_element, const_type,
                          sep = "_")) %>%
  filter(complete.cases(.)) %>%  
  filter(!duplicated(.[["concated_det"]])) #  remove duplicates from Excel

# length(unique(det_data$concated_det))
# sum(!complete.cases(det_data))  #  these seem to be relics from the Excel, not used
# let's remove the NA, we need to consider this later

blockbuster_det_data <- det_data

# CREATE R DATA -----------------------------------------------------------

devtools::use_data(blockbuster_det_data, overwrite = TRUE)

# MARKOV CHAIN ------------------------------------------------------------
library(markovchain)
# Create a list of markov chain objects, one for each
# element sub_element con_type combination
# e.g. nn, read as from n to n...

# Helper function to keep code tidy
get_det <- function(x) {
  as.numeric(det_data[1, x])
}

# We demonstrate how it works on single 
det_dtmc <- new("markovchain",
                transitionMatrix = matrix(c(1 - get_det("na"), get_det("na"), 0, 0, 0, 0,  #  n
                                            0, 1 - get_det("ab"), get_det("ab"), 0, 0, 0,  #  a
                                            0, 0, 1 - get_det("bc"), get_det("bc"), 0, 0,  #  b
                                            0, 0, 0, 1 - get_det("cd"), get_det("cd"), 0,  #  c
                                            0, 0, 0, 0, 1 - get_det("de"), get_det("de"),  #  d
                                            0, 0, 0, 0, 0, 1),  #  ee is 1
                                          nrow = 6, ncol = 6, byrow = TRUE),
                states = c("n", "a", "b", "c", "d", "e"),
                name = as.character(det_data[1, "concated_det"]))  #duplicate names have been removed
# Works for the single case, let's create a list

# MARKOV LIST -------------------------------------------------------------
# to do, get ths to work for a list
get_det <- function(x) {
  as.numeric(det_data[, x])
}



blockbuster_mc_list <- new("markovchainList",
                           markovchains = list(
                              
                           ),
                           name = "A list of Markov chains for each element sub_element con_type"
                           )
