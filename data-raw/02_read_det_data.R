
# PURPOSE -----------------------------------------------------------------

# Create a markov chain list of deterioration rate transition matrices
# for every unique element sub-element construction type combination

# Get deterioration rates into useable format --------------------------------
# take Deterioration Rates.xlsx provided by EC Harris
# using the Overarching Rates sheet
# copy and pasted and then adjusted column names and fixed NA coding
# (it would of taken longer to code it)
# extract for each unique element_sub_element_con_type
# An encoding error occurred: http://stackoverflow.com/questions/24500162/display-u0096-in-a-jsp
# I replaced the offending character with a -
# This made me think, it would be better, to ensure character matching
# by manually comparing the levels of det rate ele, sub and const with the pds data.
# It seems there is an error with minus sign encoding
# "Staircases - Balustrades" == "Staircases – Balustrades"
# gsub("[^[:alnum:] ]", "", "Staircases - Balustrades") == gsub("[^[:alnum:] ]", "", "Staircases – Balustrades")
# Matchign occurs jsut on alphanumeric now


# MISSING DATA ------------------------------------------------------------

# Some det rates were not provided by EC Harris
# We used the expert opinion of quantity surveyors in the Condition and Cost team
# Amanda Jekin-Jones and Colin Fayers to discuss viable estimates of missing rates
# using their expert knowledge and what EC Harris had provided for similar
# building components. Hence filename renamed det_data_ajj.txt after edits.
# See Trello board for details of justifications.
# Floors and Stairs, Floors - Screed & Finish, CONCRETE / UNFINISHED SCREED / FLOOR PAINT copied from 
# Floors and Stairs, STRUCTURE, CONCRETE middle of 45-75 is 60, so 60/4 at each grade, 1/15 rate
# I followed AJJ advice where possible and although some det rates were deemed to not really
# be needed asthey would be "unlikely" to occur, there were instances, so I sent these back to
# AJJ for clarification. There is a risk here that some det rates were deleted that were rare,
# i.e. not showing in our 10% sample but maybe in the whole data will appear and cause an error
# later, if it's all used.

# CAVEAT ------------------------------------------------------------------
# The overarching rates are assumed to be the averages irrespective of building type
# We added a new column for N to A, new to condition A.
# EC Harris had no data on this, newness probably depends on the element.
# For simplicity and testing we fix it here as x years/
# FLOOR AND STAIRS was NA, we got this from modified EC Harris data for
# the modal (by records) building type, post-1976.
#
# 2016-11-30
# Following Colin Fayers sharing of building component life summary
# MG used his own judgement to fill in the remaining NA by picking 
# similar components to the missing data
# e.g. if 50 years, it would be split as ab 1/20, bc 1/15, cd 1/10, de 1/5
# the idea being that well maintained components stay viable for longer
# this created new file with ajj_cf suffix

# 2016-12-01
# After this there were still 4 missing det rates that were in the PDS
# but not in our det_rates tsv, thus we used 03_identify_missing_markovs.R
# to find them out and then made up some rates for them in ajj_cf suffixed
# it looks like these are relics from the characters being misread!
# we already have the data it seems. Most recent four are added
# to the bottom of the det_rates tsv

# 2017-04-27
# at the extra mutation step to the concated_det variable by removing


# GET DATA ----------------------------------------------------------------
# this should be replaced later by getting the det rates from
# real data, rather than relying on the estimates of expert opinion

# INPUT -------------------------------------------------------------------

library(tidyverse)

det_data <- read_tsv(file  = "./data-raw/det_data_ajj_cf.txt",
                       col_names = TRUE) %>%
  mutate(concated_det =  gsub("[^[:alnum:] ]", "",
                              paste(element, sub_element, const_type,
                          sep = "_"))) %>%
  filter(complete.cases(.)) %>%  # NA are getting removed
  filter(!duplicated(.[["concated_det"]])) #  remove duplicates from Excel

# length(unique(det_data$concated_det))
# sum(!complete.cases(det_data))  #  these seem to be relics from the Excel, not used
# let's remove the NA, we need to consider this later

blockbuster_det_data <- as_tibble(det_data)

# CREATE R DATA -----------------------------------------------------------

devtools::use_data(blockbuster_det_data, overwrite = TRUE)

# ELEMENTID OVERHAUL 2017-05-17 - new feature
# can't remember where building_component_lookup came from, let's check it 
# has all the relevant elementid in it
# it probably came from the pds? so should do
x <- unique(blockbuster_pds$elementid)
y <- blockbuster::building_component_lookup$elementid
x %in% y
rm(x, y)
# of greater interest is whether our elementid in det_data_elementid
# captures all the relevant elementid

det_data_elementid <- read_tsv(file  = "./data-raw/det_data_elementid.txt",
                     col_names = TRUE) %>%
  mutate(concated_det =  gsub("[^[:alnum:] ]", "",
                              paste(element, sub_element, const_type,
                                    sep = "_")))
# this does seem to hold all the relevant building components elementid
# x <- unique(det_data_elementid$elementid)  #  x has some NA but irrelevant missing as not in PDS
# y <- unique(blockbuster_pds$elementid)
# x %in% y
# rm(x, y)

#  need to remove NAs
det_data_elementid <- det_data_elementid %>%
  filter(complete.cases(.)) %>%  # NA are getting removed
  filter(!duplicated(.[["concated_det"]])) #  remove duplicates from Excel


blockbuster_det_data <- as_tibble(det_data_elementid)
devtools::use_data(blockbuster_det_data, overwrite = TRUE)

# for next step
det_data <- det_data_elementid

# MARKOV CHAIN ------------------------------------------------------------
library(markovchain)
# Create a list of markov chain objects, one for each
# element sub_element con_type combination
# e.g. nn, read as from n to n...

# READ CAREFULLY THIS IS FOR SINGLE CASE EXAMPLE
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
rm(det_dtmc, get_det)

# MARKOV LIST -------------------------------------------------------------

#  fix function for use in for loop, note i change
get_det <- function(x) {
  as.numeric(det_data[i, x])
}

#  pre-allocate memory for list
mc_list <- vector(mode = "list", length = nrow(det_data))

#  for loop interpretable and fast here
#  create list to pass to markov chain list
for (i in 1:nrow(det_data)) {
  mc_list[[i]] <- new("markovchain",
                      transitionMatrix = matrix(c(1 - get_det("na"), get_det("na"), 0, 0, 0, 0,  #  n
                                                  0, 1 - get_det("ab"), get_det("ab"), 0, 0, 0,  #  a
                                                  0, 0, 1 - get_det("bc"), get_det("bc"), 0, 0,  #  b
                                                  0, 0, 0, 1 - get_det("cd"), get_det("cd"), 0,  #  c
                                                  0, 0, 0, 0, 1 - get_det("de"), get_det("de"),  #  d
                                                  0, 0, 0, 0, 0, 1),  #  ee is 1
                                                nrow = 6, ncol = 6, byrow = TRUE),
                      states = c("n", "a", "b", "c", "d", "e"),
                      name = as.character(det_data[i, "concated_det"]))
}

rm(i, get_det)
# Make Markov Chain list
# See documentation for markovchainList
blockbuster_mc_list <- new("markovchainList",
                           markovchains = mc_list
                           ,
                           name = "A list of Markov chains for each element sub_element con_type"
                           )

# blockbuster_mc_list@markovchains[[1]]@name  #  find individual name

# SAVE MC LIST ------------------------------------------------------------
devtools::use_data(blockbuster_mc_list, overwrite = TRUE)
