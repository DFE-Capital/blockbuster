
# GET DATA ----------------------------------------------------------------
# source("00_get_data.R")
# script currently empty

# INPUT -------------------------------------------------------------------

library(tidyverse)
block_data <- read_tsv(file  = "blockbuster_data.txt",
                       col_names = TRUE)
#  when read in some NULL in composition, only missing data, should be 1? omit for now
#  see GDA of condition, final plot visna for details and proof of this
#  available on CDC Trello board

# LOOK AT DATA ------------------------------------------------------------

#glimpse(block_data)
#nrow(block_data) - sum(complete.cases(block_data))  #  dodgy rows
block_data_complete <- block_data %>%
  filter(complete.cases(.))

# VALID NAMES -------------------------------------------------------------
# Easier to change in SQL for future compatability?
# Worry aboiut this later

# CLEAN -------------------------------------------------------------------

