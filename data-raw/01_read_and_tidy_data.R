
# Convert SQL tab delimited .txt to .RData --------------------------------
# purpose as above, take SQL query data as tab delimited file
# tidy and create some useful new variables


# GET DATA ----------------------------------------------------------------
# source("00_get_data.R")
# script currently empty

# INPUT -------------------------------------------------------------------

library(tidyverse)

block_data <- read_tsv(file  = "./data-raw/blockbuster_data.txt",
                       col_names = TRUE)  #  blockbuster_data_v3_MG

# LOOK AT DATA ------------------------------------------------------------
# dodgy encoding for first variable, thus repositioned in SQL query
#glimpse(block_data)
#nrow(block_data) - sum(complete.cases(block_data))  #  dodgy rows, composition NULL
block_data_complete <- block_data %>%
  filter(complete.cases(.)) %>%
  mutate(timestep = as.integer(0))  #  add time zero  
  
# ADD VARIABLES -----------------------------------------------------------
# compute the sum of repair costs by block, then append this on for tidy data
block_cost_sum <- block_data_complete %>%
  group_by(buildingid) %>%
  summarise(cost_sum = sum(cost))

block_data_complete <- block_data_complete %>%
  left_join(x = ., y = block_cost_sum, by = "buildingid")  #  where . passes data

#glimpse(block_data_complete)

# VARIABLE TYPE -------------------------------------------------------------------
to_correct <- c("site_ref", "block_ref",
                "element", "sub_element", "const_type",
                "const_type_accessible", "grade", "swimming_pool")
# Correct data type of variable
# use mutate_each_, which is the standard evaluation version, to change variable classes
block_data_correct_type <- block_data_complete %>%
  mutate_each_(funs(factor), to_correct)
#glimpse(block_data_correct_type)

# Add correct levels to grade, N for new, E for decommisioned
levels(block_data_correct_type$grade) <- list(N = "N", A = "A", B = "B",
                                              C = "C", D = "D", E = "E")
#table(block_data_correct_type$grade)

# TIDY DATA ---------------------------------------------------------------

blockbuster_pds <- block_data_correct_type %>%
  select(-1, -dfeno, -urn)  #  drop irrelevant yet sensitive columns
#glimpse(block_tidy)


# CREATE R DATA -----------------------------------------------------------

devtools::use_data(blockbuster_pds, overwrite = TRUE)
