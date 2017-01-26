# PURPOSE -----------------------------------------------------------------

# Create a lookup table of costs associated with each possible building component
# and the repair costs of taking one unit area of that building component back
# to A grade condition from any other condition grade e.g. B, C, D
# The input is the repair_costs_pre_tidy.csv 
# The dodgy hyphens were changed into normal hyphens
# output is the repair_costs.Rdata
library(tidyverse)
library(reshape2)

# costs_pre_tidy <- readr::read_csv("data-raw/repair_costs_pre_tidy.csv")  #  this is redundant as we had missing building components
costs_pre_tidy <- readr::read_csv("data-raw/repair_costs_pre_tidy_with_missing_appended.csv")  #  see costs tests for how we worked out what was missing



# The element, sub-element and const_type have numbers prefixed, let's remove those
# and the decimal place adn then trim the white space before converting into long format
# We also reshape
costs_tidy <- dplyr::mutate(costs_pre_tidy, 
                            element = gsub("\\b\\d+\\b", "", element),
                            sub_element = gsub("\\b\\d+\\b", "", sub_element),
                            const_type = gsub("\\b\\d+\\b", "", const_type)
                            ) %>%  ##  http://stackoverflow.com/questions/31518150/gsub-in-r-not-replacing-dot
  dplyr::mutate(element = gsub(".", "", element, fixed = TRUE),
                sub_element = gsub(".", "", sub_element, fixed = TRUE),
                const_type = gsub(".", "", const_type, fixed = TRUE)
                ) %>%
  dplyr::mutate(element = stringr::str_trim(element),
                sub_element = stringr::str_trim(sub_element),
                const_type = stringr::str_trim(const_type)) %>%  #  https://www.r-bloggers.com/melt/
  melt(id.vars = c("element", "sub_element", "const_type"),
       measure.vars = c("D", "C", "B", "A"),
       value.name = "repair_cost") %>%
  mutate(concated_costs = paste(element, sub_element,
                                                   const_type,
                                                   sep = "_"),
         concated_building_component_grade = paste(element, sub_element,
                                                   const_type, variable,
                                                              sep = "_"))

write_excel_csv(costs_tidy, path = "./data-raw/repair_costs_tidy.csv")

blockbuster_pds_repair_costs <- tibble::as_tibble(costs_tidy)

# CREATE R DATA -----------------------------------------------------------

devtools::use_data(blockbuster_pds_repair_costs,
                   overwrite = TRUE)
  