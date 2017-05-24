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
#  see costs tests for how we worked out what was missing, for the step below
costs_pre_tidy <- readr::read_csv("data-raw/repair_costs_pre_tidy_with_missing_appended.csv") %>%  
  dplyr::mutate(E = D + (D*0.05))  #  E = 105% of D


# COSTING E ---------------------------------------------------------------
#  Need to create an E variable which is the cost of D plus 5% according to AB


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
       measure.vars = c("E", "D", "C", "B", "A"),  #  Added E here with 105% of D change
       value.name = "repair_cost") %>%
  mutate(concated_costs = paste(element, sub_element,
                                                   const_type,
                                                   sep = "_"),
         concated_building_component_grade = paste(element, sub_element,
                                                   const_type, variable,
                                                              sep = "_")
         
         )
#  We help out blockbuster with some pre-processing here
#  Blockbuster will need to look-up building component and grade, let
costs_tidy <- costs_tidy %>%
  mutate(concated_building_component_grade_clean = iconv(concated_building_component_grade,
                                                         from = "UTF-8", to = "ASCII", sub = "byte") %>%
           stringr::str_replace_all("<e2><80><93>", "-") %>%  #  replace weird hyphen, here for consistency but only in the PDS data
           stringr::str_replace_all("[[:number:]]", "") %>%
           stringr::str_replace_all("[^[:alnum:]]", "") %>%  #  http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
           stringr::str_replace_all(" ", "") %>%
           stringr::str_to_lower())

# elementid ---------------------------------------------------------------
# left join on something with elementdid
# punctuation messes string matching, need to remove
costs_tidy_elementid <- mutate(costs_tidy,
                               concat_clean =
                                 iconv(
                                   concated_costs, from = "UTF-8", to = "ASCII", sub = "byte"
                                 )) %>%
  mutate(concat_clean = stringr::str_replace_all(
    concat_clean, "<e2><80><93>", "-"
  )) %>%
  mutate(concat_clean = stringr::str_replace_all(
    concat_clean, "[[:number:]]|[^[:alnum:]]| ", ""
  )) %>%
  mutate(concat_clean = stringr::str_to_lower(
    concat_clean
  ))

building_component_lookup_elementid <- building_component_lookup %>%
  mutate(concat_clean = paste(element, sub_element, const_type)) %>%
  mutate(concat_clean =
                                                iconv(
                                                  concat_clean, from = "UTF-8", to = "ASCII", sub = "byte"
                                                )) %>%
  mutate(concat_clean = stringr::str_replace_all(
    concat_clean, "<e2><80><93>", "-"
  )) %>%
  mutate(concat_clean = stringr::str_replace_all(
    concat_clean, "[[:number:]]|[^[:alnum:]]| ", ""
  )) %>%
  mutate(concat_clean = stringr::str_to_lower(
    concat_clean
  ))


costs_tidy_w_elementid <- left_join(costs_tidy_elementid, building_component_lookup_elementid,
          by = "concat_clean") %>%
  select(-(element.y:const_type.y)) %>%
  rename(element = element.x,
         sub_element = sub_element.x,
         const_type = const_type.x,
         grade = variable)

# if elementid NA then these don't appear in our 10% sample of the PDS

# UPDATED -----------------------------------------------------------------

costs_tidy <- costs_tidy_w_elementid
# Add correct levels to grade, N for new, E for decommisioned
levels(costs_tidy$grade) <- list(N = "N", A = "A", B = "B",
                                 C = "C", D = "D", E = "E")


# ELEMENTID-GRADE ---------------------------------------------------------

costs_tidy$concated_elementid_grade <- as.integer(paste0(costs_tidy$elementid,
                                              as.integer(costs_tidy$grade)
                                              ))

# save --------------------------------------------------------------------

write_excel_csv(costs_tidy, path = "./data-raw/repair_costs_tidy.csv")

blockbuster_pds_repair_costs <- tibble::as_tibble(costs_tidy)

# CREATE R DATA -----------------------------------------------------------

devtools::use_data(blockbuster_pds_repair_costs,
                   overwrite = TRUE)
  