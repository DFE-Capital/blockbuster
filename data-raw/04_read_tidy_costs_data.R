# PURPOSE -----------------------------------------------------------------

# Create a lookup table of costs associated with each possible building component
# and the repair costs of taking one unit area of that building component back
# to A grade condition from any other condition grade e.g. B, C, D
# The input is the repair_costs_pre_tidy.csv
# output is the repair_costs.Rdata

costs_pre_tidy <- readr::read_csv("data-raw/repair_costs_pre_tidy.csv")

# The element, sub-element and const_type have numbers prefixed, let's remove those
costs_tidy <- dplyr::mutate(costs_pre_tidy, 
                            element = gsub("\\b\\d+\\b", "", element),
                            sub_element = gsub("\\b\\d+\\b", "", sub_element),
                            const_type = gsub("\\b\\d+\\b", "", const_type)
                            )
