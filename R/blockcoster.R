
# COSTING FAMILY OF FUNCTIONS ---------------------------------------------

# Estimating the repair cost of a building component given its grade and unit_area

#' An internal function in \code{\link{blockbuster}} to select the correct repair cost constant for a building component and its condition grade. 
#' It works by matching strings from the concatenated \code{element}, \code{sub_element}, \code{const_type} and \code{grade}.
#' 
#' The default is to use the repair cost estimates for the PDS plus
#' an E grade cost estimator based on D grade cost plus 5% (removal costs etc.). This
#' is likely to be a source of error as the replacement of a decommisioned component
#' depends on the component as well as other complexities. This should be addressed in later
#' improvements of the \code{\link{blockbuster}}.  
#'
#' @param concated the pasted \code{element}, \code{sub_element} and \code{const_type}
#'  of a blockbuster tibble single row as shown in the example.
#' @param grade the grade of a blockbuster tibble single row
#' @param costs_lookup the relevant costs look up table, default is derived from PDS 2016 costs
#' @return a numeric constant for the repair cost of building component by grade, in pounds per unit_area.
#' @seealso \code{\link{blockbuster_pds_repair_costs}}
#' @export
#' @examples 
#' cost_to_repair_one_unit_area_back_to_grade_a <- blockcoster_lookup(
#' dplyr::mutate_(blockbuster_pds[3, ], 
#' concated = ~gsub(pattern = "[^[:alnum:] ]", replacement = "",
#' paste(element, sub_element, const_type, sep = "")))$concated,
#' blockbuster_pds[3, ]$grade)

blockcoster_lookup <- function(
  the_elementid, the_grade, costs_lookup = blockbuster_pds_repair_costs
) {
  
  #  Discovered bug, it was placing NA cost for N grade as this was not
  #  in the lookup table

    
    #  Create new variable to match against
    #  We do this without pipes which impairs readibility, see comments  for what happens at each step
    
    # concated_building_component_grade <- paste(
    #   concated, grade, sep = ""
    # )
    # 
    # concated_building_component_grade_clean <- iconv(
    #   concated_building_component_grade, from = "UTF-8", to = "ASCII", sub = "byte"
    # )
    # 
    # concated_building_component_grade_clean <- stringr::str_replace_all(
    #   concated_building_component_grade_clean, "<e2><80><93>", "-"
    # )
    # 
    # concated_building_component_grade_clean <- stringr::str_replace_all(
    #   concated_building_component_grade_clean, "[[:number:]]|[^[:alnum:]]| ", ""
    # )
    # 
    # concated_building_component_grade_clean <- stringr::str_to_lower(
    #   concated_building_component_grade_clean
    # )
    # 
    #  http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
    
    #  Match new variable and get index of match, this blockbuster_pds_repair_costs index as default
    # pos <- as.integer()
    # pos <- match(
    #   concated_building_component_grade_clean, costs_lookup$concated_building_component_grade_clean
    # )
    
    #  USE ELEMENTID INSTEAD
    pos <- as.integer()
    # pos <- which(elementid == costs_lookup$elementid &
    #   grade == costs_lookup$grade)
    pos <- which(the_elementid == costs_lookup$elementid)
    # print(pos)
    pos_elementid_match <- costs_lookup[pos, ]
    # print(pos_elementid_match)
    pos_grade_match <- pos_elementid_match[pos_elementid_match$grade == "D", ]
    # print(pos_grade_match)
    

    # print(pos)
    # Test that length pos is not zero, therefore it has been matched
    # if (length(pos) == 0) stop("Repair cost constant of building component not found by name!")

    #  Use pos to provide correct row, use column name to select repair cost numeric value
    repair_costs_constant <- pos_grade_match[, "repair_cost"]
    # print(repair_costs_constant)
    
    # It's OK if its NA for grade E
    if (is.null(repair_costs_constant)) stop("Cost constant not assigned!")
    
    # If grade is "N" it gives NA
    # The user might prefer a zero cost similar to how grade A is costed
    # If NA replace with zero, vectorised
    
    # This could be a problem if matching lookup not found
    repair_costs_constant[is.na(repair_costs_constant)] <- 0
    
    # Return for use in nested design
    return(repair_costs_constant$repair_cost)

  
  
}
