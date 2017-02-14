
# COSTING FAMILY OF FUNCTIONS ---------------------------------------------

# Estimating the repair cost of a building component given its grade and unit_area

#' An internal function in \code{\link{blockbuster}} to select the correct repair cost constant for a building component and its condition grade. 
#' It works by matching strings from the concatenated \code{element}, \code{sub_element}, \code{const_type} and \code{grade}.
#'
#' @param element the element of a blockbuster tibble single row.
#' @param sub_element the sub_element of a blockbuster tibble single row
#' @param const_type the const_type of a blockbuster tibble single row
#' @param grade the grade of a blockbuster tibble single row
#' @param costs_lookup the relevant costs look up table, default is derived from PDS 2016 costs
#' @return a numeric constant for the repair cost of building component by grade, in pounds per unit_area.
#' @seealso \code{\link{blockbuster_pds_repair_costs}}
#' @export
#' @examples 
#' cost_to_repair_one_unit_area_back_to_grade_a <- blockcoster_lookup(blockbuster_pds[3, ]$element, blockbuster_pds[3, ]$sub_element, 
#' blockbuster_pds[3, ]$const_type, blockbuster_pds[3, ]$grade)

blockcoster_lookup <- function(
  element, sub_element, const_type, grade, costs_lookup = blockbuster_pds_repair_costs
) {
  
  #  Create new variable to match against
  #  We do this without pipes which impairs readibility, see comments  for what happens at each step
  
  concated_building_component_grade <- paste(
    element, sub_element, const_type, grade, sep = "_"
  )
  
  concated_building_component_grade_clean <- iconv(
    concated_building_component_grade, from = "UTF-8", to = "ASCII", sub = "byte"
  )
  
  concated_building_component_grade_clean <- stringr::str_replace_all(
    concated_building_component_grade_clean, "<e2><80><93>", "-"
  )
  
  concated_building_component_grade_clean <- stringr::str_replace_all(
    concated_building_component_grade_clean, "[[:number:]]|[^[:alnum:]]| ", ""
  )
  
  concated_building_component_grade_clean <- stringr::str_to_lower(
    concated_building_component_grade_clean
  )

 #  http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r

  #  Match new variable and get index of match, this blockbuster_pds_repair_costs index as default
  pos <- as.integer()
  pos <- match(
    concated_building_component_grade_clean, costs_lookup$concated_building_component_grade_clean
  )
  
  # Test that length pos is not zero, therefore it has been matched
  if (length(pos) == 0) stop("Repair cost constant of building component not found by name!")
  
  #  Use pos to provide correct row, use column name to select repair cost numeric value
  repair_costs_constant <- costs_lookup[pos, "repair_cost"]
  
  # It's OK it its NA for grade E
  if (is.null(repair_costs_constant)) stop("Cost constant not assigned!")
  
  # Return for use in nested design
  return(repair_costs_constant$repair_cost)
}
