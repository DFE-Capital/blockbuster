
# COSTING FAMILY OF FUNCTIONS ---------------------------------------------

# Estimating the repair cost of a building component given its grade and unit_area

#' Select the correct repair cost constant for a blockbuster tibble row.
#'
#' @param blockbuster_row a blockbuster tibble single row.
#' @param costs_lookup
#' @return a numeric constant for the repair cost of building component by grade, in pounds per unit_area.
#' @seealso \code{\link{blockbuster_pds_repair_costs}}
#' @export
#' @examples 
#' cost_to_repair_one_unit_area_back_to_grade_a <- blockcoster_lookup(blockbuster_pds[1, ])
blockcoster_lookup <- function(blockbuster_row, costs_lookup = blockbuster_pds_repair_costs) {
  
  # Test that we are passing a blockbuster-like dataframe or tibble to the function
  
  if (!tibble::is.tibble(blockbuster_row) && !is.data.frame(blockbuster_row)) stop("'blockbuster_row' must be a single row of a blockbuster tibble")
  
  #  Create new variable to match against
  #  We do this without pipes which impairs readibility, see comments  for what happens at each step
  blockbuster_row <- dplyr::mutate(blockbuster_row,
                                   concated_building_component_grade = paste(element, sub_element,
                                                                       const_type, grade,
                                                                       sep = "_"),
                                   concated_building_component_grade_clean = iconv(concated_building_component_grade,
                                                                                   from = "UTF-8", to = "ASCII", sub = "byte")
                                   )
  # remove weird hyphens
  blockbuster_row <- dplyr::mutate(blockbuster_row,
                                   concated_building_component_grade_clean =
                                     stringr::str_replace_all(concated_building_component_grade_clean, "<e2><80><93>", "-"))
  # remove numbers, white space and make lower case, the | means OR
  blockbuster_row <- dplyr::mutate(blockbuster_row,
                                   concated_building_component_grade_clean =
                                     stringr::str_replace_all(concated_building_component_grade_clean,
                                                              "[[:number:]]|[^[:alnum:]]| ", ""))
  # make lower case
  blockbuster_row <- dplyr::mutate(blockbuster_row,
                                   concated_building_component_grade_clean =
                                     stringr::str_to_lower(concated_building_component_grade_clean))
 #  http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r

  #  Match new variable and get index of match, this blockbuster_pds_repair_costs index as default
  pos <- as.integer()
  pos <- match(blockbuster_row$concated_building_component_grade_clean, 
    costs_lookup$concated_building_component_grade_clean)
  
  # Test that length pos is not zero, therefore it has been matched
  if (length(pos) == 0) stop("Repair cost constant of building component not found by name!")
  
  #  Use pos to provide correct row, use column name to select repair cost numeric value
  repair_costs_constant <- costs_lookup[pos, "repair_cost"]
  
  # Test that det_dtmc is NULL
  if (is.null(repair_costs_constant)) stop("Cost constant not assigned!")
  
  # Return for use in nested design
  return(repair_costs_constant$repair_cost)
}
