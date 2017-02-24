
# AREAFY FUNCTION FAMILY --------------------------------------------------

# These functions take the initial input of the condition data produced
# by the Property Data Survey and, for each row and associated element-sub-element-con-type
# estimate the unit_area (an approximation or sort of area of each observation).
# In effect we create a new variable called unit_area. This is critical for the later
# deterioration stages, as you need an approximation of how much of something there
# is, if you want to deteriorate it using a discrete-time markov chain approach. 

#' Estimate the unit_area for an observation for a blockbuster tibble.
#'
#' @param blockbuster_initial_state a blockbuster dataframe or tibble.
#' @return a blockbuster tibble with the unit_area estimated for each row.
#' This should be used to provide the initial estimates of the unit_area
#' of each element-sub-element construction type in the dataframe.
#' It will not need to be called after deterioration and different grades of the elementid appear,
#' as the unit_area will be calculated by the transition from the superior grade condition.
#' As most unit area calculation methods are to use the gifa,
#'  we do this first and then specify the rarer \code{unit_area}
#' calculations or estimation methods. A window or door is assumed to have an area of one 
#' square metre.  
#' There are some building components that lack a method for \code{unit_area} estimation,
#' these may take a unit_area of zero and thus will not contribute to the cost of repairs which
#' could be misleading.
#' @seealso  
#' @export
#' @examples 
#' pds_data <- areafy(blockbuster_pds[1, ])$unit_area  #  The unit_area did not come with the raw data
#' pds_data == blockbuster_pds[1, ]$unit_area  #  The unit_area column was added to the blockbuster_pds using this function at time zero.

areafy <- function(blockbuster_initial_state) {
  
  areafyed <- dplyr::mutate(blockbuster_initial_state,
                     unit_area = dplyr::if_else(element == "Roofs", true = ground_gifa,
                          false = dplyr::if_else(element == "Floors and Stairs" |              element == "Ceilings" |
                                            element == "Internal Walls and Doors" |     element == "Sanitary Services" |
                                            element == "Sanitary Services" |            element == "Mechanical Services" |
                                            element == "Electrical Services" |          element == "Redecorations" |
                                            element == "Fixed Furniture and Fittings" | element == "External Areas",
                                          true = gifa,
                                          false = dplyr::if_else(element == "External Walls, Windows and Doors",
                                                          true = (block_perimeter*height) - windows_doors,
                                                          false = dplyr::if_else(element == "Playing Fields",
                                                                          true = field_area,
                                                                          false = 0
                                                                            )
                                                            )
                                          )
                          )
    ) #  Keep it readable, that's using element level, some we need sub-element detail for method
  areafyed <- dplyr::mutate(areafyed,
                     unit_area = dplyr::if_else(iconv(sub_element, from = "UTF-8", to = "ASCII", sub = "byte") ==
                                                  paste0("Suspended floors ", "<96>", " Structure"),  #  Problem with the weird hyphen
                                                true = gifa - ground_gifa,
                               false = dplyr::if_else(sub_element == "Windows and doors", true = windows_doors,
                                               false = dplyr::if_else(sub_element == "Lifts", true = number_lifts,
                                                               false = dplyr::if_else(sub_element == "Roads and car parks" |
                                                                                 sub_element == "Paths, pedestrian paved areas, play areas" |
                                                                                 sub_element == "Soft Landscaping" |
                                                                                 sub_element == "Mains Services" |
                                                                                 sub_element == "Other walls, fences and barriers, including around tennis courts, MUGAs etc",
                                                                               true = site_area_exc_field - ground_gifa,
                                                                 false = dplyr::if_else(sub_element == "Boundary walls and fences", true = boundary_length,
                                                                                 false = dplyr::if_else(sub_element == "Swimming Pools - Plant" |
                                                                                                   sub_element == "Swimming Pools - Structure",
                                                                                                 true = as.integer(swimming_pool) - 1,  #  Factor, level 1 is No, level 2 is Yes (alphabetical)
                                                                                                   false = unit_area  #  already defined earlier
                                                                                                 )
                                                                   
                                                                 )
                                                               )
                                                 
                                               )
                                                 )
      
    )
    )  #  Here we introduce fixes for some errors in the original methods of unit_area estimation
  areafyed <- dplyr::mutate(areafyed,
                     unit_area = dplyr::if_else(unit_area == 0 &
                                                  iconv(sub_element, from = "UTF-8", to = "ASCII", sub = "byte") ==
                                                  paste0("Suspended floors ", "<96>", " Structure"),  #  Problem with the weird hyphen
                               true = ground_gifa,
                               false = unit_area
      
    )
      
    )
  #  To finish we need to consider the composition e.g. the observation may be only 0.8 composition
  areafyed <- dplyr::mutate(areafyed,
                            unit_area = unit_area*composition)
  #  Explicitly return as_tibble
  return(tibble::as_tibble(areafyed))
}

#' Estimate the unit_area for an observation for a blockbuster tibble with readable code.
#'
#' @param blockbuster_initial_state a blockbuster dataframe or tibble.
#' @param unit_area_methods a string to specify whether "PDS" or "CDC". Currently only "PDS" supported.
#' @return a blockbuster tibble with the unit_area estimated for each row.
#' This should be used to provide the initial estimates of the unit_area
#' of each element-sub-element construction type in the dataframe found in the
#' R script 01_read_and_tidy_data.R. It may be preferred over \code{\link{areafy}} as its easier to read,
#' however as \code{\link[dplyr]{case_when}} is experimental, areafy may be preferred.  
#' It will not need to be called after deterioration and different grades of the elementid appear,
#' as the unit_area will be calculated by the transition from the superior grade condition.
#' As most unit area calculation methods are to use the gifa,
#'  we do this first and then specify the rarer \code{unit_area}
#' calculations or estimation methods. A window or door is assumed to have an area of one 
#' square metre.
#' See the data-raw file blockbuster_unit_quantity_method for method details.
#' The first areafy function used nested ifelse statements and was difficult to read. This version is
#' an attempt to tidy that and make the code more human readable for debugging.
#' @seealso  
#' @export
#' @examples 
#' pds_data <- areafy2(blockbuster_pds[1, ])$unit_area  #  The unit_area did not come with the raw data
#' pds_data == blockbuster_pds[1, ]$unit_area  #  The unit_area column was added to the blockbuster_pds using this function at time zero.

areafy2 <- function(blockbuster_initial_state, unit_area_methods = "PDS") {
  # This function is an improvement on the original areafy which was found to contain some errors
  # This areafy2 will use the dplyr case_when approach to improve ease of reading
  # We should go from the most specific (element, sub_element, const_type) required to assign
  # unit_area calculation method, to the most general (just element required to assign)
  # case_when is still experimental so we may prefer to keep areafy() and fix.
  
  
  # INPUT CHECKS ------------------------------------------------------------
  if (unit_area_methods != "PDS") stop("This function currently only supports unit area estimation methods
                                        from the Property Data Survey.")
  
  message('Checking blockbuster_initial_state is a data.frame or tibble...')
  if (!is.data.frame(blockbuster_initial_state)) stop("blockbuster_initial_state must be a data.frame")
  
  # Check necessary building component columns exist for unit_area method specification
  
  f <- function(variable_to_check) {
    #  DNRY, lots of similar column name checks, let's make a function
    message(paste("Checking blockbuster_initial_state contains a",
                  variable_to_check, "column..."))
    
    if (!variable_to_check %in% colnames(blockbuster_initial_state)) stop(paste("blockbuster_initial_state must contain",
                                                                                variable_to_check, "column..."))
    
  }
  
  variables_to_check <- c("element", "sub_element", "const_type")
  lapply(variables_to_check, f)

  # Check necessary columns exist for unit_area calculation
  variables_to_check <- c("gifa", "ground_gifa", "block_perimeter",
                          "block_perimeter", "height", "windows_doors",
                          "site_area_exc_field", "boundary_length",
                          "field_area", "swimming_pool")
  lapply(variables_to_check, f)


# INSPECT BUILDING COMPONENT THEN CALCULATE UNIT AREA ---------------------------------------------------------------

  areafyed <- dplyr::mutate(blockbuster_initial_state,
                            unit_area = case_when(
                              .$element == "Mazda RX4" & .$sub_element == "Mazda RX4 Wag" & .$const_type  ~ "Mazda",
                              TRUE ~ gifa
                            )
                            )
  
  

# CONSIDER COMPOSITION ----------------------------------------------------



  
  return(areafyed)
  
}

