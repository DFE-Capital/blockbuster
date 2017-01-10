
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
#' It will also be called after deterioration and different grades of the elementid appear.
#' As most methods are to use the gifa, we do this first and then specify the rarer \code{unit_area}
#' calculations or estimation methods. A window or door is assumed to have an area of one 
#' square metre.
#' @seealso \link{}
#' @export
#' @examples 
#' pds_data <- areafy(blockbuster_pds)

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

