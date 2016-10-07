
# AREAFY FUNCTION FAMILY --------------------------------------------------

# These functions take the initial input of the condition data produced
# by the Property Data Survey and, for each row and associated element-sub-element-con-type
# estimate the unit_area (an approximation or sort of area of each observation).
# In effect we create a new variable called unit_area. This is critical for the later
# deterioration stages, as you need an approximation of how much of something there
# is, if you want to deteriorate it using a discrete-time markov chain approach.

#' Estimate the unit_area for an observation for a Property Data Survey tibble.
#'
#' @param blockbuster_initial_state
#' @return a blockbuster tibble with the unit_area estimated for each row.
#' This should be used to provide the initial estimates of the unit_area
#' of each element-sub-element construction type in the dataframe.
#' It will also be called after deterioration and different grades of the elementid appear.
#' As most methods are to use the gifa, we do this first and then specify the rarer \code{unit_area}
#' calculations or estimation methods. A window or door is assumed to have an area of one 
#' square metre.
#' @seealso \link{/data-raw/blockbuster_unit_quantity_methods.csv}
#' @export
#' @examples

areafy <- function(blockbuster_initial_state) {
  areafyed <- blockbuster_initial_state %>%
    mutate(unit_area = if_else(element == "Roofs", true = ground_gifa,
                          false = if_else(element == "Floors and Stairs" |              element == "Ceilings" |
                                            element == "Internal Walls and Doors" |     element == "Sanitary Services" |
                                            element == "Sanitary Services" |            element == "Mechanical Services" |
                                            element == "Electrical Services" |          element == "Redecorations" |
                                            element == "Fixed Furniture and Fittings" | element == "External Areas",
                                          true = gifa,
                                          false = if_else(element == "External Walls" | element == "Windows and Doors",
                                                          true = (block_perimeter*height) - windows_doors,
                                                          false = if_else(element == "Playing Fields",
                                                                          true = field_area,
                                                                          false = 0
                                                                            )
                                                            )
                                          )
                          )
    ) #%>%  #  Keep it readable, that's using element level, some we need sub-element detail for method
    
  invisible(as_tibble(areafyed))
}

