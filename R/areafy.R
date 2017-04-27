
# AREAFY FUNCTION FAMILY --------------------------------------------------

# These functions take the initial input of the condition data produced
# by the Property Data Survey and, for each row and associated element-sub-element-con-type
# estimate the unit_area (an approximation or sort of area of each observation).
# In effect we create a new variable called unit_area. This is critical for the later
# deterioration stages, as you need an approximation of how much of something there
# is, if you want to deteriorate it using a discrete-time markov chain approach. 

#' Estimate the unit_area for an observation for a blockbuster tibble with readable code.
#' 
#' This should be used to provide the initial estimates of the \code{unit_area}
#' of each element-sub-element construction type (aka building component) in
#'  the dataframe found in the
#' R script 01_read_and_tidy_data.R. Input tibble is checked by default can be skipped.
#' 
#' It may be preferred over the deprecated \code{areafy} as it's easier to read,
#' however as \code{\link[dplyr]{case_when}} is experimental, areafy may be preferred.  
#' It will not need to be called after deterioration and different grades of the elementid appear,
#' as the \code{unit_area} will be calculated by the transition from the superior grade condition.
#' As most unit area calculation methods are to use the gifa,
#'  we do this first and then specify the rarer \code{unit_area}
#' calculations or estimation methods. A window or door is assumed to have an area of one 
#' square metre. See the data-raw file blockbuster_unit_quantity_method.csv for method details.
#' The first areafy function used nested \code{ifelse()} statements and was difficult to read.
#'  This version is
#' an attempt to tidy that and make the code more human readable for debugging. Set the 
#' \code{input_checks} to FALSE if it has already been checked for speed.
#'
#' @param blockbuster_initial_state a blockbuster dataframe or tibble without \code{unit_area}.
#' @param unit_area_methods a string to specify whether "PDS" or "CDC". Currently only "PDS" supported.
#' @param input_checks logical to specify whether to check inputs or not for speed in blockbuster.
#' @return a blockbuster tibble with the unit_area estimated for each row.
#' @importFrom dplyr %>%
#' @export
#' @seealso \url{http://www.machinegurning.com/rstats/case_when/}
#' @examples 
#' pds_data <- areafy2(blockbuster_pds[1, ])$unit_area 
#' pds_data == blockbuster_pds[1, ]$unit_area  

areafy2 <- function(blockbuster_initial_state, unit_area_methods = "PDS", input_checks = TRUE) {
  # This function is an improvement on the original areafy which was found to contain some errors.
  # This areafy2 will use the dplyr case_when approach to improve ease of reading.
  # We should go from the most specific (element, sub_element, const_type) required to assign
  # unit_area calculation method, to the most general (just element required to assign)
  # case_when is still experimental so we may prefer to keep areafy() and fix.
  # The unit_area methods are based on expert opinion used to help quantify cost of repairs in PDS buildings.
  
  if (input_checks == TRUE) {
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
                          "field_area", "swimming_pool", "composition",
                          "number_lifts")
  lapply(variables_to_check, f)

} #  if input_checks = FALSE skip to here
# CALL DPLYR --------------------------------------------------------------
# require(tidyverse)  
  
# INSPECT BUILDING COMPONENT THEN CALCULATE UNIT AREA ---------------------------------------------------------------

  areafyed <- blockbuster_initial_state %>%
    dplyr::mutate_(unit_area = ~dplyr::case_when(
      
      # External Areas and five distinctions
      .$sub_element == "Boundary walls and fences" ~ .$boundary_length,
      .$sub_element == "Other walls, fences and barriers, including around tennis courts, MUGAs etc" ~ .$site_area_exc_field - .$ground_gifa,
      .$sub_element == "Swimming Pools - Plant" | .$sub_element == "Swimming Pools - Structure" ~ as.double(.$swimming_pool),
      .$sub_element == "Drainage - Other" | .$sub_element == "Drainage - Treatment plant" ~ .$gifa ,
      .$element == "External Areas" ~ .$site_area_exc_field - .$ground_gifa,
        
      # Floors and Stairs, three distinctions
      
      #  match on alpha numeric on strings with dodgy punctuation e.g. weird hyphen en-dash Alt + 0150
      #  This does exist but it's not being detected due to different encodings?
      #  y[grep("^Suspended floors", y$sub_element), ]  #  possible fix
      iconv(.$sub_element, from = "UTF-8", to = "ASCII", sub = "byte") ==
        paste0("Suspended floors ", "<96>", " Structure") ~ .$gifa - .$ground_gifa,
      
      #  this incorporates the suspended floors (matching fails) and ground / bearing hollow floors
      .$element == "Floors and Stairs" & .$const_type == "Generally" ~ .$ground_gifa,
      
      #  all other floors and stairs
      .$element == "Floors and Stairs" ~ .$gifa,
      
      #  Windows and doors two distinctions
      .$element == "External Walls, Windows and Doors" & .$sub_element == "Windows and doors" ~ .$windows_doors,
      .$element == "External Walls, Windows and Doors" ~ (.$block_perimeter * .$height) - .$windows_doors,
      
      #  Electrical Services have two distinctions
      .$sub_element == "Lifts" ~ .$number_lifts, 
      .$element == "Electrical Services" ~ .$gifa,
      
      #  Roofs are all estimated as being ground floor gifa
      .$element == "Roofs" ~ .$ground_gifa,
      
      #  Code self explanatory
      
      .$element == "Playing Fields" ~ .$field_area,
      
      .$element == "Redecorations" ~ .$gifa, 
      
      .$element == "Fixed Furniture and Fittings" ~ .$gifa,
      
      .$element == "Ceilings" ~ .$gifa,
      
      .$element == "Internal Walls and Doors" ~ .$gifa,
      
      .$element == "Sanitary Services" ~ .$gifa,
      
      .$element == "Mechanical Services" ~ .$gifa,
      
      TRUE ~ 0  #  if no method default to zero
      )
      )
  

# TESTER ------------------------------------------------------------------
#  sum(areafy2(y)$unit_area == 0)
#  filter(areafy2(y), element == "Fixed Furniture and Fittings")[, c("unit_area", "gifa", "composition")]

# CONSIDER COMPOSITION ----------------------------------------------------
  areafyed <- areafyed %>%
    dplyr::mutate_(unit_area = ~(unit_area * composition))  #  composition relates to const_type not grade

  return(areafyed)
  
}


