
# BLOCKBUSTER -------------------------------------------------------------

# High level modelling of a blockbuster_tibble through time that considers
# maintenance and rebuilding interventions on the condition of the modelled building components.
# It is composed of many smaller functions.

#' The deterioration of more than one blockbuster rows through time.
#'
#' @param blockbuster_tibble a blockbuster dataframe or tibble. 
#' @param forecast_horizon an integer for the number of timesteps to model deterioration over.
#' @return A list of n tibbles (where n is based on the \code{forecast_horizon}),
#' with each tibble containing
#' the \code{unit_area} and condition of the \code{element sub_element constr_type} 
#' combination at a given timestep while also duplicating all
#'  other variables and values from the input tibble.
#' After each timestep is simulated the rows are aggregated by \code{elementid} and \code{grade}. 
#' Then repair cost estimates are calculated using \code{\link{blockcoster_lookup}} to find the correct constant which
#' is multiplied by the \code{unit_area} to give the expected repair \code{cost}.
#' @importFrom stats aggregate
#' @export
#' @examples 
#'
#' two_year_later <- blockbuster(dplyr::filter(blockbuster_pds, buildingid == 127617), 2)
#' 
blockbuster <- function(blockbuster_tibble, forecast_horizon) {
  
  #  Sensible forecast horizon
  stopifnot(forecast_horizon > 0, forecast_horizon < 51)
  
  #  Create placeholder
  blockbusted <- blockbuster_tibble
  blockbusted <- dplyr::slice(blockbusted, -(1:n()))  #  keep attributes, drop values
  #  Rep this and create a list of empty blockbuster tibbles
  blockbusted <- rep(list(blockbusted), forecast_horizon + 1)
  
  #  Provide initial tibble at timestep zero
  blockbusted[[1]] <- blockbuster_tibble
  
  for (i in 1:forecast_horizon) {
    #  the input tibble is at timestep zero, not included in the output list of tibbles
    #  Need to remove the cost variables, as it will be incorrect and misleading for non zero timestep
    x <- dplyr::mutate(blockbust(blockbusted[[i]]),
                       cost = 0,  #  get repair costs constant, causes failure if done before aggregate
                       cost_sum = 0)  #  composition should also be set to zero as it is now redundant given unit_area estimates
    # perhaps dropping these at the end rather than changing them each run will be faster
    
    #  Sum unit_area over each row, keep all other variables
    #  then mutate the cost, needs to happen here after aggregation but before rebuild / maintenance
    #  Note if E grade or decommissioned it will return NA for cost.
    blockbusted[[i + 1]] <- dplyr::mutate(tibble::as_tibble(stats::aggregate(unit_area ~., data = x, FUN = sum)),
                                          cost = unit_area * blockcoster_lookup(element, sub_element, const_type, grade))
    
  }
  # Aggregate over each list to make tidy data, avoid repeated rows for elementid and grade
  return(blockbusted)
  # tidyr::nest(data, ..., .key = data)
}
