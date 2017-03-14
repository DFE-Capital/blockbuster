
# BLOCKBUSTER -------------------------------------------------------------

#' The deterioration of a blockbuster_tibble through time.
#' 
#' High level modelling of a blockbuster_tibble through time that considers
#' maintenance and rebuilding interventions on the condition of the modelled building components.
#' It is composed of many smaller functions.

#' Outputs a list of blockbuster_tibbles with each tibble containing
#' the \code{unit_area} and condition of the \code{element sub_element constr_type} 
#' combination at a given timestep while also duplicating all
#'  other variables and values from the input tibble. This is not as expensive as it sounds
#'  because modifying a list no longer makes a deep copy; modifying a list efficiently reuses
#'  existing vectors (R >= 3.1.0).  
#' After each timestep is simulated the \code{unit_area} are aggregated by identifying features, e.g.
#' \code{buildingid}, \code{elementid} and \code{grade}. 
#' Then repair cost estimates are calculated using \code{\link{blockcoster_lookup}} to find the correct constant which
#' is multiplied by the \code{unit_area} to give the expected repair \code{cost} (the initial
#' \code{unit_area} at time zero is estimated using \code{\link{areafy2}}). Grade E building
#' components don't have a repair cost (they can't be repaired and must be rebuilt) thus
#' a seperate variable \code{block_rebuild_cost} is needed to quantify their cost.
#'  This cost value applies to the estimated rebuild
#' cost of the whole block (not just that one building component)
#'  based on the argument \code{rebuild_cost_rate} (£ per m^2). The  \code{rebuild_cost_rate}
#'  is also used to quantify the cost of decommissioned \code{unit_area}, creating the new variable
#'  \code{cost_decommissioned} (zero cost for condition grades not E).  This ideas is being developed
#'  and may be introduced into later versions of blockbuster. For the moment we simplify
#'  by adding a costing for E grade to the \code{\link{blockcoster_lookup}}, this simply
#'  takes the D grade costing and adds 5% to it (based on expert domain knowledge of AB).
#'
#' @param blockbuster_tibble a blockbuster dataframe or tibble. 
#' @param forecast_horizon an integer for the number of timesteps to model deterioration over.
#' @param rebuild_cost_rate a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @param rebuild_monies a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @param repair_monies a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @return A list of n plus one tibbles (where n is the \code{forecast_horizon}). 
#' The first tibble is the initial \code{blockbuster_tibble}.
#' 
#' @importFrom stats aggregate
#' @export
#' @examples 
#' 
#' two_yrs_counterfactual <- blockbuster(dplyr::filter(blockbuster_pds,
#'  buildingid == 127617), 2)
#' 
blockbuster <- function(blockbuster_tibble, forecast_horizon,
                        rebuild_monies = 0, repair_monies = 0, rebuild_cost_rate = 1274) {
  
  
  #  Sensible forecast horizon
  stopifnot(forecast_horizon > 0, forecast_horizon < 21)
  
  #  Create appropriate vector for rebuild_cost_rate if constant cost throughout forecast
  stopifnot(is.numeric(rebuild_cost_rate))
  
  if (length(rebuild_cost_rate) == 1) {
    rebuild_cost_rate <- rep_len(rebuild_cost_rate,
                                 length.out = forecast_horizon)
  }
  
  stopifnot(length(rebuild_cost_rate) == forecast_horizon)
  
  #  Create appropriate vector for rebuild_monies if constant investment throughout forecast
  stopifnot(is.numeric(rebuild_monies))
  
  if (length(rebuild_monies) == 1) {
    rebuild_monies <- rep_len(rebuild_monies,
                                 length.out = forecast_horizon)
  }
  
  stopifnot(length(rebuild_monies) == forecast_horizon)
  
  #  Create appropriate vector for repair_monies if constant investment throughout forecast
  stopifnot(is.numeric(repair_monies))
  
  if (length(repair_monies) == 1) {
    repair_monies <- rep_len(repair_monies,
                              length.out = forecast_horizon)
  }
  
  stopifnot(length(repair_monies) == forecast_horizon)
  
  #  Create placeholder
  #  Create placeholder variables for cost and block_rebuild_cost
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
                       cost_sum = 0,
                       cost_decommissioned = 0)  #  having this here avoids aggregation errors, likely sub-optimal 
    #  composition should also be set to zero as it is now redundant given unit_area estimates
    #  perhaps dropping these at the end rather than changing them each run will be faster
    
    #  BLOCKCOSTING ----

    #  Sum unit_area over each row, keep all other variables
    #  then mutate the cost, needs to happen here after aggregation but before rebuild / maintenance
    #  Note if E grade / decommissioned it will return zero for cost but non-zero for cost_decommissioned
    blockbusted[[i + 1]] <- dplyr::mutate(tibble::as_tibble(stats::aggregate(unit_area ~ .,  #  could try cbind(y1, y2) ~., here...
                                                                             data = x, FUN = sum)),
                                          cost = unit_area * blockcoster_lookup(element, sub_element, const_type, grade),
                                          block_rebuild_cost = rebuild_cost_rate[i] * gifa)
                                          # ,
                                          # cost_decommissioned = ifelse(grade == "E",
                                          #                              yes = rebuild_cost_rate[i] * unit_area,
                                          #                              0)  #  method to quantify E grade component cost, as it can't be repaired
    # We ignore non-critical building elements
    
    #  REBUILDING ---- in blockbuster_tibble, out rebuilt blockbuster_tibble
    # blockbusted[[i + 1]] <- rebuild(blockbusted[[i + 1]], rebuild_monies[i], rebuild_cost_rate[i])
    #  REPAIRS ----
    # blockbusted[[i + 1]] <- repair(blockbusted[[i + 1]], repair_monies)
  }
  # GATHER and TIDY ----
  
  return(blockbusted)
  # tidyr::nest(data, ..., .key = data)
}
