
# BLOCKBUSTER -------------------------------------------------------------

#' The deterioration of a \code{blockbuster_tibble} through time.
#' 
#' High level modelling of a blockbuster_tibble through time that considers
#' repairs and rebuilding interventions on the condition of the modelled building components.
#' It is composed of many smaller functions that deteriorate, rebuild and repair the \code{blockbuster_tibble}.

#' Outputs a list of blockbuster_tibbles with each tibble containing
#' the \code{unit_area} and condition of the \code{element sub_element constr_type} 
#' combination at a given \code{timestep} while also duplicating all
#'  other variables and values from the input tibble. This is not as expensive as it sounds
#'  because modifying a list no longer makes a deep copy; modifying a list efficiently reuses
#'  existing vectors (R >= 3.1.0).  
#' After each \code{timestep} is simulated the \code{unit_area} are aggregated by identifying features, e.g.
#' \code{buildingid}, \code{elementid} and \code{grade}. 
#' Then repair cost estimates are calculated using \code{\link{blockcoster_lookup}}
#'  to find the correct constant which
#' is multiplied by the \code{unit_area} to give the expected repair \code{cost} (the initial
#' \code{unit_area} at time zero is estimated using \code{\link{areafy2}}). Grade E building
#' components have a repair cost of 5 per cent on Grade D, see
#' \code{\link{blockbuster_pds_repair_costs}} for repair cost details.
#'  A seperate variable \code{block_rebuild_cost} is created to help quantify Grade E cost.
#'  This cost value applies to the estimated rebuild
#' cost of the whole block (not just that one building component)
#'  based on the argument \code{rebuild_cost_rate} (£ per m^2). For the moment we simplify
#'  by adding a costing for E grade to the \code{\link{blockcoster_lookup}}, this simply
#'  takes the D grade costing and adds 5 per cent to it 
#'  (based on expert domain knowledge of @@adam.bray@education.gov.uk).
#'
#' @param blockbuster_tibble a blockbuster dataframe or tibble. 
#' @param forecast_horizon an integer for the number of timesteps to model deterioration over.
#' @param rebuild_cost_rate a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @param rebuild_monies a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @param repair_monies a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @return An object of class \code{blockbuster_list};
#'  list of n plus one tibbles (where n is the \code{forecast_horizon}). 
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
  stopifnot(forecast_horizon > 0, forecast_horizon < 31)
  
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
  
  #  Sensible rebuild_monies
  #  Check the rebuild monies is either zero or greater than the cheapest
  #  block to rebuild, for the year with the lowest non-zero rebuild_monies
  
#   rebuild_monies_check <- ifelse((test = (rebuild_monies != 0) &&
#                                     rebuild_monies <= 
#                                     (blockbuster_tibble$gifa *
#                                     rebuild_cost_rate)),
#                                  yes = 1,
#                                  no = 0)
#   
#   if (sum(rebuild_monies_check) > 0) {
#     stop("Error: reconsider your rebuild monies; at least one of your years 
# has insufficient funds
#          to rebuild the cheapest block in your blockbuster_tibble!")
#     
#   }
  
  #  Create placeholder
  #  Create placeholder variables for cost and block_rebuild_cost
  blockbusted <- blockbuster_tibble
  blockbusted <- dplyr::slice(blockbusted, -(1:n()))  #  keep attributes, drop values
  #  Rep this and create a list of empty blockbuster tibbles
  blockbusted <- rep(list(blockbusted), forecast_horizon + 1)
  
  #  Provide initial tibble at timestep zero
  blockbusted[[1]] <- blockbuster_tibble %>%
    #  Need to cache variable before for loop, only do this once to speed up code
    #  This was originally carried out in det_what_tm()
    #  Should also do for the blockcoster_lookup() also
    dplyr::mutate_(concated = ~gsub(pattern = "[^[:alnum:] ]",
                                    replacement = "",
                                    paste(element, sub_element, const_type,
                                     sep = "")))
  
  for (i in 1:forecast_horizon) {
    #  the input tibble is at timestep zero, not included in the output list of tibbles
    #  Need to zero the cost variables, as it will be incorrect and misleading for non zero timestep
    x <- dplyr::mutate_(blockbust(blockbusted[[i]]),
                       cost = 0,  #  get repair costs constant, causes failure if done before aggregate
                       cost_sum = 0)  #  having this here avoids aggregation errors, likely sub-optimal 
    #  composition should also be set to zero as it is now redundant given unit_area estimates
    #  perhaps dropping these at the end rather than changing them each run will be faster
    
    #  BLOCKCOSTING ----

    #  Sum unit_area over each row, keep all other variables
    #  then mutate the cost, needs to happen here after aggregation but before rebuild / maintenance
    blockbusted[[i + 1]] <- dplyr::mutate_(tibble::as_tibble(stats::aggregate(unit_area ~ .,  #  could try cbind(y1, y2) ~., here...
                                                                             data = x, FUN = sum)),
                                          cost = ~(unit_area * blockcoster_lookup(concated = concated,
                                                                                  grade = grade)),
                                          block_rebuild_cost = ~(rebuild_cost_rate[i] * gifa))
                                          # ,
                                          # cost_decommissioned = ifelse(grade == "E",
                                          #                              yes = rebuild_cost_rate[i] * unit_area,
                                          #                              0)  #  method to quantify E grade component cost, as it can't be repaired
    # We ignore non-critical building elements for now
    
    #  REBUILDING ---- in blockbuster_tibble, out rebuilt blockbuster_tibble
    blockbusted[[i + 1]] <- rebuild(blockbusted[[i + 1]], rebuild_monies = rebuild_monies[i])  
    # don't need rebuild_cost_rate[i] as block_rebuild_cost calculated above in costing
    #  REPAIRS ----
    blockbusted[[i + 1]] <- repair(blockbusted[[i + 1]], repair_monies = repair_monies[i])
  }
  # CUSTOM CLASS FOR GENERIC METHODS
  class(blockbusted) <- c("blockbuster_list", "list")
  # ATTRIBUTES DETAIL THE SPENDING PROFILES
  attr(blockbusted, "rebuild_cost_rate") <- rebuild_cost_rate
  attr(blockbusted, "rebuild_monies") <- rebuild_monies
  attr(blockbusted, "repair_monies") <- repair_monies
  
  # RETURN
  return(blockbusted)

}
