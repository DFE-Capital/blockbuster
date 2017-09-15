
# BLOCKBUSTER SIM  -------------------------------------------------------------

#' @title RAM efficient version of blockbuster designed to scale.
#' 
#' @description Sacrifices speed somewhat for a reduction in
#'  RAM requirements by writing
#' intermediary blockbuster tibbles for each timestep
#'  to disk when no longer
#' required.
#' 
#' See the help for \code{\link{blockbuster}} for more details.
#'
#' @param blockbuster_tibble a blockbuster dataframe or tibble. 
#' @param forecast_horizon an integer for the number of timesteps
#'  to model deterioration over.
#' @param rebuild_cost_rate a numeric vector of length equal to the
#'  \code{forecast_horizon} or one.
#' @param rebuild_monies a numeric vector of length equal to the
#'  \code{forecast_horizon} or one.
#' @param repair_monies a numeric vector of length equal to the
#'  \code{forecast_horizon} or one.
#' @param output_dir a character string of where to write
#'  each timestep's tibble to.
#' @param output_filename a character string of the desired
#'  filename. Default is 
#' system date and the timestep.
#' @param plenty_of_ram the most conservative approach
#'  is setting this to FALSE.
#' @return An object of class \code{blockbuster_list} 
#' of n plus one tibbles (where n is the \code{forecast_horizon}).
#' 
#' @importFrom stats aggregate
#' @importFrom readr read_rds
#' @importFrom readr write_rds
#' @export
#' @examples 
#' 
#' one_yr_counterfactual <- blockbuster_sim(dplyr::filter(blockbuster_pds,
#'  buildingid == 127617), 2,
#'   output_dir = "./", output_filename = "example")
#' 
blockbuster_sim <- function(blockbuster_tibble, forecast_horizon,
                        rebuild_monies = 0, repair_monies = 0,
                        rebuild_cost_rate = 1274,
                        output_dir = "./output/",
                        output_filename = paste0(Sys.Date(),
                                                 "-blockbuster_sim"),
                        plenty_of_ram = TRUE) {
  
  
  #  Sensible forecast horizon
  stopifnot(forecast_horizon > 0, forecast_horizon < 31)
  
  #  Character string for output_dir
  stopifnot(is.character(output_dir))
  
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
  #  Not working as intended yet, needs fix
  
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
  blockbusted[[1]] <- blockbuster_tibble 
  
  for (i in 1:forecast_horizon) {
    #  the input tibble is at timestep zero
    #  Need to zero the cost variables to allow aggregation then recalculate afterwards
    #  This is where we experience the RAM issue, in blockbuster prior to aggregation
    
    x <- blockbust(blockbusted[[i]])
    x$cost <- 0L
    x$cost_sum <- 0L
    #  get repair costs constant, causes failure if done before aggregate
    #  having this here avoids aggregation errors, likely sub-optimal 
    #  composition should also be set to zero as it is now redundant given unit_area estimates
    #  perhaps dropping these at the end rather than changing them each run will be faster
    
    #  BLOCKCOSTING ----
    
    #  Sum unit_area over each row, keep all other variables and aggregate
    #  then mutate the cost, needs to happen here after aggregation but before rebuild / maintenance
    blockbusted[[i + 1]] <- dplyr::mutate_(tibble::as_tibble(stats::aggregate(unit_area ~ .,  
                                                                              data = x, FUN = sum)),
                                           cost = ~(unit_area * blockcoster_lookup(the_elementid = elementid,
                                                                                   the_grade = grade)),
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
    
    #  RAM ISSUES, 
    #  WRITE i (previous) YEAR TO DISC THEN DELETE, AS NO LONGER NEEDED in RAM
    readr::write_rds(blockbusted[[i]], path = paste0(output_dir, output_filename,
                                                     "_timestep_", 
                                                     as.character(unique(blockbusted[[i]]$timestep)),
                                                     ".rds")
                     )
    #  SAVE LAST YEAR TO DISC ALSO
    if (i == forecast_horizon) {

    readr::write_rds(blockbusted[[i + 1]], path = paste0(output_dir, output_filename,
                                                     "_timestep_", 
                                                     as.character(unique(blockbusted[[i + 1]]$timestep)),
                                                     ".rds"))
      }
   #  WIPE if no longer needed
    #  fill with variables but no data
    if (i > 1) {
      blockbusted[[i - 1]] <- dplyr::slice(blockbusted[[i - 1]],
                                           -(1:n()))
    }

  }
  #  WITH COMPUTATION COMPLETE READ IN DATA (as long as not too big)
  #  the last 2 years will contain data already, see above
  #  we an choose to read in the .rds from output folder into the list or leave as is
  #  with most of the blockbuster list empty, except the last two years.
  if (plenty_of_ram == TRUE) {
    
    for (i in 0:(forecast_horizon - 1)) {
      blockbusted[[i + 1]] <- readr::read_rds(path = paste0(output_dir, output_filename,
                                                            "_timestep_", 
                                                            as.character(i),
                                                            ".rds"))
    }
    
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
