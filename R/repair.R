# REPAIR -----------------------------------------------------------------

#' An internal function that determines repairs intervention effect on a blockbuster_tibble.
#' 
#' A given amount of money (\code{repair_monies}) is invested in repairing
#'  the \code{blockbuster_tibble}, passed as an arguement to the function. 
#'  The \code{repair_monies} is distributed evenly to each \code{buildingid} 
#'  included in the \code{blockbuster_tibble}. Grade D, C and B condition 
#'  building components are repaired in that order.
#' 
#' Outputs a blockbuster_tibble after spending \code{repair_monies} on a finite
#' number of blocks. Repairing a block attempts to convert some 
#' (depending on \code{repair_monies} available) of it's building components
#' back to grade A, making it Excellent condition. 
#'
#' @param blockbuster_tibble a blockbuster dataframe or tibble.
#' @param repair_monies a vector of length one. 
#' @return A \code{blockbuster_tibble} that has had building components repaired
#'  (or not if \code{repair_monies} are 0).
#' 
#' @importFrom dplyr %>%
#' @export
#' @examples 
#' 
#' quick_fix <- blockbuster(dplyr::filter(blockbuster_pds,
#'   buildingid == 4382 | buildingid == 4472 | buildingid == 4487), 
#'   forecast_horizon = 2, rebuild_monies = 0, repair_monies = 2.5e6)
#' 
repair <- function(blockbuster_tibble, repair_monies) {
  
  if (repair_monies <= 0) {
    
    return(blockbuster_tibble)  #  save time, only repair if there's money
    
  } else {
    
    #  MONEY PER BLOCK ----
    per_block_spend <- repair_monies / nrow(blockbuster_tibble)
    
    #  REPAIR DECISION MAKING ----
    repairing <- blockbuster_tibble %>%
      dplyr::arrange_(~ buildingid, ~ dplyr::desc(grade), ~ dplyr::desc(cost))
    
    #  GO THROUGH EACH BLOCK MARKING "TO REPAIR" UNTIL MONEY RUNS OUT
    #  define function to identify repair_status
    what_needs_repair_within_block <-
    
    # Split repairing into pieces by buildingid, repair_status update
    by_block_list <- repairing %>%
      split(.$buildingid)
    
    by_block <- lapply(names(by_block_list),  #  go through each block
                       function(x) by_block_list[[x]])  #  and use internal function
    
    # mods <- by_block %>%
    #   map(~ lm(mpg ~ wt, data = .))
    # map2(mods, by_cyl, predict)
    
    # tibble_to_repair <- repairing %>%
    #   dplyr::filter(repair_status == 1)
    
    #  Getting repaired need change grade to A and reareafy unit_area
    
    
    #  OUTPUT ----
    output <- repairing
  }
  return(by_block)
}