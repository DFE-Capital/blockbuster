# REPAIR -----------------------------------------------------------------
#'
#' A helper function that updates the \code{repair_status} of building components in a block.
#' 
#' Outputs a block_tibble after spending \code{per_block_spend} on one block. Used in the 
#' \code{\link{repair}} function. A new binary variable is created of the \code{repair_status} 
#' of each building component within the block. The building components are sorted by 
#' descending grade and then descending cost in the \code{\link{repair}} function prior to 
#' being passed to this helper function.
#'
#' @param block_tibble a block dataframe or tibble with a \code{costs}variable.
#' @param per_block_spend a scalar calculated from \code{repair_monies} / number of blocks. 
#' @return A \code{block_tibble} that has had building components repaired or not indicated
#' by the new variable \code{repair_status}.
#' 
#' @examples 
#' 
#' example <- what_needs_repair_within_block(tibble::tibble(
#' cost = seq(from = 0, to = 10000, by = 500)),
#'  1e4)
what_needs_repair_within_block <- function(block_tibble, per_block_spend) {
  
  #  money counter
  money2spend <- per_block_spend
  #  add placeholder column, defaultto no rebuild
  the_block <- dplyr::mutate_(block_tibble, repair_status = 0)
  
  if (max(the_block$cost) == 0) {  #  if new build block or all grade A
    
    return(the_block)  #  i.e. if there's nothing to repair!
    
  } else {
    
    repair_min_not_zero <- min(the_block$cost[the_block$cost > 0]) #  cheapest repair in block
    # print(repair_min_not_zero)  #  debugging
    
    for (i in 1:nrow(the_block)) {
      
      if (the_block[i, "cost"] == 0) {  #  ignore zero costs, don't repair
        next
      }
      
      if (the_block[i, "cost"] < money2spend) {  #  if we can afford to repair
        the_block[i, "repair_status"] <- 1  #  repair it
        money2spend <- money2spend - the_block[i, "cost"]  #  and take from budget
        # print(money2spend)
        if (money2spend < repair_min_not_zero) break
      }
    } 
    
  }
  return(the_block)
}

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
    

    #  REPAIR DECISION MAKING ----
    repairing <- blockbuster_tibble %>%
      dplyr::arrange_(~ buildingid, ~ dplyr::desc(grade), ~ dplyr::desc(cost))
    
    #  MONEY PER BLOCK ----
    number_of_blocks <-  nrow(dplyr::distinct_(repairing, ~ buildingid,
                                        .keep_all = FALSE))
    per_block_spend <- repair_monies / number_of_blocks
    
    #  GO THROUGH EACH BLOCK MARKING "TO REPAIR" UNTIL MONEY RUNS OUT
    #  define helper function to identify repair_status
    
    # input list of block_tibbles
    # output list of block_tibbles with repair_status added

    
    # Split repairing into pieces by buildingid, repair_status update
    # what_needs_repair_within_block can be applied to this
    by_block_list <- vector(mode = "list", length = number_of_blocks)  #  pre-allocate
    by_block_list <- repairing %>%
      split(.$buildingid)
    

    # ERROR HERE
    
    by_block <- lapply(names(by_block_list),  #  go through each block
                       blockbuster::what_needs_repair_within_block(
                         per_block_spend = per_block_spend))  #  and use internal function
    
    # mods <- by_block %>%
    #   map(~ lm(mpg ~ wt, data = .))
    # map2(mods, by_cyl, predict)
    
    # tibble_to_repair <- repairing %>%
    #   dplyr::filter(repair_status == 1)
    
    #  Getting repaired need change grade to A and reareafy unit_area
    #  Remove repair status column? or not for speed
    
    
    #  OUTPUT ----
    output <- repairing
  }
  return(by_block)
}