
# REBUILD -----------------------------------------------------------------

#' An internal function that determines rebuilding intervention effect on a blockbuster_tibble.
#' 
#' A given amount of money (\code{rebuild_monies}) is invested in rebuilding
#'  the \code{blockbuster_tibble}, passed as an arguement to the function. 
#'  This is dependent on the \code{block_rebuild_cost} and the \code{rebuild_monies}.
#' 
#' Outputs a blockbuster_tibble after spending \code{rebuild_monies} on a finite
#' number of blocks. Rebuilding a block converts all it's building components
#' back to grade N, making it as new. The rebuilding decision making is based on
#' a summarised sorted list of blocks, sorted by the ratio of the total cost of repairs
#' divided by the rebuild cost of the block. As this ratio approaches one it suggests the
#' cost of repairs are equivalent to building a new block! Using the available funding
#' or \code{rebuild_monies} we systematically work through and rebuild each block based
#' on our sorted list until all the money is used up. If a block is to expensive to rebuild,
#' we move on and attempt to rebuild the next one. If our \code{rebuild_monies} is less than
#' the cheapest block to repair we stop trying to rebuild.
#'
#' @param blockbuster_tibble a blockbuster dataframe or tibble. 
#' @param rebuild_cost_rate a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @return A \code{blockbuster_tibble} that has had blocks rebuilt (or not if \code{rebuild_monies} are 0).
#' 
#' @importFrom dplyr %>%
#' @export
#' @examples 
#' 
#' rebuild_two_blocks <- rebuild(dplyr::filter(blockbuster_pds,
#'  buildingid == 4382 | buildingid == 4472), rebuild_monies = 5e6)
#' 
rebuild <- function(blockbuster_tibble, rebuild_cost_rate, rebuild_monies) {
  
  if (rebuild_monies <= 0) {
    
    return(blockbuster_tibble)  #  save time, only rebuild if there's money
    
  } else {
    
    #  REBUILD DECISION MAKING ----
    rebuilding <- blockbuster_tibble %>%  #  prepare for rebuild if there's money
      dplyr::group_by(buildingid) %>%
      dplyr::summarise(cost_sum = sum(cost),
                       block_rebuild_cost = max(block_rebuild_cost),
                       cost_to_rebuild_ratio = cost_sum / block_rebuild_cost) %>%
      dplyr::arrange(desc(cost_to_rebuild_ratio))
    
  }
  
  #  Order to rebuild blocks that need it most
  #  rebuilding_priority <- rebuilding$buildingid
  #  Stopping criteria for rebuild
  cheapest_rebuild <- min(rebuilding$block_rebuild_cost)
  max_i <- nrow(rebuilding) + 1
  #  Money to spend
  money_leftover <- rebuild_monies
  
  #  INITIALISE VARIABLE
  to_be_rebuilt <- integer(length = nrow(rebuilding))
  #  Lopping with while
  #  We need to go through this loop until the rebuild money runs out.
  #  Each time we go round the loop or rebuild a block, we check to see
  #  if we have any money left to carry on rebuilding
  #  We use the break to stop if we have inspected all the buildings in our rebuilding object
  #  USE SORTED BUILDING ID VECTOR & WHILE To DETERMINE REBUILD STATUS
  i <- 1  #  iteration
  while (money_leftover > cheapest_rebuild) {
    if (rebuilding[i, "block_rebuild_cost"] <= money_leftover) { 
      
      to_be_rebuilt[i] <- rebuilding[[i, "buildingid"]]  #  we add to our to_be_rebuilt list
      money_leftover <- money_leftover - rebuilding[[i, "block_rebuild_cost"]]  #  update our rebuild monies
      i <- i + 1  #   then move on
      if (i == max_i) break  #  stopping condition
      
    } else {
      
      i <- i + 1  #  if we can't afford rebuild move to next block
      if (i == max_i) break  #  stopping condition
    }
    
  }
  
  to_be_rebuilt <- to_be_rebuilt[which( to_be_rebuilt != 0 )]  #  remove zeroes
    #  efficiency thing we prespecified the length of the to_be_rebuilt list
    
    #  OUTPUT VECTOR of blocks to rebuild
  #to_be_rebuilt <- rebuilding
  
  #  CHANGE APPROPRIATE VARIABLE VALUES AND TIDY
  rebuilt <- to_be_rebuilt
  
  return(to_be_rebuilt)
}
