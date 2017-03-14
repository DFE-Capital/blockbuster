
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
#' the
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
  
  #  USE SORTED BUILDING ID VECTOR FOR REBUILD
  to_be_rebuilt <- rebuilding
  
  #  CHANGE APPROPRIATE VARIABLE VALUES AND TIDY
  rebuilt <- to_be_rebuilt
  
  return(rebuilt)
}
