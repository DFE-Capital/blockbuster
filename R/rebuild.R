
# REBUILD -----------------------------------------------------------------

#' An internal function that determines rebuilding intervention effect on a blockbuster_tibble.
#' 
#' A given amount of money (\code{rebuild_monies}) is invested in rebuilding
#'  the \code{blockbuster_tibble}, passed as an arguement to the function. 
#'  This is dependent on the \code{block_rebuild_cost} and the \code{rebuild_monies}.
#' 
#' Outputs a blockbuster_tibble after spending \code{rebuild_monies} on a finite
#' number of blocks. Rebuilding a block converts all it's building components
#' back to grade N, making it as new.
#'
#' @param blockbuster_tibble a blockbuster dataframe or tibble. 
#' @param rebuild_cost_rate a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @param rebuild_monies a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @return A \code{blockbuster_tibble} that has had blocks rebuilt (or not if \code{rebuild_monies} are 0).
#' 
#' @importFrom
#' @export
#' @examples 
#' 
#' rebuild_two_blocks <- rebuild(dplyr::filter(blockbuster_pds,
#'  buildingid == 4382 | buildingid == 4472), rebuild_cost_rate = 1274, rebuild_monies = 5e6)
#' 
rebuild <- function(blockbuster_tibble, rebuild_cost_rate, rebuild_monies) {
  
  rebuilding <- blockbuster_tibble
  rebuilt <- rebuilding
  
  return(rebuilt)
}
