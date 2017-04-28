# REPAIR -----------------------------------------------------------------
#'
#' A helper function that updates the \code{repair_status} of building components in a block.
#' 
#' Outputs a block_tibble after spending \code{per_block_spend} on one block. Used in the 
#' \code{\link{repair}} function. A new binary variable is created of the \code{repair_status} 
#' of each building component within the block (later dropped in the \code{\link{repair}} function).
#'  The building components are sorted by 
#' descending grade and then descending cost in the \code{\link{repair}} function prior to 
#' being passed to this helper function.
#' 
#' This function works by first checking if there's anything to repair in the block. If not,
#' it returns the input argument \code{block_tibble}. Only building components of \code{grade} 
#' B, C or D are considered for repair. If a building component has a repair cost of zero
#' it is also ignored for repair (these two logical tests are applied using an "OR"). As the
#' passed tibble would have been sorted by grade and then cost the function proceeds through
#' the sorted dataframe repairing what building components it can until all the money is spent. 
#' Money is divided evenly between each \code{buildingid} or block based on the \code{\link{repair}} function.
#'
#' @param block_tibble a block dataframe or tibble with a \code{costs}variable.
#' @param per_block_spend a scalar calculated from \code{repair_monies} / number of blocks. 
#' @return A \code{block_tibble} that has had building components ready to be
#'  repaired or not indicated
#' by the new variable \code{repair_status}.
#' 
#' @export
#' @examples 
#' 
#' example <- what_needs_repair_within_block(
#' tibble::tibble(cost = seq(from = 0, to = 4500, by = 500),
#'                grade = c("A", "B", "C", "D", "E", "B", "C", "N", "D", "B")),
#'                1e4)$repair_status
#'                
what_needs_repair_within_block <- function(block_tibble, per_block_spend) {
  
  #  money counter
  money2spend <- per_block_spend
  #  add placeholder column, default to no rebuild
  the_block <- dplyr::mutate_(block_tibble, repair_status = 0)
  
  if (max(the_block$cost, na.rm = TRUE) == 0) {  #  if new build block or all grade A
    #  Need na.rm = TRUE as 
    
    return(the_block)  #  i.e. if there's nothing to repair!
    
  } else {
    
    repair_min_not_zero <- min(the_block$cost[the_block$cost > 0]) #  cheapest repair in block
    # print(repair_min_not_zero)  #  debugging
    
    for (i in 1:nrow(the_block)) {
      
      if (the_block[i, "cost"] == 0 || the_block[[i, "grade"]] %in% c("N", "A", "E")) {  
        #  ignore zero costs and N or E Grade, don't repair
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
#'  building components are repaired to A in that order. Partial repairs does not happen
#'  and any overspend is discarded (spent by the school on other things).
#' 
#' Outputs a blockbuster_tibble after spending \code{repair_monies} on a finite
#' number of blocks. Repairing a block attempts to convert some 
#' (depending on \code{repair_monies} available) of it's building components
#' back to grade A, making it Excellent condition. If the building components have
#' zero cost they are assumed to be grade N or A (this is not always true) and are not
#' repaired (the cost model is the issue not the assumption). The input is seperated into
#' a list of dataframes by block and then \code{repair_status} binary variable added using 
#' the internal helper function \code{\link{what_needs_repair_within_block}}. The repairs are 
#' then carried out an additional helper function called ...
#' 
#'
#' @param blockbuster_tibble a blockbuster dataframe or tibble.
#' @param repair_monies a vector of length one. 
#' @return A \code{blockbuster_tibble} that has had building components repaired
#'  (or not if \code{repair_monies} are 0).
#' @importFrom dplyr %>% 
#' @importFrom data.table rbindlist
#' @export
#' @examples 
#' 
#' quick_fix <- blockbuster(dplyr::filter(blockbuster_pds,
#'   buildingid == 4382 | buildingid == 4472 | buildingid == 4487), 
#'   forecast_horizon = 2, rebuild_monies = 0, repair_monies = 2.5e6)
#' 
repair <- function(blockbuster_tibble, repair_monies) {
  
  if (repair_monies <= 0) {
    
    return(blockbuster_tibble)  #  save time, only repair if there's money!
    
  } else {
    

    #  REPAIR DECISION MAKING ----
    repairing <- blockbuster_tibble %>%
      dplyr::arrange_(~ buildingid, ~ dplyr::desc(grade), ~ dplyr::desc(cost))
    
    #  MONEY PER BLOCK ----
    number_of_blocks <-  nrow(dplyr::distinct_(repairing, ~ buildingid,
                                        .keep_all = FALSE))
    # print(paste("Number of blocks is ", number_of_blocks))
    per_block_spend <- (repair_monies / number_of_blocks)
    # print(paste("Per block spend is, £", per_block_spend))
    
    #  GO THROUGH EACH BLOCK MARKING "TO REPAIR" UNTIL MONEY RUNS OUT

    #  REPAIR STATUS
    # Split repairing into pieces by buildingid, repair_status update
    # what_needs_repair_within_block can be applied to this
    by_block_list <- vector(mode = "list", length = number_of_blocks)  #  pre-allocate
    by_block_list <- repairing %>%
      split(.$buildingid) %>%  #  split into a list of dataframes by block
      lapply(function(x) {  #  Apply helper function over a list of dataframes
        what_needs_repair_within_block(x,
                                       per_block_spend)
      }) %>%
      data.table::rbindlist()  #  changed from dplyr::bind_rows
    
    #  CARRY OUT REPAIRS
    #  Create a list of 2 dataframes, one for repair_status 0 and two for repair_status 1
    by_repair_status_list <- vector(mode = "list", length = 2)  #  pre-allocate
    by_repair_status_list <- by_block_list %>%
      split(.$repair_status)

    #  Getting repaired need change grade to A, aggregate then reareafy unit_area
    #  repair_status is true or positive, or repaired, hence name of df within list is "1"
    by_repair_status_list[["1"]]$grade <- factor("A")
    
    # Add correct levels to grade, N for new, E for decommisioned
    # should this be ordered rather than factor?
    levels(by_repair_status_list[["1"]]$grade) <- list(N = "N", A = "A", B = "B",
                                                  C = "C", D = "D", E = "E")
    #  ZERO COST after repair ----
    by_repair_status_list[["1"]]$cost <- as.double(0)
    
    #  TIDY ----
    #  Remove repair status column? or not for speed, PROBABLY BETTER TO LEAVE IT, time it?
    #  but need to drop for the aggregate step!
    repaired <- dplyr::bind_rows(by_repair_status_list)
    repaired$repair_status <- NULL
    
    #  https://www.r-bloggers.com/concatenating-a-list-of-data-frames/
    #  Do we need areafy2 here? QA by tracking an elementid total area through time
    #  Seems OK
    #  RESHAPE ---- merge repeated rows unit_area e.g. same buildingid, elementid and grade A
    #  print(repaired)
    df_tidied <- tibble::as_tibble(stats::aggregate(unit_area ~ .
                                                      ,
                           data = repaired, FUN = sum))

    #  OUTPUT ----
    
    output <- df_tidied   
    
  }
  return(output)
}