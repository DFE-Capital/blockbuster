
# DETERIORATE -------------------------------------------------------------

# Purpose: a family of functions to deteriorate any building ele_sub-ele_constr_type
# unit_area through time. 


#' Internal function that selects the correct markovchain object for deterioration of a row.
#' 
#' New approach that should improve speed 
#' because testing equality (of \code{elementid}) is simpler than testing
#'  inclusion in a set using %in%.
#'
#' @param blockbuster_initial_state_row a blockbuster dataframe or tibble single row.
#' @return a markovchain object containing the appropriate transition matrix
#' for the input blockbuster tibble row. Can only accept one row at a time due to how grep works.
#' @seealso \code{\link{blockbuster_det_data}}
#' @import markovchain
#' @export
#' @examples 
#' old_strings_method <- det_what_tm(
#' dplyr::mutate_(blockbuster_pds[1, ], 
#' concated = ~gsub(pattern = "[^[:alnum:] ]",
#' replacement = "",
#' paste(element, sub_element, const_type,
#'       sep = ""))))  #  old string method lookup required concated
#'       
#' new_lookup_is_faster <- det_what_tm(blockbuster_pds[96, ])
#'       
det_what_tm <- function(blockbuster_initial_state_row) {
  
  # Test that we are passing a blockbuster-like dataframe or tibble to the function
  
  if (!tibble::is.tibble(blockbuster_initial_state_row) && !is.data.frame(blockbuster_initial_state_row)) stop("'blockbuster_initial_state_row' must be a single row of a blockbuster tibble")
  
  #  Create new variable to match against 
  #  This is now cached before the for loop in blockbuster
  # blockbuster_initial_state_row <- dplyr::mutate_(blockbuster_initial_state_row,
  #                                              concated = ~paste(element, sub_element, const_type,
  #                                                            sep = "_"))
  #  Match new variable and get index of match, this provide mc reference, see 02_read_det_data
  #  Note how we ignore case due to differences in caps from  Excel and SQL files
  pos <- integer(length = 1)
  
  #  USE ELEMENTID INSTEAD
  pos <- which(blockbuster_initial_state_row$elementid == blockbuster_det_data$elementid)[1]  # some duplicates, take first one
  
  #  OLD METHOD USING STRINGS
  #  Match on alphanumeric
  #  benchmarked, perl option as TRUE is faster
  # pos <- grep(pattern = blockbuster_initial_state_row[["concated"]],
  #             x = blockbuster_det_data$concated_det,
  #             ignore.case = TRUE, perl = TRUE)
 
  # Test that length pos is not zero, therefore it has been matched
  if (length(pos) == 0) stop("Transition matrix of deterioration rates not found by name!")
  
  #  Use pos to provide transition matrix
  #  benchmarked as fast, we are not growing a list or vector
  det_dtmc <- blockbuster_mc_list@markovchains[[pos]]
  
  # Test that det_dtmc is NULL
  if (is.null(det_dtmc)) stop("Markovchain of deterioration rates not assigned!")
  
  # Return for use in nested design
  return(det_dtmc)
}

#' The deterioration of one blockbuster row through one time period.
#' 
#' Since changes to how the internal \code{concated} variable is created
#' the function requires this variable to be part of the input dataframe row.
#'
#' @param blockbuster_initial_state_row a blockbuster dataframe or tibble single row. 
#' Can only accept one row at a time due to how \code{grep} works
#'  in \code{\link{det_what_tm}}.
#' @return A one (if row is condition E) or two row tibble containing
#' the \code{unit_area} and condition of the \code{element sub_element constr_type} 
#' combination after one time period. This is handled within the function
#' by creating two intermediary objects; same grade and different grade.
#'  The ouput duplicates all other variables and values.
#' The timestep also needs to increase by one given deterioration has occurred.
#' @seealso \code{\link{blockbuster_det_data}}
#' @import markovchain
#' @export
#' @examples 
#' one_year_later <- det_eriorate(dplyr::mutate_(blockbuster_pds[1, ],
#' concated = ~gsub(pattern = "[^[:alnum:] ]",
#' replacement = "",
#' paste(element, sub_element, const_type,
#'       sep = ""))))
det_eriorate <- function(blockbuster_initial_state_row) {
  
  #  Get appropriate markovchain
  mc <- det_what_tm(blockbuster_initial_state_row)
  
  #  Use switch to select appropriate transition rate for 
  #  same grade, from x to x
  #  .subset2 benchmarked as faster than the vestigial alternative below
   mc_stay_same <- switch(EXPR = as.character(blockbuster_initial_state_row$grade),
                          "N" = .subset2(mc@transitionMatrix, 1, 1),
                          "A" = .subset2(mc@transitionMatrix, 2, 2),
                          "B" = .subset2(mc@transitionMatrix, 3, 3),
                          "C" = .subset2(mc@transitionMatrix, 4, 4),
                          "D" = .subset2(mc@transitionMatrix, 5, 5),
                          "E" = .subset2(mc@transitionMatrix, 6, 6))
     #  more familiar method but a little bit slower
     # mc_stay_same <- switch(EXPR = as.character(blockbuster_initial_state_row$grade),
     #                    "N" = mc@transitionMatrix[1, 1],
     #                    "A" = mc@transitionMatrix[2, 2],
     #                    "B" = mc@transitionMatrix[3, 3],
     #                    "C" = mc@transitionMatrix[4, 4],
     #                    "D" = mc@transitionMatrix[5, 5],
     #                    "E" = mc@transitionMatrix[6, 6])
  
  #  Deteriorate unit_area through one timestep at same grade
  #  output should be same grade but reduced unit_area as some decays
  same_grade <- dplyr::mutate_(blockbuster_initial_state_row,
                unit_area = ~(unit_area*mc_stay_same), 
                timestep = ~(timestep + 1), 
                grade = ~(grade)
                )
  
  #  Use switch to select appropriate transition rate for 
  #  worse grade, from x to y
  #  benchmark says this is slightly faster
  mc_get_worse <- switch(EXPR = as.character(blockbuster_initial_state_row$grade),
                         "N" = .subset2(mc@transitionMatrix, 1, 2),
                         "A" = .subset2(mc@transitionMatrix, 2, 3),
                         "B" = .subset2(mc@transitionMatrix, 3, 4),
                         "C" = .subset2(mc@transitionMatrix, 4, 5),
                         "D" = .subset2(mc@transitionMatrix, 5, 6),
                         "E" = 0)
    
  # old method, easier to intepret the above code
  # mc_get_worse <- switch(EXPR = as.character(blockbuster_initial_state_row$grade), 
  #                       "N" = mc@transitionMatrix[1, 2],
  #                       "A" = mc@transitionMatrix[2, 3],
  #                       "B" = mc@transitionMatrix[3, 4],
  #                       "C" = mc@transitionMatrix[4, 5],
  #                       "D" = mc@transitionMatrix[5, 6],
  #                       "E" = 0)
  
  #  store as object rather than running levels() function five times, see if_else below
  cache_levels <- levels(blockbuster_initial_state_row$grade)
  
  
  worse_grade <- dplyr::mutate_(blockbuster_initial_state_row,
                               unit_area = ~(unit_area*mc_get_worse), 
                               timestep = ~(timestep + 1), 
                               grade = ~(dplyr::if_else(grade == "E",
                                                      true = grade, #  E stays as E, the rest decay
                                                      false = switch(as.character(grade), 
                                                                     "N" = factor("A", cache_levels),
                                                                     "A" = factor("B", cache_levels),
                                                                     "B" = factor("C", cache_levels),
                                                                     "C" = factor("D", cache_levels),
                                                                     "D" = factor("E", cache_levels)
                                                      )))
  )
  
  # growing a dataframe? No binding two rows.
  output <- dplyr::bind_rows(same_grade, worse_grade)
  #  Drop duplicate row for "E" grade.
  output <- dplyr::filter_(output, ~(unit_area != 0))
  output <- tibble::as_tibble(output)
  
  return(output)
  
  
}

#' The deterioration of more than one blockbuster rows through one time period.
#'
#' @param blockbuster_tibble a blockbuster dataframe or tibble. 
#' @return A tibble containing
#' the \code{unit_area} and condition of the \code{element sub_element constr_type} 
#' combination after one time period. Duplicating all other variables and values.
#' The timestep also increases by one. The output tibble can be up to twice the number
#' of rows of the input tibble. Accordingly this function merges to reduce the number of rows
#' if possible, whereby there should be a max of six (one for each grade state) rows
#' per \code{elementid}. This function is built using a for loop
#'  and the \code{\link{det_eriorate}} function.
#' @export
#' @importFrom data.table rbindlist
#' @examples 
#' \dontrun{
#' one_year_later <- blockbust(dplyr::filter(blockbuster_pds, buildingid == 127617))
#' }
blockbust <- function(blockbuster_tibble) {
  
  #  Initiate placeholders, cache variable, preallocate space
  # blockbuster_tibble <- blockbuster_pds[1:10, ]  #  for testing
  blockbuster_initial_state <- blockbuster_tibble
  nRow <- nrow(blockbuster_tibble)
  d <- as.list(seq_len(nRow))
  
  #  http://winvector.github.io/Accumulation/Accum.html
  for (i in seq_len(nRow)) {
    blockbuster_initial_state_row <- dplyr::slice_(blockbuster_initial_state, ~(i))
    di <- blockbuster::det_eriorate(blockbuster_initial_state_row)
    d[[i]] <- di
    }
  
  d <- data.table::rbindlist(d)
  #  all.equal(blockbust(blockbuster_pds[1:10, ]), d)  # same as the original slower version
  
    output <- tibble::as_tibble(d)
    
    return(output)
  }