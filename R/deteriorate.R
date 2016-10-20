
# DETERIORATE -------------------------------------------------------------

# Purpose: a family of functions to deteriorate any building ele_sub-ele_constr_type
# unit_area through time. 


#' Select the correct markovchain object for deterioration of a row.
#'
#' @param blockbuster_initial_state_row a blockbuster dataframe or tibble single row.
#' @return a markovchain object containing the appropriate transition matrix
#' for the input blockbuster row. Can only except one row at a time due to how grep works.
#' @seealso \link{/man/blockbuster_det_data.Rd}
#' @export
#' @examples 
#' dtmc_a <- det_what_tm(blockbuster_pds[1, ])
det_what_tm <- function(blockbuster_initial_state_row) {
  
  # Test that we are passing a blockbuster-like dataframe or tibble to the function
  
  if (!tibble::is.tibble(blockbuster_initial_state_row) && !is.data.frame(blockbuster_initial_state_row)) stop("'blockbuster_initial_state_row' must be a single row of a blockbuster tibble")
  
  #  Create new variable to match against 
  blockbuster_initial_state_row <- dplyr::mutate(blockbuster_initial_state_row,
                                               concated = paste(element, sub_element, const_type,
                                                             sep = "_"))
  #  Match new variable and get index of match, this provide mc reference, see 02_read_det_data
  #  Note how we ignore case due to differences in caps from  Excel and SQL files
  pos <- as.integer()
  #  Match on alphanumeric, see 
  pos <- grep(gsub("[^[:alnum:] ]", "", blockbuster_initial_state_row$concated),
              gsub("[^[:alnum:] ]", "", blockbuster_det_data$concated_det),
              ignore.case = TRUE)
 
  # Test that length pos is not zero, therefore it has been matched
  if (length(pos) == 0) stop("Transition matrix of deterioration rates not found by name!")
  
  #  Use pos to provide transition matrix
  det_dtmc <- blockbuster_mc_list@markovchains[[pos]]
  
  # Test that det_dtmc is NULL
  if (is.null(det_dtmc)) stop("Markovchain of deterioration rates not assigned!")
  
  # Return for use in nested design
  return(det_dtmc)
}

#' The deterioration of one blockbuster row through one time period.
#'
#' @param blockbuster_initial_state_row a blockbuster dataframe or tibble single row. 
#' Can only accept one row at a time due to how \code{grep} works
#'  in \link{/man/det_what_tm.Rd}.
#' @return A one (if row is condition E) or two row tibble containing
#' the \code{unit_area} and condition of the \code{element sub_element constr_type} 
#' combination after one time period. Duplicating all other variables and values.
#' The timestep needs to increase by one.
#' @seealso \link{/man/blockbuster_det_data.Rd}
#' @export
#' @examples 
#' one_year_later <- deteriorate(blockbuster_pds[1, ])
det_eriorate <- function(blockbuster_initial_state_row) {
  
  #  Get appropriate markovchain
  mc <- det_what_tm(blockbuster_initial_state_row)
  
  #  Use switch to select appropriate transition rate for 
  #  same grade, from x to x
   mc_stay_same <- switch(as.character(blockbuster_initial_state_row$grade), 
                        "N" = mc@transitionMatrix[1, 1],
                        "A" = mc@transitionMatrix[2, 2],
                        "B" = mc@transitionMatrix[3, 3],
                        "C" = mc@transitionMatrix[4, 4],
                        "D" = mc@transitionMatrix[5, 5],
                        "E" = mc@transitionMatrix[6, 6])
  
  #  Deteriorate unit_area through one timestep at same grade
  #  output should be same grade but reduced unit_area as some decays
  same_grade <- dplyr::mutate(blockbuster_initial_state_row,
                unit_area = unit_area*mc_stay_same, 
                timestep = timestep + 1, 
                grade = grade
                )
  
  #  Use switch to select appropriate transition rate for 
  #  worse grade, from x to y
  mc_get_worse <- switch(as.character(blockbuster_initial_state_row$grade), 
                        "N" = mc@transitionMatrix[1, 2],
                        "A" = mc@transitionMatrix[2, 3],
                        "B" = mc@transitionMatrix[3, 4],
                        "C" = mc@transitionMatrix[4, 5],
                        "D" = mc@transitionMatrix[5, 6],
                        "E" = 0)
  
  worse_grade <- dplyr::mutate(blockbuster_initial_state_row,
                               unit_area = unit_area*mc_get_worse, 
                               timestep = timestep + 1, 
                               grade = dplyr::if_else(grade == "E",
                                                      true = grade, #  E stays as E, the rest decay
                                                      false = switch(as.character(grade), 
                                                                     "N" = factor("A", levels(blockbuster_initial_state_row$grade)),
                                                                     "A" = factor("B", levels(blockbuster_initial_state_row$grade)),
                                                                     "B" = factor("C", levels(blockbuster_initial_state_row$grade)),
                                                                     "C" = factor("D", levels(blockbuster_initial_state_row$grade)),
                                                                     "D" = factor("E", levels(blockbuster_initial_state_row$grade))
                                                      ))
  )
  
  output <- dplyr::bind_rows(same_grade, worse_grade)
  #  Drop duplicate row for "E" grade.
  output <- dplyr::filter(output, unit_area != 0)
  output <- tibble::as_tibble(output)
  
  return(output)
  
  
}
