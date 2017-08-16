
#  BLOCKBUSTER BIG  -------------------------------------------------------------

#' Blockbuster mapreduce-like hack for answering questions using all blockbuster_pds.
#' 
#' Copes with the large size of the Property Data Survey by randomly 
#' splitting the data (using the \code{buildingid} variable), 
#' running the \code{\link{blockbuster_sim}} seperately on 
#' each chunk and reducing the monies proportional to the number of splits or 
#' \code{partitions} and multiplying by the \code{pds_prop}.
#'  After computation is complete 
#' the simulated output for each timestep is joined together. Currently this only works
#' for the \code{blockbuster_pds} object and smaller (10% sample of the PDS). This approach 
#' could be used on the whole PDS data set with some extra effort and time resource invested into
#' making the code generaliseable. See the help for \code{\link{blockbuster}} and \code{\link{blockbuster_sim}}
#'  for more details. The comments in the body of the function explain how this 
#'  function works in more detail. Essentially it divides and conquers 
#'  sort-of-like how map reduce works, just a very simple version.
#'
#' @param blockbuster_tibble a blockbuster dataframe or tibble. 
#' @param forecast_horizon an integer for the number of timesteps to model deterioration over.
#' @param rebuild_cost_rate a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @param rebuild_monies a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @param repair_monies a numeric vector of length equal to the \code{forecast_horizon} or one.
#' @param output_dir a character string of where to write each timestep's tibble to.
#' @param output_filename a character string of the desired filename. Default is 
#' system date and the timestep.
#' @param plenty_of_ram the most conservative approach is setting this to FALSE.
#' @param pds_prop a numeric between zero and one describing the relative size of your tibble
#' to the entire PDS data set (\code{blockbuster_pds} is 0.1 of all \code{buildingid}). This is 
#' used to multiply by the monies thus reducing investment accordingly to match 
#' the size of the School Estate sample used. Currently defaulted at 10%. 
#' @param reproducible_seed make it reproducible by defining the random seed.
#' @param partitions the number of partitions the data is split into (currently fixed at 2).
#' @return An object of class \code{blockbuster_list} 
#' of n plus one tibbles (where n is the \code{forecast_horizon}). It also writes to disc this 
#' output, saving it in the \code{ouput_dir} with the \code{output_filename} suffixed 
#' with "_blockbuster_list.rds"
#' @importFrom purrr map_df
#' @importFrom readr read_rds
#' @importFrom readr write_rds
#' @export
#' @examples 
#' \dontrun{ten_percent_counterfactual <- blockbuster_big(blockbuster_pds,
#'  forecast_horizon = 1, pds_prop = 0.1,
#'   reproducible_seed = 1337, partitions = 2)}
#' 
blockbuster_big <- function(blockbuster_tibble, forecast_horizon,
                           rebuild_monies = 0, repair_monies = 0, rebuild_cost_rate = 1274,
                           pds_prop,
                           output_dir = "./output/",
                           output_filename = paste0(Sys.Date(), "-blockbuster_sim"),
                           plenty_of_ram = FALSE,
                           reproducible_seed = 1337,
                           partitions = 2) {
  
  #  Sensible value, not a percentage
  if(pds_prop < 0 | pds_prop > 1) {
    stop("pds_prop should be a proportion between zero and one.")
  }
  
  message("***** This function takes your input monies and multiplies it by the
          pds_prop argument, making it easier for you to run simulations 
          on the blockbuster_pds data object which represents 
          a ten percent random sample of blocks from the whole 
          Property Data Survey. \n")
  
  message("\n
          Currently this function only supports 10% or less PDS samples; 
          it splits the input in half and applies blockbuster_sim.")
  
  message("***** This function takes a long time to run, 1 hr per forecast_horizon
          ")
  
  if (pds_prop > 0.1) {
    warning("\n\n *** Are you sure the pds_prop is greater than 0.1? \n 
            N.B. The data object, blockbuster_pds, has a pds_prop of 0.1... 
            i.e. 10% of the PDS. \n\n
            Due to time constraints this function currently works with up to 0.1 only.")
  }
  
  #  blockbuster_sim contains other checks
  
  #  SPLIT THE TIBBLE INTO CHUNKS WITHOUT REPLACEMENT
  bigdata <- blockbuster_tibble
  #  it's not really big data :)
  #  but now i can say i've worked with bigdata
  
  #  we develop for 0.1 of PDS, could be scaled later
  set.seed(reproducible_seed)
  
  #  CORRECT MONIES
  #  save for attributes
  rebuild_monies_uncorrected <- rebuild_monies
  repair_monies_uncorrected <- repair_monies
  #  correct
  rebuild_monies <- (rebuild_monies * pds_prop) / partitions
  repair_monies <- (repair_monies * pds_prop) / partitions
  
  #  partition the vector of buildingid 
  #  as we are working with 0.1 PDS we can split in two
  pds_blocks_a <- sample(unique(bigdata$buildingid),
                         size = length(unique(bigdata$buildingid))
                         / partitions, replace = FALSE)

  #  filter bigdata
  #  Default creates 2 partitions or chunks of data to work on
  pds_a <- dplyr::filter(bigdata, buildingid %in% pds_blocks_a)
  #  NOT in a; not a typo, note the !
  pds_b <- dplyr::filter(bigdata, !buildingid %in% pds_blocks_a)
  
  #  BLOCKBUSTER SIM ALL CHUNKS
  #  run sim, it saves to disc, free up RAM after
  temp <- blockbuster::blockbuster_sim(pds_a, forecast_horizon,
                                       rebuild_monies,
                                       repair_monies,
                                       rebuild_cost_rate,
                                       output_dir = "./output/",
                                       output_filename = paste0(output_filename, "_chunk_a_"),
                                       plenty_of_ram = FALSE )
  rm(temp)

  temp <- blockbuster::blockbuster_sim(pds_b, forecast_horizon,
                                       rebuild_monies,
                                       repair_monies,
                                       rebuild_cost_rate,
                                       output_dir = "./output/",
                                       output_filename = paste0(output_filename, "_chunk_b_"),
                                       plenty_of_ram = FALSE)
  rm(temp)
  
  #  FUSE CHUNKS
  # Create list of file names to read
  
  rds_a <- list.files(path = "./output", pattern = "_chunk_a_")
  rds_b <- list.files(path = "./output", pattern = "_chunk_b_")
  
  #  Create placeholder
  #  Create placeholder variables for cost and block_rebuild_cost
  block_simmed <- readr::read_rds(paste0("./output/", rds_a[1]))
  block_simmed <- dplyr::slice(block_simmed, -(1:n()))  #  keep attributes, drop values
  #  Rep this and create a list of empty blockbuster tibbles
  block_simmed <- rep(list(block_simmed), (forecast_horizon + 1))
  
  #  need to read in timestep 0 at index 1
  for (i in 1: (forecast_horizon + 1)) {
    rds <- c(rds_a[i], rds_b[i])
    rds <- paste0("./output/", rds)
    
    block_simmed[[i]] <- purrr::map_df(rds, readr::read_rds)
  }
   

  #  TO OUTPUT AS ONE DATAFRAME FROM TEMP FILES
  # rds <- c(rds_a, rds_b)
  # rds <- paste0("./output/", rds)
  # 
  # output <- purrr::map_df(rds, readr::read_rds)
  
  # CUSTOM CLASS FOR GENERIC METHODS
  class(block_simmed) <- c("blockbuster_list", "list")
  # ATTRIBUTES DETAIL THE SPENDING PROFILES
  attr(block_simmed, "rebuild_cost_rate") <- rebuild_cost_rate
  attr(block_simmed, "rebuild_monies") <- rebuild_monies_uncorrected
  attr(block_simmed, "repair_monies") <- repair_monies_uncorrected
  
  output <- block_simmed
  #  OUTPUT AS RDS WITH ATTRIBUTES
  readr::write_rds(output, path = paste0(output_dir, output_filename,
                                                   "_collected_blockbuster_list",
                                                   ".rds")
  )
  #  RETURN AFTER WRITING
  return(output)
}