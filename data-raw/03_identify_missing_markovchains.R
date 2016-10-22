
# PURPOSE -----------------------------------------------------------------

#  Identify all missing markovchain objects and transition matrices
#  based on character strings of element-sub_element-constr_type


x <- dplyr::mutate(blockbuster_pds, 
                   concated_pds = paste(element, sub_element, const_type,
                                        sep = "_"))

pds_concat_list <- unique(x$concated_pds)
# remove punctuation
pds_concat_list_alnum <- gsub("[^[:alnum:] ]", "", pds_concat_list)
# make lower case
pds_concat_list_alnum_lower <- sapply(pds_concat_list_alnum, tolower)

det_concat_list <- unique(blockbuster_det_data$concated_det)
det_concat_list_alnum <- gsub("[^[:alnum:] ]", "", det_concat_list)
det_concat_list_alnum_lower <- sapply(det_concat_list_alnum, tolower)


# MATCH -------------------------------------------------------------------
is_it_missing <- match(pds_concat_list_alnum_lower, det_concat_list_alnum_lower)
sum(is.na(is_it_missing))
pds_concat_list_alnum_lower[is.na(is_it_missing)]
#  For original names
dtmc_missing <- pds_concat_list[is.na(is_it_missing)]
#  We need deterioration rates for these combinations


# AFFECTED ROWS -----------------------------------------------------------
affected_rows <- tibble()

for (i in 1:length(dtmc_missing)) {
  affected_rows <- dplyr::bind_rows(affected_rows, 
  dplyr::filter(x, concated_pds == dtmc_missing[i])
  )
}

# 12% of rows affected, need fix
(nrow(affected_rows) / nrow(x)) * 100


# SEPERATE and output as Excel --------------------------------------------
#unlist(strsplit(dtmc_missing[[1]], split = "_"))[3]

dtmc_needed <- tibble()
for (i in 1:length(dtmc_missing)) {
  
  dtmc_needed <- dplyr::bind_rows(dtmc_needed,
    tibble::tibble(element = unlist(strsplit(dtmc_missing[[i]], split = "_"))[1],
                 sub_element = unlist(strsplit(dtmc_missing[[i]], split = "_"))[2],
                 const_type = unlist(strsplit(dtmc_missing[[i]], split = "_"))[3])
  )
}

readr::write_delim(x = dtmc_needed, path = "./data-raw/missing_dtmc.txt", delim = "\t")