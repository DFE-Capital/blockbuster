x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472			
                   | buildingid == 4487)

rebuild_cost_rate <- 1274
rebuild_monies <- 2e6
#  testing
rebuilding <- x   #  prepare for rebuild if there's money
rebuilding <- dplyr::mutate_(rebuilding, block_rebuild_cost = ~(rebuild_cost_rate * gifa))
rebuilding <- dplyr::group_by(rebuilding, buildingid)
rebuilding <- dplyr::summarise_(rebuilding, cost_sum = ~sum(cost, na.rm = FALSE),
                    block_rebuild_cost = ~max(block_rebuild_cost, na.rm = FALSE),
                    cost_to_rebuild_ratio = ~(cost_sum / block_rebuild_cost))
rebuilding <- dplyr::arrange_(rebuilding, ~desc(cost_to_rebuild_ratio))

#  above looks fine

cheapest_rebuild <- min(rebuilding$block_rebuild_cost)
max_i <- nrow(rebuilding) + 1  # ?? why is this plus one? Stopping condition, ok
#  Money to spend
money_leftover <- rebuild_monies  #  it failed when 2e6, work out why

#  INITIALISE VARIABLE
to_be_rebuilt <- integer(length = nrow(rebuilding))

#  USE SORTED BUILDING ID VECTOR & WHILE To DETERMINE REBUILD STATUS
i <- 1  #  iteration
while (money_leftover > cheapest_rebuild) {
  print(paste("Money start of logic", money_leftover))
  if (rebuilding[i, "block_rebuild_cost"] <= money_leftover) { 
    
    to_be_rebuilt[i] <- rebuilding[[i, "buildingid"]]  #  we add to our to_be_rebuilt list
    money_leftover <- money_leftover - rebuilding[[i, "block_rebuild_cost"]]  #  update our rebuild monies
    print(paste("Money after rebuild", money_leftover))
    i <- i + 1  #   then move on
    if (i == max_i) break  #  stopping condition
    
  } else {
    
    i <- i + 1  #  if we can't afford rebuild move to next block
    if (i == max_i) break  #  stopping condition
    print(paste("Money after no rebuild", money_leftover))
  }
  
}
#  OUTPUT VECTOR of blocks to rebuild 
to_be_rebuilt <- to_be_rebuilt[which(to_be_rebuilt != 0)]  #  remove zeroes

# above looks good actually

rebuild_tibble <- x
rebuild_tibble <- dplyr::mutate_(x, rebuild_status = ~(dplyr::if_else(
    condition = buildingid %in% to_be_rebuilt,  # p.359 Advanced R, could be faster
    true = 1,
    false = 0 
  ))
  )

#  fine as is, not getting rebuilt so no change to these rows
rebuild_tibble_not <- dplyr::filter_(rebuild_tibble, ~(rebuild_status == 0))

#  Getting rebuilt need change grade to N and reareafy unit_area
rebuild_tibble_to_rebuild <- dplyr::filter_(rebuild_tibble, ~(rebuild_status == 1))

#  above ok

df <- rebuild_tibble_to_rebuild  #  easier to read
#  Collapse, as we will recalculate unit area and set all grade to N
#  Remove duplicates based on siteid, buildingid, elementid
#  CHANGE APPROPRIATE VARIABLE VALUES AND TIDY
#  there will be 
rebuilt <- df[!duplicated(df[, c("lano", "siteid", "buildingid", "elementid")]), ]
rebuilt <-  dplyr::mutate(rebuilt, grade = factor("N", levels = list(N = "N", A = "A", B = "B",
                                                  C = "C", D = "D", E = "E")),
                cost = 0
  )  #  need to aggregate!
rebuilt <- blockbuster::areafy2(rebuilt, input_checks = FALSE)  #  disable messages

# APPEND ROWS FROM REBUILT TO NOT REBUILT TIBBLE
# drop temp variables, if rebuilt Grade is N
output <- dplyr::bind_rows(rebuilt, rebuild_tibble_not) %>%
  dplyr::select_(~(-rebuild_status))

return(output)

#  As 2e6 rebuild is not doing what is anticipated this derserves a closer look
#  10e6 behaves as expected, perhaps im dividing the monies by the number of blocks?
# or something silly

z <- blockbuster(x, 5,
                 rebuild_monies = c(0, 2.5e6, 0, 0, 0),
                 repair_monies = 0,
                 rebuild_cost_rate = c(1280, 1280, 1280, 1280, 1285))

z1 <- blockbuster(x, 5,
                  rebuild_monies = c(0, 2e6, 0, 0, 0),
                  repair_monies = 0,
                  rebuild_cost_rate = c(1280, 1280, 1280, 1280, 1285))

z2 <- blockbuster(x, 5,
                  rebuild_monies = c(0, 0, 0, 0, 0),
                  repair_monies = 100e3,
                  rebuild_cost_rate = c(1280, 1280, 1280, 1280, 1285))