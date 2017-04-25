## ----warning=FALSE, message=FALSE, error=FALSE---------------------------
#  You could load tidyverse or you can load the individual packages, one-at-a-time.

library(dplyr)  #  need pipe %>%
library(ggplot2)  #  for visualisation
library(markovchain)  #  Recommended for markovchain S4 object handling (advanced).

library(blockbuster)  #  Required.

library(ggthemes)  #  Make pretty output with little effort.


## ------------------------------------------------------------------------
dplyr::glimpse(blockbuster_pds)

## ------------------------------------------------------------------------
#  output demonstration
dplyr::glimpse(  #  this function makes the output nicer to read
  blockbuster(blockbuster_tibble = blockbuster_pds[1:10, ],  #  ten rows for demo
                           forecast_horizon = 1)  #  We deteriorate through one year
               [[2]])  #  The initial data at timestep 0 is held in [[1]]


## ------------------------------------------------------------------------
formals(blockbuster)

## ------------------------------------------------------------------------
blockbuster_det_data[2, ]

## ------------------------------------------------------------------------
blockbuster_mc_list@markovchains[[2]]
# isS4(blockbuster_mc_list@markovchains[[1]])  #  TRUE

## ------------------------------------------------------------------------

x <- blockbuster_pds[1, ]
dplyr::select(x, element, sub_element, const_type, grade, unit_area)


## ------------------------------------------------------------------------
mc1 <- det_what_tm(x)
mc1

## ------------------------------------------------------------------------
mc1@transitionMatrix[, ]  #  [i, j] i for row, j column

## ------------------------------------------------------------------------
my_wall <- dplyr::filter(blockbuster_pds,
                         buildingid == 4382, sub_element == "Walls - Structure", 
                         const_type == "Brick / block")

my_wall_next_year <- blockbuster(my_wall, 1)
my_wall_next_year

my_wall_20year <- blockbuster(my_wall, 20)


## ------------------------------------------------------------------------
my_wall_next_year[[2]]
head(my_wall_next_year[[2]]$timestep, 1)  #  1 timestep later than initial

## ------------------------------------------------------------------------
y <- blockbuster_pds[1:20, ]  #  select all rows associated with decision level of interest, perhaps one block or one LA?
my_block_10years <- blockbuster(y, 10)
dplyr::select(my_block_10years[[11]],
              buildingid, elementid, grade, unit_area) %>%
  dplyr::arrange(elementid, grade)

## ----fig.width=9, fig.height=6-------------------------------------------
p4 <- my_wall_20year %>%
  purrr::map_df(
    ~ .x ,
    .id = NULL
  ) %>%
  dplyr::group_by(timestep, grade) %>%
  dplyr::summarise(sum_area = sum(unit_area, na.rm = TRUE)) %>%
  ggplot2::ggplot(aes(y = sum_area, x = grade)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(
    ~timestep
  )

p4 + ylab("Estimated area (m^2)") + xlab("Condition grade") +
  govstyle::theme_gov() 

## ----warning=FALSE, message=FALSE, error=FALSE, fig.width=9, fig.height=6----

my_wall_10 <- blockbuster(my_wall, 10)
#  We are interested in the 10 th (+ 1) timestep
df <- my_wall_10[[11]]

#  Use gov style for plot, install these if you don't have them
# devtools::install_github('mangothecat/visualTest')  #  required for govstyle
# devtools::install_github("UKGov-Data-Science/govstyle")

p <- ggplot2::ggplot(data = df, aes(x = grade, y = unit_area)) +
  ggplot2::geom_bar(stat = "identity", fill = govstyle::gov_cols['turquoise']) +
  xlab("Condition") +
  ylab(expression(paste(
  "Unit area (",m^2,
  ")", sep = ""))) +
  govstyle::theme_gov()

p 

## ------------------------------------------------------------------------
total_wall_area <- sum(df$unit_area)
dplyr::mutate(df, prop_area = unit_area / total_wall_area) %>%
  dplyr::select(grade, unit_area, prop_area)

## ----fig.width=9, fig.height=6-------------------------------------------
#  http://www.machinegurning.com/rstats/map_df/

# 
# cool_cars <- list(mtcars, mtcars, mtcars) %>%
#   map_df(
#     ~ .x ,
#     .id = "year"
#   ) %>%
#   group_by("year") %>%
#   ggplot(aes(x = cyl, y = disp, group = cyl)) +
#   geom_boxplot() +
#   facet_wrap(
#     ~year
#   )
# cool_cars

p2 <- my_block_10years %>%
  purrr::map_df(
    ~ .x ,
    .id = NULL
  ) %>%
  dplyr::group_by("timestep") %>%
  ggplot2::ggplot(aes(x = grade, y = cost)) +
  ggplot2::geom_boxplot(outlier.colour = "red") +  #  Set to NA to avoid double plot
  ggplot2::facet_wrap(
    ~timestep
  )

p2 + ylab("Cost of repairs (£)") + xlab("Condition grade") +
  govstyle::theme_gov() + geom_point(position = position_jitter(width = 0.05),
                                     alpha = 0.15)


## ----fig.width=9, fig.height=6-------------------------------------------
p3 <- my_block_10years %>%
  purrr::map_df(
    ~ .x ,
    .id = NULL
  ) %>%
  dplyr::group_by(timestep, grade) %>%
  dplyr::summarise(sum_cost = sum(cost, na.rm = TRUE)) %>%
  ggplot2::ggplot(aes(x = grade, y = sum_cost)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(
    ~timestep
  )

p3 + ylab("Total cost of repairs (£)") + xlab("Condition grade") +
  govstyle::theme_gov() 

## ----fig.width=9, fig.height=6-------------------------------------------

p4 <- my_block_10years %>%
  purrr::map_df(
    ~ .x ,
    .id = NULL
  ) %>%
  dplyr::group_by(timestep, grade) %>%
  dplyr::summarise(sum_cost = sum(cost, na.rm = TRUE)) %>%
  ggplot2::ggplot(aes(x = timestep, y = sum_cost, group = grade, colour = grade)) +
  ggplot2::geom_line(size = 1.3) 
  

p4 + ylab("Total cost of repairs (£)") + xlab("Years after PDS") +
  govstyle::theme_gov() +
  theme(legend.position = "bottom") +
  # ggthemes::theme_hc() + ggthemes::scale_colour_hc() +
  ggtitle("Cost of repairs through time for our example block")


## ------------------------------------------------------------------------
two_blocks_4_years_inflation <- blockbuster(dplyr::filter(blockbuster_pds,
                                                              buildingid == 4382 |
                                                                buildingid == 4472),
                                                forecast_horizon = 4,
                                                rebuild_cost_rate = c(1274, 1281, 1289, 1296) 
                                                )

two_blocks_4_years_inflation %>%
  purrr::map_df(
    ~ .x ,
    .id = NULL
  ) %>%
  dplyr::group_by(buildingid, timestep, block_rebuild_cost) %>%
  dplyr::tally()


## ----fig.width=9, fig.height=6-------------------------------------------
p5 <- my_block_10years %>%
  purrr::map_df(
    ~ .x ,
    .id = NULL
  ) %>%
  dplyr::group_by(timestep, element) %>%
  dplyr::summarise(sum_cost = sum(cost, na.rm = TRUE)) %>%
  ggplot2::ggplot(aes(x = element, y = sum_cost)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(
    ~timestep
  )

p5 + ylab("Total cost of repairs (£)") + xlab("Building element") +
  govstyle::theme_gov() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin = unit(c(20,10,10,10),"mm"))


## ----fig.width=9, fig.height=6-------------------------------------------
p6 <- my_block_10years[11] %>%
  purrr::map_df(
    ~ .x ,
    .id = NULL
  ) %>%
  dplyr::group_by(timestep, grade, element_shrt = as.factor(abbreviate(element, 14))) %>%
  dplyr::summarise(sum_unit_area = sum(unit_area, na.rm = TRUE),
                   sum_cost = sum(cost, na.rm = TRUE)) %>%
  ggplot2::ggplot(aes(x = grade, y = sum_cost, colour = element_shrt, size = sum_unit_area)) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::facet_wrap(
    ~ timestep
  )

p6 + ylab("Cost of repairs (£)") + xlab("Condition grade")  +
  theme_economist() + 
  scale_size_continuous(breaks = c(1000, 2000, 3000), range = c(3,10))


## ----fig.width=10, fig.height=12-----------------------------------------
#  We filter our PDS sample for just three blocks to keep things simple
x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472
                   | buildingid == 4487)
#  Rebuild spending profile
y <- blockbuster(x, forecast_horizon = 8, rebuild_monies = c(0, 5e6, 0, 0, 0, 0, 0, 0),  #  five million
                 rebuild_cost_rate = c(1274, 1274 + 0, 1274 + 12.74, 1286.74, 1286.74,
                                       1286.74, 1286.74, 1286.74))  #  £/m^2

p7 <- y %>%
  purrr::map_df(
    ~ .x ,
    .id = NULL
  ) %>%
  dplyr::group_by(timestep, element, grade) %>%
  dplyr::summarise(sum_cost = sum(cost, na.rm = TRUE)) %>%
  ggplot2::ggplot(aes(x = element, y = sum_cost, fill = grade)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::facet_wrap(
    ~timestep
  ) + scale_fill_discrete(drop = FALSE) + scale_x_discrete(drop = FALSE)  #  gives E a colour

p7 + ylab("Total cost of repairs (£)") + xlab("Building element") +
  govstyle::theme_gov() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin = unit(c(20,10,10,10),"mm")) +
  theme(legend.position = "top") 

## ------------------------------------------------------------------------
#  We filter our PDS sample for just three blocks to keep things simple
#  using identical blocks to our rebuild example above.

x <- dplyr::filter(blockbuster::blockbuster_pds,  buildingid == 4382 | buildingid == 4472
                   | buildingid == 4487)
#  Repair spending profile of different strategies
drip_repairs <- blockbuster(x, forecast_horizon = 5,  #  sum(rep(3e4, 5))
                            repair_monies = rep(3e4, 5))  #  £10k / block / year = £ 15e5

flood_repairs <- blockbuster(x, forecast_horizon = 5,
                             repair_monies = c(6e4, 6e4,
                                               1e4, 1e4, 1e4))  
#  (20k, 20k, 5k, 5k, 5k)* 3 blocks = £ 10.5e5


## ------------------------------------------------------------------------
drip <- dplyr::bind_rows(drip_repairs)
flood <- dplyr::bind_rows(flood_repairs)


## ------------------------------------------------------------------------
dripping <- drip %>%  #  extracat::visna(drip, sort = "b")
  group_by(timestep) %>%  #  drip_missing <- drip %>% filter(!complete.cases(.))
  #  table(drip_missing$timestep)  #  the originals rows are not being given this variable
  #  maybe this is OK and makes sense, block can't be rebuilt at timestep 0, therefore NA cost
  summarise(total_cost = sum(cost))

flooding <- flood %>%
  group_by(timestep) %>%
  summarise(total_cost = sum(cost))

#  we can join them and create an ID variable
spending_profile_ts <- dplyr::bind_rows("drip" = dripping, "flood" = flooding,
                                        .id = "spend_profiles")

spending_profile_ts

## ------------------------------------------------------------------------
p8 <- ggplot(spending_profile_ts, aes(timestep,
                                total_cost,
                                colour = spend_profiles,
                                group = spend_profiles)) +
  geom_line(size = 2)

  p8 <- p8 + ylab("Total cost of repairs (£)") + xlab("Years") +
  govstyle::theme_gov() + 
  theme(plot.margin = unit(c(20,10,10,10),"mm")) +
  theme(legend.position = "top")


## ----fig.width=10, fig.height=12-----------------------------------------
print(p8)

## ------------------------------------------------------------------------
#  Repair spending profile of different strategies

#  rebuild most expensive block in first year
rebuild_once <- blockbuster(x, forecast_horizon = 5,  
                            rebuild_monies = c(2440984, rep(0, 4))) %>%
  dplyr::bind_rows() %>%
  group_by(timestep) %>%
  summarise(total_cost = sum(cost, na.rm = TRUE))

#  repair funding using same funds spread over five years
drip_fix <- blockbuster(x, forecast_horizon = 5,
                             repair_monies = rep(2440984/5, 5)) %>% 
  dplyr::bind_rows() %>%
  group_by(timestep) %>%
  summarise(total_cost = sum(cost, na.rm = TRUE))

#  we can join them and create an ID variable
spending_profile_ts <- dplyr::bind_rows("Repairs only" = drip_fix,
                                        "Rebuild only" = rebuild_once,
                                        .id = "spend_profiles")

p9 <- ggplot(spending_profile_ts, aes(timestep,
                                total_cost,
                                colour = spend_profiles,
                                group = spend_profiles)) + geom_line(size = 2)

  p9 <- p9 + ylab("Total cost of repairs (£)") + xlab("Years") +
  govstyle::theme_gov() + 
  theme(plot.margin = unit(c(20,10,10,10),"mm")) +
  theme(legend.position = "top")
  
  

## ----fig.width=10, fig.height=12-----------------------------------------
print(p9)

## ------------------------------------------------------------------------
sessionInfo()

