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
blockbuster_det_data[1, ]

## ------------------------------------------------------------------------
blockbuster_mc_list@markovchains[[1]]
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
  ggplot2::geom_boxplot() +
  ggplot2::facet_wrap(
    ~timestep
  )

p2 + ylab("Cost of repairs (£)") + xlab("Condition grade") +
  govstyle::theme_gov() 


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
  # theme_Publication()  +
  ggthemes::theme_hc() + ggthemes::scale_colour_hc() +
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


## ------------------------------------------------------------------------
sessionInfo()

