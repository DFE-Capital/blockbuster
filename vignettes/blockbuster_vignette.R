## ----warning=FALSE, message=FALSE, error=FALSE---------------------------
library(tidyverse)  #  Recommended for filterting rows desired for input.
library(markovchain)  #  Recommended for markovchain S4 object handling (advanced).

library(blockbuster)  #  Required.


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
dplyr::select(my_block_10years[[11]], buildingid, elementid, grade, unit_area) %>%
  dplyr::arrange(elementid, grade)

## ----warning=FALSE, message=FALSE, error=FALSE---------------------------

my_wall_10 <- blockbuster(my_wall, 10)
#  We are interested in the 10 th (+ 1) timestep
df <- my_wall_10[[11]]

#  Use gov style for plot, install these if you don't have them
# devtools::install_github('mangothecat/visualTest')  #  required for govstyle
# devtools::install_github("UKGov-Data-Science/govstyle")

p <- ggplot(data = df, aes(x = grade, y = unit_area)) +
  geom_bar(stat = "identity", fill = govstyle::gov_cols['turquoise']) +
  xlab("Condition") +
  ylab(expression(paste(
  "Unit area (",m^2,
  ")", sep = "")))

p +
  govstyle::theme_gov()

## ------------------------------------------------------------------------
total_wall_area <- sum(df$unit_area)
dplyr::mutate(df, prop_area = unit_area / total_wall_area) %>%
  dplyr::select(grade, unit_area, prop_area)

## ------------------------------------------------------------------------
#  http://www.machinegurning.com/rstats/map_df/
# my_block_2_years <- list(as_tibble(my_block_10years[[1]]),
#                          as_tibble(my_block_10years[[2]]),
#                          as_tibble(my_block_10years[[3]]))
# 
# cool_cars <- list(mtcars, mtcars, mtcars) %>%
#   map_df(
#     ~ .x , 
#     .id = "timestep"
#   ) %>%
#   group_by(cyl) %>%
#   summarise(mean(disp), mean(hp)) %>%
#   ggplot(aes(x = cyl, y = `mean(disp)`)) +
#   geom_point()
#   
#   ggplot +
#   aes(
#     x = x,
#     fill = dist
#   ) +
#   geom_histogram() +
#   facet_wrap(
#     ~dist,
#     ncol = 2 
#   )


## ------------------------------------------------------------------------
sessionInfo()

