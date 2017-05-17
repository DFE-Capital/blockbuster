## ------------------------------------------------------------------------
require(tidyverse)  #  if not already loaded, load it
require(blockbuster)  #  we need this for obvious reasons

## ----eval=FALSE----------------------------------------------------------
#  ? blockbuster

## ------------------------------------------------------------------------
formals(blockbuster)

## ----eval = FALSE--------------------------------------------------------
#  blockbuster(blockbuster_pds[1, ], )
#  blockbuster(, 1)

## ----eval=FALSE----------------------------------------------------------
#  blockbuster(, forecast_horizon = 1)

## ----eval = FALSE--------------------------------------------------------
#  ?blockbuster_pds

## ------------------------------------------------------------------------
str(blockbuster_pds)

## ----eval = FALSE--------------------------------------------------------
#  one_yr_later_a <- blockbuster(blockbuster_tibble = blockbuster_pds[1, ],
#                              forecast_horizon = 1)
#  
#  one_yr_later_b <- blockbuster(blockbuster_tibble = blockbuster_pds[1:2, ],
#                              forecast_horizon = 1)
#  
#  one_yr_later_c <- blockbuster(blockbuster_tibble = blockbuster_pds[c(1, 3, 3, 7), ],
#                              forecast_horizon = 1)

## ------------------------------------------------------------------------
dplyr::filter(blockbuster::blockbuster_pds,  (buildingid == 4382 |
                buildingid == 4472 |
                buildingid == 4487) #  & element == "Roofs"
              )

## ------------------------------------------------------------------------
  #  We filter our PDS sample for just three blocks to keep things simple			
  x <- dplyr::filter(blockbuster::blockbuster_pds,
                     buildingid == 4382 |
                       buildingid == 4472 | buildingid == 4487)			


## ------------------------------------------------------------------------
y <- blockbuster(blockbuster_tibble = x, forecast_horizon = 5)
#  z <- blockbuster(x, 5) 
#  identical(y, z)  #  TRUE
class(y)

## ----eval=FALSE----------------------------------------------------------
#  plot(y)

## ----eval=FALSE----------------------------------------------------------
#  boxplot(y)

## ----eval=FALSE----------------------------------------------------------
#  summary(y)

## ------------------------------------------------------------------------
#  y[[1]]  #  timestep 0

final_year <- y[[6]]
final_year

## ------------------------------------------------------------------------
fivenum(final_year$cost)

## ----eval = FALSE--------------------------------------------------------
#  z <- blockbuster(blockbuster_tibble = x, forecast_horizon = 5,
#              rebuild_monies = c(0, 9e4, 0, 0),
#              repair_monies = 5e3,
#              rebuild_cost_rate = c(rep(1280, 4), 1285))

## ----eval = FALSE--------------------------------------------------------
#  attributes(z)

