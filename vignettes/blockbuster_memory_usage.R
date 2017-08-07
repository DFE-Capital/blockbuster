## ------------------------------------------------------------------------
library(pryr)
library(dplyr)
#  library(blockbuster)
devtools::load_all(".")  #  loads my development version of blockbuster
object_size(blockbuster::blockbuster_pds)

## ------------------------------------------------------------------------
sizes <- sapply(0:50, function(n) object_size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Size (bytes)", 
  type = "s")

## ------------------------------------------------------------------------
plot(0:50, sizes - 40, xlab = "Length", 
  ylab = "Bytes excluding overhead", type = "n")
abline(h = 0, col = "grey80")
abline(h = c(8, 16, 32, 48, 64, 128), col = "grey80")
abline(a = 0, b = 4, col = "grey90", lwd = 4)
lines(sizes - 40, type = "s")

## ------------------------------------------------------------------------
x <- 1:1e6
object_size(x)

## ------------------------------------------------------------------------
y <- list(x, x, x, x)
object_size(y)

## ------------------------------------------------------------------------
x <- blockbuster::blockbuster_pds
object_size(x)
y <- list(x, x, x, x)
object_size(y)

## ------------------------------------------------------------------------
x <- blockbuster_pds[1:100, ]
object_size(x)

y <- blockbuster(x, forecast_horizon = 5)
object_size(y)


## ------------------------------------------------------------------------

sizer <- function(y) {
  # where x is the blockbuster initial tibble of interest fed into blockbuster
  # where y is the blockbuster_list 
  n <- length(y) - 1
  z <- as.numeric()
  
  for (i in 1:n) {
    z[i] <- pryr::object_size(y[1:i+1])
  }
  
  z$plot <- plot(z/1000, type = "l",
       ylab = "Size in kB")
  
  return(as.vector(z))
  
} 

sizer(y)


## ------------------------------------------------------------------------
rm(y)
mem_change(y <- blockbuster(x, forecast_horizon = 5))

## ------------------------------------------------------------------------
x_small <- x %>%
  dplyr::select(buildingid:const_type, grade, cost, timestep, gifa, unit_area)


mem_change(y_small <- blockbuster(x_small, forecast_horizon = 5))
  

## ------------------------------------------------------------------------
sizer(y_small)

