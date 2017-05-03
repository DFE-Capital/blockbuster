#  Generic functions for blockbuster_list class of object

#' The default plotting of a \code{blockbuster_list} object.
#' 
#' Assigns a new method to the generic \code{plot} function.
#' 
#' Produces a line plot of total cost of repairs by grade through time.
#'
#'
#' @param blockbuster_list output from \code{\link{blockbuster}} function; an object of class \code{blockbuster_list}.
#' @return A ggplot object (lineplot).
#' @importFrom dplyr %>%
#' @examples 
#' plot(blockbuster(dplyr::filter(blockbuster_pds,
#'  buildingid == 127617), 5))
#' 
plot.blockbuster_list <- function(x, ...) {
  
  p1 <- x %>%
    purrr::map_df(
      ~ .x ,
      .id = NULL
    ) %>%
    dplyr::group_by(timestep, grade) %>%
    dplyr::summarise(sum_cost = sum(cost, na.rm = TRUE)) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = timestep, y = sum_cost, group = grade, colour = grade)
      ) +
    ggplot2::geom_line(size = 1.3) 
  
  
  p1 + ggplot2::ylab("Total cost of repairs (\u00A3)") +  #  unicode for pound sign
    ggplot2::xlab("Years after PDS") +
    govstyle::theme_gov() +
    ggplot2::theme(legend.position = "bottom") +
    # ggthemes::theme_hc() + ggthemes::scale_colour_hc() +
    ggplot2::ggtitle("")
}

#' The default box-plotting of a \code{blockbuster_list} object.
#' 
#' Assigns a new method to the generic \code{boxplot} function.
#'
#'
#' @param blockbuster_list output from \code{\link{blockbuster}} function; an object of class \code{blockbuster_list}.
#' @return A ggplot (boxplot) object.
#' @importFrom dplyr %>%
#' @examples 
#' boxplot(blockbuster(dplyr::filter(blockbuster_pds,
#'  buildingid == 127617), 5))
#' 
boxplot.blockbuster_list <- function(x, ...) {
  p1 <- x %>%
    purrr::map_df(
      ~ .x ,
      .id = NULL
    ) %>%
    dplyr::group_by("timestep") %>%
    ggplot2::ggplot(ggplot2::aes(x = grade, y = cost)) +
    ggplot2::geom_boxplot(outlier.colour = "red") +  #  Set to NA to avoid double plot
    ggplot2::facet_wrap(
      ~timestep
    )
  
  p1 + ggplot2::ylab("Cost of repairs (\u00A3)") + ggplot2::xlab("Condition grade") +
    govstyle::theme_gov() +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.05),
                                       alpha = 0.15)
}


#' The default summary of a \code{blockbuster_list} object.
#' 
#' Assigns a new method to the generic \code{summary} function.
#' 
#' Provides an overview of total costs by year and the proportion of the unit_area in
#' the different grades for the final year of the simulation. 
#'
#'
#' @param blockbuster_list output from \code{\link{blockbuster}} function; an object of class \code{blockbuster_list}.
#' @return A list containing some descriptive text and 
#' @importFrom dplyr %>%
#' @examples 
#' summary(blockbuster(dplyr::filter(blockbuster_pds,
#'  buildingid == 127617), 5))
#' 
summary.blockbuster_list <- function(x, ...) {
  y1 <- paste0(paste0("This simulation predicts the deterioration of the input blockbuster_tibble through  ", 
               length(x) - 1, " years."))
  
  y2 <- paste0("The total cost of repairs across all grades by year was:")
  y3 <- x %>%
    purrr::map_df(
      ~ .x ,
      .id = NULL
    ) %>%
    dplyr::group_by(timestep) %>%
    dplyr::summarise(sum_cost = round(sum(cost, na.rm = TRUE)))
  
  y4 <- paste0("The proportion of the unit_area in the different grades for the final year:")
  y5 <- x[[length(x)]] %>%
  dplyr::mutate_(prop_area = ~(unit_area / sum(unit_area)),
                 prop_cost = ~(cost / sum(cost))) %>%
    dplyr::select(grade, unit_area, prop_area, prop_cost) %>%
    dplyr::group_by(grade) %>%
    dplyr::summarise(total_proportion_unit_area = round(sum(prop_area, na.rm = TRUE), 2),
                     total_proportion_cost = round(sum(prop_cost, na.rm = TRUE), 2))
  
  return(list(y1, y2, y3, y4, y5))
  
  
}
