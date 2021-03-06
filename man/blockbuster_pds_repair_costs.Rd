% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{blockbuster_pds_repair_costs}
\alias{blockbuster_pds_repair_costs}
\title{A dataframe of repair costs at 2016 prices.}
\format{A dataframe of 800 rows and 8 variables to be used for cost estimation in blockbuster. 
\describe{
  \item{element}{The top level of the quantity surveyors' building component hierarchy.}
  \item{sub_element}{The mid level of the quantity surveyors' building component hierarchy.}
  \item{const_type}{The lowest level of the quantity surveyors' building component hierarchy.}
  \item{grade}{The condition grade of the building component}
  \item{repair_costs}{Cost in pounds per unit_area of the building component repaired to condition A.}
  \item{concated_building_component_grade}{To save computation time we pre concatenate these for lookup.}
  \item{concated_building_component_grade_clean}{To save computation time we pre concatenate these and tidy for lookup.}
  \item{concat_clean}{Convenience, pasted element, sub element and construction type with only letters.}
  \item{elementid}{Foreign key for building component to facilitate lookup.}
  ...
}}
\usage{
blockbuster_pds_repair_costs
}
\description{
A dataframe containing all repair costs per unit area by grade for each building component. 
This is used in the blockbuster function to multiply the building components and its unit_area by
the appropriate constant to estimate the expected repair costs to get the building component from
grade D, C or B to grade A (grade E has been added based on user feedback (E = D plus 5%)).
 Due to discrepancies in the spelling of the building components
and the original Excel Costs model, these have been fixed and matched to that of the PDS here, to ensure
compatability. For details of the process see \code{04_read_tidy_costs_data.R} in the \code{data-raw}
directory; also see the associated \code{tests}.
}
\keyword{datasets}

