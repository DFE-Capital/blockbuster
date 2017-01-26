#' A random ten percent sample of the condition of the School Estate.
#'
#' A dataset containing the condition grade and other attributes of almost 255,000
#' building components of the English School Estate. The unit_area was estimated using
#' the \code{areafy} function in this package. School identifying features have been removed.
#'
#' @format A tibble with 254,878 rows and 30 variables:
#' \describe{
#'   \item{lano}{unique identifier code of the Local Authority in which the School is located}
#'   \item{businessunitid}{unique identifier code of the School which can exist on more than one site, removed for annonynimity}
#'   \item{urn}{Unique Reference Number, removed for annonynimity}
#'   \item{siteid}{A School can be located on more than one site, however one is the most common. A unique id number.}
#'   \item{elementid}{Unclear, probably a unique id for each building component in the School.}
#'   \item{element}{Top of the building component description hierarchy.}
#'   \item{sub_element}{Middle of the building component description hierarchy.}
#'   \item{const_type}{Bottom of the building component description hierarchy.}
#'   \item{const_type_accessible}{Could the cons_type be assessed? Sometimes building components might be hidden or difficult.}
#'   \item{grade}{Semi-subjective condition grade judged by visiting quantity surveyor.}
#'   \item{cost}{The cost to make good in pounds, to bring the offending building component, considering its unit_area back to grade A.}
#'   \item{composition}{the proportion of that elementid of a given grade}
#'   \item{storeys}{Number of floors in the associated block.}
#'   \item{gifa}{Gross Internal Floor Area metres squared in the associated block.}
#'   \item{ground_gifa}{Gross Internal Ground Floor Area metres squared in the associated block.}
#'   \item{block_perimeter}{Metres squared.}
#'   \item{height}{Height of block, metres squared.}
#'   \item{windows_doors}{Count of windows and doors.}
#'   \item{number_lifts}{Count per associated block.}
#'   \item{site_area_exc_field}{Site area minus the playing fields in metres squared.}
#'   \item{boundary_length}{Perimeter of site in metres squared presumably.}
#'   \item{field_area}{Playing fields areas metres squared.}
#'   \item{swimming_pool}{Swimming pool? Caveat, no definition or minimum size to qualify.}
#'   \item{timestep}{What year relative to the survey date are we in, timestep zero is at 2012-2014.}
#'   \item{unit_area}{An estimation / quantification of how much of a building component there is. See \code{\link{areafy}} }
#'   \item{cost_sum}{An aggregate of the costs by buildingid.}
#'   ...
#' }
#' @source \url{https://www.gov.uk/government/organisations/education-funding-agency}
"blockbuster_pds"

#' Deterioration rates of all blockbuster element-sub-element-construction types.
#'
#' A dataset containing the Deterioration rates of all element by sub-element
#' by construction type combinations. These rates were provided by the
#' consultant EC Harris and are necessary for predicting the
#' deterioration of each element by sub-element
#' by construction type combination through time as in the
#'  \code{det_eriorate} function in this package.
#'
#' @format A tibble with 147 rows and 9 variables:
#' \describe{
#'   \item{element}{Building taxonomic classification}
#'   \item{sub_element}{}
#'   \item{const_type}{Construction type}
#'   \item{concated_det}{A pasting together of the element sub-element and const_type.}
#'   \item{transition rates}{From a to b, b to c, c to d and d to e.}
#'   ...
#' }
"blockbuster_det_data"

#' A markovList of markovchain objects containing the Deterioration rates.
#'
#' A markovList containing the Deterioration rates of all element by sub-element
#' by construction type combinations as markovchain objects. 
#' These rates were provided by the
#' consultant EC Harris and are necessary for predicting the
#' deterioration of each element by sub-element
#' by construction type combination through time as in the
#'  \code{det_eriorate} function in this package. Similar to
#'  the \code{blockbuster_det_data} but as a S4 rather than S3.
#'  To access slots of a S4 object use "@" or \code{slot}() 
#'
#' @format A S4 markovList of 147 markovchain S4 objects:
#' \describe{
#'   \item{markovchains}{A list of 6 by 6 markovchain transition matrices}
#'   \item{name}{Name of the entire list.}
#'   ...
#' }
"blockbuster_mc_list"

#' A dataframe of repair costs at 2016 prices.
#'
#' A dataframe containing all repair costs per unit area by grade for each building component. 
#' This is used in the blockbuster function to multiply the building components and its unit_area by
#' the appropriate constant to estimate the expected repair costs to get the building component from
#' grade D, C or B to grade A.
#'
#' @format A dataframe of 556 rows and 6 variables to be used for cost estimation in blockbuster. 
#' \describe{
#'   \item{element}{The top level of the quantity surveyors' building component hierarchy.}
#'   \item{sub_element}{The mid level of the quantity surveyors' building component hierarchy.}
#'   \item{const_type}{The lowest level of the quantity surveyors' building component hierarchy.}
#'   \item{variable}{The condition grade of the building component}
#'   \item{repair_costs}{Cost in pounds per unit_area of the building component repaired to condition A.}
#'   \item{concated_building_component_grade}{To save computation time we pre concatenate these for lookup.}
#'   ...
#' }
"blockbuster_pds_repair_costs"
