#' A random ten percent sample of the condition of the School Estate.
#'
#' A dataset containing the condition grade and other attributes of almost 255,000
#' building elements of the English School Estate. The unit_area was estimated using
#' the \code{areafy} function in this package. School identifying features have been removed.
#'
#' @format A tibble with 254,878 rows and 30 variables:
#' \describe{
#'   \item{businessunitid}{unique identifier code of the School which can exist on more than one site}
#'   \item{composition}{the proportion of that elementid of a given grade}
#'   \item{grade}{the condition of that element ranked }
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
#'  \code{deteriorate} function in this package.
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
#'  \code{deteriorate} function in this package. Similar to
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