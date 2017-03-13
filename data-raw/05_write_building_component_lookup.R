
# CREATE BUILDING COMPONENT LOOKUP TABLE ----------------------------------

#  Probably should of built this early on and made it the basis of the package
#  Instead I used slower string lookup.
#  Could probably make things more efficient by later adding to this building component lookup
#  e.g. add a column for every deterioration rate and repair cost and decomissioned rebuild cost etc.

x <- dplyr::select(blockbuster_pds, elementid, element, sub_element, const_type)
building_component_lookup <- dplyr::arrange(unique(x), elementid)

# CREATE R DATA -----------------------------------------------------------

devtools::use_data(building_component_lookup, overwrite = TRUE)
