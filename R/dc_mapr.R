###---------------------------------------------------------------
###  `dc_mapr`: Generate Plain or Chropleth Map of DC
###---------------------------------------------------------------

#' @title Generate a plain or choropleth map of Washington, D.C. 
#'
#' @description Create a "nice" publication-level map of Washington, D.C. by ward (2012 or 2022 redistricting),
#' Zip-Code Tabulation Area (ZCTA), Census tract, or block.
#'
#' @details
#' Attribute data in `d` (see below) should be organized by ward, ZIP-code tabulation area (ZCTA), Census tract, or block. 
#' @param d A data frame or tibble with the attribute data used to fill the shapes.
#' @param geo One of "ward", "zcta", "tract', or "block". The geography that is desired.
#' @param id Column name in `d` that contains 

dc_mapr <- function( d, geo, id ){
  
  
}