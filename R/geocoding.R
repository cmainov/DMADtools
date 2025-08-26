

###---------------------------------------------------------------
###  `validate_address_dc`: Validate a vector of DC Addresses
###---------------------------------------------------------------

#' @md
#' @title Validate a vector of Washington, D.C. Addresses
#'
#' @description A vectorized function to validate dc addresses (a wrapper for 
#' `tidymar::find_location()` ).
#'
#' # Citation for tidymar::find_location():
#' Gupta, H. (n.d.). tidymar: An R interface to DC’s Master Address Repository [R package]. GitHub. Retrieved from https://github.com/hersh-gupta/tidymar. Accessed on August 26, 2025.
#' 
#' @import purrr
#' 
#' @details
#' This wrapper function uses the `tidymar::find_location()` to validate whether an address exists in the District of Columbia’s Master Address Repository (MAR). Please note that the District of Columbia address should contain the quadrant (e.g., NW, SW, etc.). Otherwise, the address will return as invalid.
#'  
#' @param address A string vector of addresses. 
#' 
#' @returns An integer vector is returned. If the address was detected in the District of Columbia’s MAR, then a `1` is returned. If the address was not detected, a `0` is returned.
#' 
#' @export

validate_address_dc <- function( address ){
  
  purrr::map_int( .x = address,
                  .f = function( address){ 
                    tryCatch(
                      {
                        if( !is.na( tidymar::find_location( address )$MARID ) ) 1
                      },
                      error = function(e) {
                        # Custom behavior when an error occurs
                        return( 0 ) 
                      } 
                    )
                  },
                  
                  .progress = TRUE )
}


###---------------------------------------------------------------
###  `mar2_find`: Validate a vector of DC Addresses
###---------------------------------------------------------------

#' @md
#' @title Validate a vector of Washington, D.C. Addresses
#'
#' @description A vectorized function to validate dc addresses (a wrapper for 
#' `tidymar::find_location()` ).
#'
#' # Citation for tidymar::find_location():
#' Gupta, H. (n.d.). tidymar: An R interface to DC’s Master Address Repository [R package]. GitHub. Retrieved from https://github.com/hersh-gupta/tidymar. Accessed on August 26, 2025.
#' 
#' @import purrr
#' 
#' @details
#' This wrapper function uses the `tidymar::find_location()` to validate whether an address exists in the District of Columbia’s Master Address Repository (MAR). Please note that the District of Columbia address should contain the quadrant (e.g., NW, SW, etc.). Otherwise, the address will return as invalid.
#'  
#' @param address A string vector of addresses. 
#' 
#' @returns An integer vector is returned. If the address was detected in the District of Columbia’s MAR, then a `1` is returned. If the address was not detected, a `0` is returned.
#' 
#' @export

mar2_find <- function( address, field = "WARD" ){

  
  purrr::map_chr( .x = address,
                  .f = function( address){ 
                    tryCatch(
                      {
                        
                        d <- tidymar::find_location( address )
                        
                        if( !is.na( d$MARID ) ) d[[ field ]]
                      },
                      error = function(e) {
                        # Custom behavior when an error occurs
                        return( "Unregistered address" ) 
                      } 
                    )
                  },
                  
                  .progress = TRUE )
  
}

mar2_find( address = my_sf$ADDRESS[1:24] )
