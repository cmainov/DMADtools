###---------------------------------------------------------------
###  `validate_address_dc`: Validate a vector of DC addresses
###---------------------------------------------------------------

#' @md
#' @title Validate a vector of Washington, D.C. Addresses
#'
#' @description A vectorized function to validate dc addresses (a wrapper for 
#' `tidymar::find_location()` ).
#'
#' # Citation for tidymar::find_location():
#' Gupta, H. (n.d.). tidymar: An R interface to DC’s Master Address Repository (R package). GitHub. Retrieved from https://github.com/hersh-gupta/tidymar. Accessed on August 26, 2025.
#' 
#' @usage validate_address_dc( address )
#' @importFrom furrr future_map_int
#' @importFrom parallelly availableCores
#' @importFrom future plan
#' @importFrom future multisession
#' 
#' @details
#' This wrapper function uses the `tidymar::find_location()` to validate whether an address exists in the District of Columbia’s Master Address Repository (MAR). Please note that the District of Columbia address should contain the quadrant (e.g., NW, SW, etc.). Otherwise, the address will return as invalid.
#'  
#' @param address A string vector of addresses. 
#' 
#' @returns An integer vector is returned. If the address was detected in the District of Columbia’s MAR, then a `1` is returned. If the address was not detected, a `0` is returned.
#' 
#' @examples
#' 
#' validate_address_dc( address = "1600 Pennsylvania Ave NW" )
#' 
#' @export


validate_address_dc <- function( address ){
  UseMethod( "validate_address_dc")
}

#' @export
#' @method validate_address_dc default

validate_address_dc.default <- function( address ){
  
  # set up multi-core processing
  old_plan <- future::plan()
  future::plan( future::multisession, workers = parallelly::availableCores() )
  
  y <- furrr::future_map_int( .x = address,
    .f = function( address ){ 
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
    
    on.exit( future::plan( old_plan ), add = TRUE)
    
    # reset future plan to system default
    return( y )
  }
  
  #' @export
  #' @method validate_address_dc list
  
  validate_address_dc.list <- function( address ){
    address <- unlist( address )
    NextMethod()
  }
  
  ###----------------------------------------------------------------------
  ###  `mar2_find`: Extract address features from a vector of DC addresses
  ###----------------------------------------------------------------------
  
  #' @md
  #' @title Extract Address Features from a Vector of Washington, D.C. Addresses
  #'
  #' @description A vectorized function (a wrapper for `tidymar::find_location()` ) to extract address features (e.g., ward, census tract, ZIP code, etc.).
  #'
  #' # Citation for tidymar::find_location():
  #' Gupta, H. (n.d.). tidymar: An R interface to DC’s Master Address Repository (R package). GitHub. Retrieved from https://github.com/hersh-gupta/tidymar. Accessed on August 26, 2025.
  #' 
  #' @usage mar2_find( address, field = "WARD" )
  #' 
  #' @importFrom furrr future_map_int
  #' @importFrom parallelly availableCores
  #' @importFrom future plan
  #' @importFrom future multisession
  #' 
  #' @details
  #' This wrapper function uses the `tidymar::find_location()` to extract a field from the `tibble` returned after querying the District of Columbia’s Master Address Repository (MAR) via the `tidymar::find_location()` interface.
  #' 
  #' Please note that the District of Columbia address should contain the quadrant (e.g., NW, SW, etc.). Otherwise, the address will return as invalid.
  #'  
  #' @param address A string vector of addresses. 
  #' @param field A string. The field to return. Must be a column from the `tibble` returned by `tidymar::find_location()`. See [https://github.com/hersh-gupta/tidymar](https://github.com/hersh-gupta/tidymar) for a list of fields that can be extracted. The default field is "WARD".
  #' 
  #' @returns A character vector with the field requested.
  #' 
  #' @examples
  #' 
  #' mar2_find( address = "1600 Pennsylvania Ave NW", field = "WARD" )
  #' 
  #' mar2_find( address = "1600 Pennsylvania Ave NW", field = "ANC" )
  #' 
  #' @export
  
  mar2_find <- function( address, field = "WARD" ){
    UseMethod( "mar2_find" )
  }
  
  #' @export
  #' @method mar2_find character
  
  mar2_find.character <- function( address, field = "WARD" ){
    
    # set up multi-core processing
    old_plan <- future::plan()
    future::plan( future::multisession, workers = parallelly::availableCores() )
    
    y <- furrr::future_map_chr( .x = address,
      .f = function( address ){ 
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
      
      on.exit( future::plan( old_plan ), add = TRUE)
      
      # reset future plan to system default
      return( y )
    }
    
    
    
    
    