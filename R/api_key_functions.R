###---------------------------------------------------------------
###  `write_mar2_api_key`: Write MAR2 API Key to .Renviron
###---------------------------------------------------------------

#' @md
#' @title Write the MAR2 API Key to .Renviron
#'
#' @description Modifies the local .Renviron by adding the user-supplied MAR2 API key as a new line in the file. All extant keys and variables previously in the file are left unmodified. Function structure is loosely based off of `tidycensus::census_api_key`.
#'
#' # Citation for `tidycensus::census_api_key`:
#' Walker, K and Herman, M (n.d.). tidycensus: Load US Census Boundary and Attribute Data as 'tidyverse' and 'sf'-Ready Data Frames (R package). GitHub. Retrieved from \url{https://walker-data.com/tidycensus/}. Accessed on October 27, 2026.
#' 
#' @usage write_mar2_api_key( key, overwrite = FALSE )
#' @import crayon 
#' 
#' @details
#' This function requires a MAR2 API key from the District of Columbia Office of the Chief Technology Officer (OCTO). See \url{https://developers.data.dc.gov/} for details on obtaining a key. To write the API key to the .Renviron file, the `overwrite` parameter will need to be set to `TRUE`. The default is `FALSE`.
#'  
#' @param key A string with your personal and private API key. See \url{https://developers.data.dc.gov/} for instructions on obtaining a MAR2 API key from the District of Columbia Office of the Chief Technology Officer.
#' @param overwrite A logical. Gives permission to the function to overwrite the .Renviron file with the supplied key value. Will need to be set to `TRUE` for the key to be written. Default is `FALSE`.
#' @examples
#' \dontrun{
#' write_mar2_api_key( key = "MY_KEY", overwrite = TRUE )
#' }
#' @export


write_mar2_api_key <- function( key, overwrite = FALSE ){
  
  # home directory
  r_path <- Sys.getenv( "HOME" )
  
  # .renviron path 
  renviron_path <- file.path( home, ".Renviron" )
  
  #.renviron exists logic, generate file if it doesn't exist
  if( !file.exists( renviron_path ) ) file.create( renviron_path )
  
  # read .renviron
  read_renviron <- readLines( renviron_path )
  
  which_mar2 <- which( grepl( pattern = "MAR2_KEY", x = read_renviron ) )
  
  if( overwrite ){
    
    if( is.null( which_mar2 ) || !length( which_mar2 ) ) cat( note( "MAR2_KEY not detected in .Renviron file. Will write supplied key and update the file.\n\n" ) )
    
    if( length( which_mar2 ) >= 1  ){ 
      cat( note( "Note:\n" ), "One or more MAR2_KEY entries detected in .Renviron file. Will delete all instances and rewrite with the supplied key.\n\n" )
      
      new_renv <- read_renviron[ c( -which_mar2, -which( read_renviron == "" ) ) ]
      new_renv_out <- c( new_renv, paste0( "MAR2_KEY=\"", key, "\"" ), "" )
      
    }
    writeLines( new_renv_out, renviron_path )
    cat( success( "MAR2_KEY was succesfully written to the local .Renviron file.\n" ) )
  }
}


