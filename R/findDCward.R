###------------------------------------------------------------------------------------
###  `findDCward`: Return Contemporary or Historical Ward Assignments for DC Addresses
###------------------------------------------------------------------------------------

#' @md
#' @title Return the contemporaneous or historical ward assignments for DC addresses using the OCTO's Master Address Repository 2 (MAR2) API
#'
#' @description Builds and sends a POST request query to the District of Columbia Office of the Chief Technology Officer (OCTO)'s MAR2 web service. Requires a MAR2 API key. See OCTO's website for more information on obtaining an API key (\url{https://developers.data.dc.gov/}). Also see `DMADtools::write_mar2_api_key` and `DMADtools::read_mar2_api_key` for handling the MAR2 API key within the environment of this package. This function builds off of `tidymar::findlocation`, which queried the legacy MAR API.
#' 
#' @usage findDCward( address, api_key = NULL, cycle = "2022", verbose = TRUE )
#' 
#' @details
#' OCTO's MAR2 API is a modern .NET geocoding web service that leverages ElasticSearch to refine address search results for DC address queries. This web service allows for a single address query at once and returns only the ward assignments for an address string. For a more comprehensive batch geocoding functionality, see `DMADtools::findlocation2`. More information on the MAR2 web service can be found at \url{https://mar2.data.dc.gov/}.
#' 
#' Address inputs should follow a standard District of Columbia address ( `NUMBER(S)` `STREETNAME` `STREETTYPE [APT|UNIT NUMBER] QUADRANT [ZIPCODE]` ). For instance: "1600 Pennsylvania Ave NW 20500". The address apartment/unit number and ZIP code are not required. You can use `DMADtools::dc_address_parser` to assist in building MAR2-ready address strings to send in your request.  The return from the endpoint is then formatted into a tidy `tibble` output.
#' 
#' # References:
#' ## MAR2 Web Service
#' District of Columbia Government. "MAR 2 Batch Geocoder Portal." Accessed October 29, 2026. \url{https://mar2.data.dc.gov/}. 
#' 
#' @param address A string, character vector, or list of District of Columbia addresses to geocode. If known, a `MARID` can be supplied that links to an exact match in the MAR.
#' @param api_key A string or `NULL`. MAR2 API key value. If `NULL`, the function calls `DMADtools::read_mar2_api_key` to search for the key in the local .Renviron file. See `DMADtools::write_mar2_api_key` and `DMADtools::read_mar2_api_key` for more details.
#' @param cycle A string or character vector. The redistricting cycle to base the ward assignment on. One or more of "2002", "2012", "2022". Default if "2022". 
#' @param verbose A logical. Display output and messaging in console. 
#' 
#' @return A `tibble` with the results of the query. The first column (`ADDRESS_MATCH`) is the string of the address matched from the API response. `MARID_MATCH` is the MARID matched from the query. One or more of the following column will be present in the output depending on the arguments used in `cycle`, `WARD2002`, `WARD2012`, `WARD2022`.  These fields contain the ward assignment for each redistricting cycle selected as a string (e.g., )
#' 
#' @examples
#' 
#' library( DMADtools )
#' 
#' findDCward( "1500 S Capitol St SE" ) # default behavior will return 2022 ward assignment
#' 
#' # ask for all historical ward assignments in addition to contemporaneous
#' findDCward( "1500 S Capitol St SE", cycle = c( "2022", "2012", "2002" ) ) 
#' 
#'# use MARID instead
#' findDCward( "303109", cycle = c( "2022", "2012", "2002" ) ) 
#' 
#' @export

findDCward <- function( address, api_key = NULL, cycle = "2022", verbose = TRUE ){
  
  if( is.integer( cycle ) ) cycle <- as.character( cycle ) # coerce cycle to character if integer
  if( length( address ) > 1 ) stop( "Too many addresses. The MAR2 API Geocoder accepts 1 address at a time." )
  if( !!length( setdiff( cycle, c( "2002", "2012", "2022" ) ) ) ) stop( accent( "cycle" ), " must be one of ", accent("'2022'" ), ", ", accent("'2012'" ), ", or ", accent( "'2002'" ), ", " )
  
  # api key extract from .Renviron, if it exists and if api_key is NULL
  if( is.null( api_key ) ){
    key_send <- read_mar2_api_key()
    if( verbose ) cat( note( "Note:\n" ), "MAR2 API key detected in .Renviron file and will be used to send queries.\n" )
  } else key_send <- api_key
  
  
  # API endpoint address
  api_endpoint <- "https://datagate.dc.gov/mar/open/api/v2.2/locations/"
  
  # parse URL for components
  parsed_url <- httr::parse_url( paste0( api_endpoint, 
                                         paste0( "ward_", cycle, collapse = utils::URLencode(",", reserved = TRUE ) ),
                                         "/" ) )
  
  # add query parameters
  parsed_url$query <- list( geo = "false",
                            apikey = key_send )
  
  # build the URL for the GET request
  url_send <- httr::build_url( parsed_url )
  
  # send the POST request (POST method is used instead of GET bc it omits the address inputs from the URL)
  # the query parameters are sent in the body instead
  response_post <- httr::POST( url = url_send, encode = "raw",
                               body = jsonlite::toJSON( address, auto_unbox = TRUE ),
                               httr::add_headers( `Content-Type`= "text/json",
                                                  accept = "text/plain",
                                                  charset = "utf-8" ) ) 
  
  # check status of response; stop if !=200
  httr::stop_for_status( response_post )
  
  # extract JSON text from response
  jsontext <- httr::content( response_post, as = "text" )
  
  # parse it
  json_data <- jsonlite::toJSON( list( text = jsontext ), auto_unbox = TRUE )
  
  # parse text in response to JSON, keep header out
  outer <- jsonlite::fromJSON( json_data ) 
  
  # parse again to retain only JSON text data
  inner <- jsonlite::fromJSON( outer$text )
  
  if( !is.null( inner$Message ) ){
    if( inner$Message == "No results found" ) stop( "No results found." )
  }
  
  # flatten results
  flat <- inner$Result$addresses %>% purrr::flatten() 
  
  # return final ward with cycle label/name
  flat2 <- flat[ grepl( "ward", names( flat ) ) ] 
  
  # return
  as.data.frame( lapply( flat2 , function(x){
    paste( "Ward", unlist( x )[ grepl( "\\.WARD$", names( unlist( x ) ) ) ] )
  } ) ) %>% 
    rename_with( .cols = contains( "ward"),
                 .fn = ~ stringr::str_remove( toupper( .x ), "\\_+" ) ) %>% 
    mutate( ADDRESS_MATCH = flat$properties$FullAddress,
            MARID_MATCH = flat$properties$MarId ) %>% 
    relocate( c( ADDRESS_MATCH, MARID_MATCH ), .before = 1 ) %>% 
    as_tibble()
}

