
findDCward <- function( address, api_key = NULL, cycle = "2022", verbose = TRUE, ... ){
  
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
                                         paste0( "ward_", cycle, collapse = URLencode(",", reserved = TRUE ) ),
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
  
  # flatten results
  flat <- inner$Result$addresses %>% purrr::flatten() 
  
  # return final ward with cycle label/name
  flat2 <- flat[ grepl( "ward", names( flat ) ) ]
  
  # return
  as.data.frame( lapply( flat2 , function(x){
    paste( "Ward", unlist( x )[ grepl( "\\.WARD$", names( unlist( x ) ) ) ] )
  } ) ) %>% 
    rename_with( .cols = contains( "ward"),
                 .fn = ~ str_remove( toupper( .x ), "\\_+" ) ) %>% 
    mutate( ADDRESS_INPUT = address ) %>% 
    relocate( ADDRESS_INPUT, .before = 1 )
}

