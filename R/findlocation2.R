
library( httr )
library( dplyr )
library( jsonlite )
library( purrr )
library( RCurl )



findlocation2 <- function( address, api_key, parallel = TRUE ){
  
  # encode separations in addresses if vectorsupplied
  add_sep <- paste( "||", paste( address, collapse = " $$ " ) ) # "||" is the chunk sequence separator. "$" is the address separator
  
  # base64 encode the address string(s)
  base64_address <- RCurl::base64( add_sep )[1] %>% URLencode( reserved = TRUE )
  
  # GET request to API endpoint
  api_endpoint <- "https://datagate.dc.gov/mar/open/api/v2.2/locationbatch"
  
  # parse URL for components
  parsed_url <- parse_url( paste0( api_endpoint, "/", base64_address ) ) 
  
  # add query parameters
  parsed_url$query <- list( address_separator = URLencode( "$$", reserved = TRUE ),
                            chunkSequnce_separator = URLencode("||", reserved = TRUE ),
                            parallel = tolower( parallel ),
                            apikey = api_key )
  
  # build the URL for the GET request
  url_send <- build_url( parsed_url )
  
  # send the GET request
  response_get <- httr::GET( url = url_send, add_headers( accept = "accept: text/json" ) )
  
  # check status of response; stop if !=200
  httr::stop_for_status( response_get )
  
  # extract JSON text from response
  jsontext <- httr::content( response_get, as = "text" )
  
  # parse it
  json_data <- jsonlite::toJSON( list( text = jsontext ), auto_unbox = TRUE )
  
  # parse text in response to JSON, keep header out
  outer <- jsonlite::fromJSON( json_data ) 
  
  # parse again to retain only JSON text data
  inner <- jsonlite::fromJSON( outer$text )
  
  # flatten results
  flat <- inner$Results %>% purrr::flatten()
  
  # each list contains sublists for each column, so must flatten again; returns list of tibbles
  # where each element in the list corresponds to each address query in address input vector
  map( flat$addresses, ~ as_tibble( jsonlite::flatten( .x$address ) ) %>% 
         rename_with( .cols = everything(),
                      .fn = ~ toupper( sub( x = .x, 
                                            pattern = "^properties\\.(?=\\w+)", 
                                            replacement = "", 
                                            perl = TRUE # POSIX regex does not support lookaheads, need PERL engine for this function
                      ) ) ) %>% 
         rename_with( .cols = everything(),
                      .fn = ~ toupper( sub( x = .x, 
                                            pattern = "\\.", 
                                            replacement = "_"
                      ) ) ) )
  
}

findlocation2(address = "1600 Pennsylvania Ave NW", api_key =  Sys.getenv("MAR2_KEY"),parallel = TRUE )
