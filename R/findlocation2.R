
library( httr )
library( dplyr )
library( jsonlite )
library( purrr )
library( RCurl )
library( stringdist )

address <- c( "1600 Pennsylvania Ave NW 20050", "2201 Shannon Pl SE" )
api_key <-  Sys.getenv("MAR2_KEY" )

findlocation2 <- function( address, api_key, refine_result = FALSE, parallel = TRUE, ... ){
  
  # capture dots
  dots <- list(...)
  
  # arguments for string_dist
  args_stringdist <- dots[ names(dots) %in% c( "method", "useBytes", "weight", "q", "p", "bt", "mthread", "useNames" ) ]
  
  
  # encode separations in addresses if vectorsupplied
  add_sep <- paste( "||", paste( address, collapse = "$$" ) ) # "||" is the chunk sequence separator. "$" is the address separator
  
  # base64 encode the address string(s)
  base64_address <- RCurl::base64( add_sep )[1] %>% URLencode( reserved = TRUE )
  
  # GET request to API endpoint
  api_endpoint <- "https://datagate.dc.gov/mar/open/api/v2.2/locationbatch"
  
  # parse URL for components
  parsed_url <- parse_url( paste0( api_endpoint, "/", base64_address ) ) 
  
  # add query parameters
  parsed_url$query <- list( address_separator = "$$", # the string is automatically URL encoded in `build_url`, no need to encode it beforehand
                            chunkSequnce_separator = "||",  # the string is automatically URL encoded in `build_url`, no need to encode it beforehand
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
  result <- map( .x = flat$addresses, .f = ~ 
                   as_tibble( jsonlite::flatten( .x$address ) ) %>% 
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
  
  # if refine_result is TRUE, we use additional string distance/fuzzy matching functions to get the closest match and recompute scores
  if( refine_result ){
    Map( function( listframe, i ){
      
      parsed <- dc_address_parser( address[i] ) # call `dc_address_parser` to parse and build a clean address for matching
      
      dists <- do.call( stringdist, c( list( a = parsed$ADDRESS_CLEAN,
                                             b = listframe$FULLADDRESS ), args_stringdist ) )
      
      these_match <- if( 0 %in% dists ){ which( dists == 0 ) # if there is a distance of 0, that is our match--keep it
      } else if( length( unique( dists ) == 1 ) ){ seq_along( dists ) # if there are no 0's and all distances are the same, return original frame
      } else which( dists == min( dists ) ) # else, return the set of row(s) with the smallest string distance
      
      listframe[ these_match, ] %>% 
        # recompute `_SCORE` column using softmax function
        mutate( `_SCORE` = 100 * exp( -dists[ these_match ] ) / sum( exp( -dists[ these_match ] ) ) ) # softmax function for score
      
    }, result, seq_along( result ) )
  } else result
  
}

findlocation2(address = "1600 Pennsylvania Ave NW", api_key =  Sys.getenv("MAR2_KEY"), parallel = TRUE, refine_result = TRUE, method = "lv" )
