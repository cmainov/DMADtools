###-------------------------------------------------------------------
###  `findlocation2`: Geocode a DC Address Using MAR2
###-------------------------------------------------------------------

#' @md
#' @title Geocode a DC Address with the OCTO's Master Address Repository 2 (MAR2) API
#'
#' @description Builds and sends a GET request query to the District of Columbia Office of the Chief Technology Officer (OCTO)'s MAR2 Batch Geocoder API service. Requires a MAR2 API key. See OCTO's website for more information on obtaining an API key (\url{https://developers.data.dc.gov/}). Also see `DMADtools::write_mar2_api_key` and `DMADtools::read_mar2_api_key` for handling the MAR2 API key within the environment of this package.
#' 
#' @import httr 
#' @import dplyr 
#' @import jsonlite 
#' @import purrr 
#' @import RCurl 
#' @import stringdist 
#' @import crayon
#' 
#' @usage findlocation2( address, api_key = NULL, refine_result = FALSE, verbose = TRUE, ...)
#' 
#' @details
#' OCTO's MAR2 API is a modern .NET geocoding web service that leverages ElasticSearch to refine address search results for DC address queries. It is recommended to not exceed 400 records in a single query. More information on the geocoding service can be found at \url{https://mar2.data.dc.gov/}.
#' 
#'Addresses are base 64 encoded and a GET request URL is constructed and sent to the API endpoint. Address inputs should follow a standard District of Columbia address ( `NUMBER(S)` `STREETNAME` `STREETTYPE [APT|UNIT NUMBER] QUADRANT [ZIPCODE]` ) For instance: "1600 Pennsylvania Ave NW 20500". The address apartment/unit number and ZIP code are not required. You can use `DMADtools::dc_address_parser` to assist in building MAR2-ready address strings to send in your request.  The return from the endpoint is then formatted into a tidy `tibble` output.
#' 
#' # Citation for the MAR2 API:
#' District of Columbia Government. “MAR 2 Batch Geocoder Portal.” Accessed October 29, 2026. \url{https://mar2.data.dc.gov/}. 
#' 
#' @param address A string, character vector, or list of District of Columbia addresses to geocode.
#' @param api_key A string or `NULL`. MAR2 API key value. If `NULL`, the function calls `DMADtools::read_mar2_api_key` to search for the key in the local .Renviron file. See `DMADtools::write_mar2_api_key` and `DMADtools::read_mar2_api_key` for more details.
#' @param refine_result A logical. Triggers an additional layer of fuzzy matching (using `stringdist::stringdist`) to filter search results and increase the probability of an exact match, if possible. Usually not needed.
#' @param verbose A logical. Display output and messaging in console.
#' @param ... Additional arguments forwarded to `stringdist::stringdist`. Note that if no additional arguments are included, the function reverts to the defaults in `stringdist::stringdist`. Use `method = ` to modify the fuzzy matching algorithm. See the Help documentation for `stringdist::stringdist` for more information on the available fuzzy searching algorithms.
#' 
#' @return A `list` of `tibble` frames with the results of each address query, in tidy format. If the query consists of a single address, an object of class `tibble` is returned.
#' 
#' @examples
#' 
#' libary( DMADtools )
#' 
#' findlocation2( "1600 Pennsylvania Ave NW" )
#' 
#' # use `refine_result` to further filter results
#' findlocation2( "1600 Pennsylvania Ave NW", refine_result = TRUE )
#' 
#' 
# send a vector of addresses
#' these_geocode <- c( "1600 Pennsylvania Ave NW",
#'                     "101 Independence Ave SE",
#'                     "1 1st Street NE" )
#' 
#' findlocation2( address = these_geocode, refine_result = TRUE  )
#' 
#' # send a list of addresses
#' these_geocode_list <- list( "1600 Pennsylvania Ave NW",
#'                             "101 Independence Ave SE",
#'                             "1 1st Street NE" )
#' 
#' findlocation2( address = these_geocode_list, refine_result = TRUE  )
#' 
#' # use Levenshtein distance for fuzzy mtaching
#' findlocation2( address = these_geocode_list, refine_result = TRUE, method = "lv" )
#' 
#' # use Levenshtein distance and suppress messaging
#' findlocation2( address = these_geocode_list, refine_result = TRUE, method = "lv",
#'                verbose = FALSE )
#' 
#' @export


findlocation2 <- function( address, api_key = NULL, refine_result = FALSE, verbose = TRUE, ... ){
  
  if( length( address ) > 10000 ) stop( "Too many addresses. The MAR2 API Batch Geocoder accepts up to 10,000 results. Consider sending separate queries and no more than 400 addresses per query." )
  
  if( length( address ) > 400 ) warning( "Note that the MAR2 API accepts up to 10,000 addresses for geocoding in a single query. Consider sending separate queries and no more than 400 addresses per query." )
  
  if( !is.logical( refine_result ) ) stop( accent( "refine_result" ), " must be one of ", logical( "TRUE" ), " or ", logical( "FALSE" ) )
  
  if( !is.logical( verbose ) ) stop( accent( "verbose" ), " must be one of ", logical( "TRUE" ), " or ", logical( "FALSE" ) )
  
  if( inherits( address, "list" ) ) address <- unlist( address )
    
  # capture dots
  dots <- list(...)
  
  # api key extract from .Renviron, if it exists and if api_key is NULL
  if( is.null( api_key ) ){
    key_send <- read_mar2_api_key()
    if( verbose ) cat( note( "Note:\n" ), "MAR2 API key detected in .Renviron file and will be used to send queries.\n" )
  } else key_send <- api_key
  
  # arguments for string_dist
  args_stringdist <- dots[ names( dots ) %in% c( "method", "useBytes", "weight", "q", "p", "bt", "mthread", "useNames" ) ]
  
  # encode separations in addresses if vector supplied
  add_sep <- paste( paste( address, collapse = "$$" ) ) # "||" is the chunk sequence separator. "$$" is the address separator
  
  # base64 encode the address string(s)
  base64_address <- RCurl::base64( add_sep )[1] %>% URLencode( reserved = TRUE )
  
  # GET request to API endpoint
  api_endpoint <- "https://datagate.dc.gov/mar/open/api/v2.2/locationbatch/"
  
  # parse URL for components
  parsed_url <- httr::parse_url( paste0( api_endpoint ) ) 
  
  # add query parameters
  parsed_url$query <- list( address_separator = "$$", # the string is automatically URL encoded in `build_url`, no need to encode it beforehand
                            chunkSequnce_separator = "||",  # the string is automatically URL encoded in `build_url`, no need to encode it beforehand
                            parallel = "false", # we set parallel argument to false bc the mar2 documentation suggests that although `parallel=true` will run faster, items are not guaranteed to be in the same order submitted and preserving order is essential to rest of fct
                            apikey = key_send )
  
  # build the URL for the GET request
  url_send <- httr::build_url( parsed_url )

  # send the POST request (POST method is used instead of GET bc it omits the address inputs from the URL)
    # the query parameters are sent in the body instead
  response_post <- httr::POST( url = url_send, 
                              body = list( `Content-Type`= "application/json-patch+json",
                                           accept = "text/json",
                                           address_base64 = base64_address,
                                           charset = "utf-8" ),
                              encode = "json" ) 
  
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
  flat <- inner$Results %>% purrr::flatten()
  
  # each list contains sublists for each column, so must flatten again; returns list of tibbles
  # where each element in the list corresponds to each address query in address input vector
  result <- purrr::map( .x = flat$addresses, .f = ~ 
                   as_tibble( jsonlite::flatten( .x$address ) ) %>% 
                     # clean up column names
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
    
    if( verbose ){
      fuzz_method <- if( length( args_stringdist[["method"]] ) == 0 ) "osa" else args_stringdist[["method"]] # "osa" is mentioned as default method in stringdist::stringdist() Help documentation
      
      fuzz_method_full <- switch( fuzz_method,
                                  osa      = "Optimal string alignment (restricted Damerau–Levenshtein distance)",
                                  lv       = "Levenshtein distance",
                                  dl       = "Full Damerau–Levenshtein distance",
                                  hamming  = "Hamming distance",
                                  lcs      = "Longest common substring distance",
                                  qgram    = "q-gram distance",
                                  cosine   = "Cosine distance between q-gram profiles",
                                  jaccard  = "Jaccard distance between q-gram profiles",
                                  jw       = "Jaro or Jaro–Winkler distance",
                                  soundex  = "Distance based on soundex encoding",
                                  stop( "Unknown ", accent( "method" ), ": '", fuzz_method, "'", call. = FALSE )
      )
      
      cat( note( "Note:\n" ), accent( "refine_result" ), "set to", paste0( logical( "TRUE" ), "." ), "\nUsing the", accent( fuzz_method_full ), "algorithm for fuzzy matching and additional filtering.\n" )
    }
    
    Map( function( listframe, i ){
      
      parsed <- dc_address_parser( address[i] ) # call `findlocation2` to parse and build a clean address for matching
      
      dists <- do.call( stringdist::stringdist, c( list( a = parsed$ADDRESS_CLEAN,
                                                         b = listframe$FULLADDRESS ), args_stringdist ) )
      
      these_match <- if( 0 %in% dists ){ which( dists == 0 ) # if there is a distance of 0, that is our match--keep it (them)
      } else if( length( unique( dists ) == 1 ) ){ seq_along( dists ) # if there are no 0's and all distances are the same, return original frame
      } else which( dists == min( dists ) ) # else, return the set of row(s) with the smallest string distance
      
      new_result <- listframe[ these_match, ] %>% 
        # recompute `_SCORE` column using softmax function
        mutate( `_SCORE` = 100 * exp( -dists[ these_match ] ) / sum( exp( -dists[ these_match ] ) ) ) # softmax function for score
      
      if( verbose ){
        
        if( nrow( new_result ) < nrow( listframe ) ) cat( note( "Note: query = ", accent( address[i] ), "\n" ), "Matches were filtered further from", accent( nrow( listframe ) ), "to", accent( nrow( new_result ) ), "matches, with a maximum match score of", paste0( paste0( accent( sprintf( "%.1f", max( new_result$`_SCORE` ) ), "%" ) ), "." ), "\n" )
      
      if( nrow( new_result ) == nrow( listframe ) ) cat( note( "Note: query = ", accent( address[i] ), "\n" ), "Fuzzy matching did not provide additional filtering advantage. There were", accent( nrow( new_result ) ), "matches, with a maximum match score of", paste0( paste0( accent( sprintf( "%.1f", max( new_result$`_SCORE` ) ), "%" ) ), "." ), "\n" )
      }
      
      new_result
      
    }, result, seq_along( result ) ) %>% { if( length(.) == 1 ) .[[1]] else . } # return tibble if length 1, else list of tibbles
  } else result %>% { if( length(.) == 1 ) .[[1]] else . }
  
}

