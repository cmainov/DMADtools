


findlocation2 <- function( address, api_key = NULL, refine_result = FALSE, verbose = TRUE, ... ){
  
  # capture dots
  dots <- list(...)
  
  # api key extract from .Renviron, if it exists and if api_key is NULL
  if( is.null( api_key ) ){
    key_send <- read_mar2_api_key()
    if( verbose ) cat( note( "Note:\n" ), "MAR2 API key detected in .Renviron file and will be used to send queries.\n" )
  } else key_send <- api_key
  
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
                            parallel = "true", # we set parallel argument to false bc the mar2 documentation suggests that parallel will run faster, but that items are not guaranteed to be in the same order submitted
                            apikey = key_send )
  
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
                                  stop( "Unknown method: ", fuzz_method, call. = FALSE )
      )
      
      cat( note( "Note:\n" ), accent( "refine_result" ), "set to", paste0( logical( "TRUE" ), "." ), "\nUsing the", accent( fuzz_method_full ), "algorithm for fuzzy matching and additional filtering.\n" )
    }
    
    Map( function( listframe, i ){
      
      parsed <- dc_address_parser( address[i] ) # call `dc_address_parser` to parse and build a clean address for matching
      
      dists <- do.call( stringdist::stringdist, c( list( a = parsed$ADDRESS_CLEAN,
                                                         b = listframe$FULLADDRESS ), args_stringdist ) )
      
      these_match <- if( 0 %in% dists ){ which( dists == 0 ) # if there is a distance of 0, that is our match--keep it (them)
      } else if( length( unique( dists ) == 1 ) ){ seq_along( dists ) # if there are no 0's and all distances are the same, return original frame
      } else which( dists == min( dists ) ) # else, return the set of row(s) with the smallest string distance
      
      new_result <- listframe[ these_match, ] %>% 
        # recompute `_SCORE` column using softmax function
        mutate( `_SCORE` = 100 * exp( -dists[ these_match ] ) / sum( exp( -dists[ these_match ] ) ) ) # softmax function for score
      
      if( verbose ){
        
        if( nrow( new_result ) < nrow( listframe ) ) cat( note( "Note:\n" ), "Matches were filtered further from", accent( nrow( listframe ) ), "to", accent( nrow( new_result ) ), "matches, with a maximum match score of", paste0( paste0( accent( max( new_result$`_SCORE` ), "%" ) ), "." ), "\n\n" )
      
      if( nrow( new_result ) == nrow( listframe ) ) cat( note( "Note:\n" ), "Fuzzy matching did not provide additional filtering advantage. There were", accent( new_result ), "matches, with a maximum match score of", paste0( paste0( accent( max( new_result$`_SCORE` ), "%" ) ), "." ), "\n\n" )
      }
      
      new_result
      
    }, result, seq_along( result ) )
  } else result
  
}

