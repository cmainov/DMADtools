

# send a vector of addresses
these_geocode <- c( "1600 Pennsylvania Ave NW",
                    "101 Independence Ave SE",
                    "1 1st Street NE" )



test_that( "single run of findlocation2`", {
  
  res <- expect_no_error( findlocation2( "1 1st Street NE"  ) )
  
  # expect tibble for single address query
  expect_true( inherits( res, "tbl_df" ) )
  
  expect_no_error( findlocation2( "1 1st Street NE", api_key = read_mar2_api_key() ) )
  
  expect_true( nrow(res) == 1 )
  
  expect_true( res$FULLADDRESS == "1 1ST STREET NE" )
  
  expect_true( res$WARD == "Ward 6" )
  
  # no errors with other parameters selected
  expect_no_error( findlocation2( "1 1st Street NE", verbose = FALSE ) )
  expect_no_error( findlocation2( "1 1st Street NE", verbose = FALSE, refine_result = TRUE ) )
  expect_no_error( findlocation2( "1 1st Street NE", refine_result = FALSE, method = "lv") )
} )
  
test_that( "vector run of findlocation2`", {
  
  res <- expect_no_error( findlocation2( address = these_geocode ) )
  
  expect_true( length( res ) == length( these_geocode ) )
  
  # expect list for multiple address query
  expect_true( inherits( res, "list" ) )
  
  res_bind <- do.call( "rbind", res )
  
  # test unique results for each query
  expect_true( nrow( res_bind ) == length( these_geocode ) )
  
  expect_true( all( res_bind$FULLADDRESS == c( "1600 PENNSYLVANIA AVENUE NW",
                                          "101 INDEPENDENCE AVENUE SE",
                                          "1 1ST STREET NE" ) ) )
  
  # no errors with other parameters selected
  expect_no_error( findlocation2( these_geocode, verbose = FALSE ) )
  expect_no_error( findlocation2( these_geocode, verbose = FALSE, refine_result = TRUE ) )
  expect_no_error( findlocation2( these_geocode, refine_result = FALSE, method = "lv" ) )
  
  
} )

test_that( "list run of findlocation2`", {
  
  list_geocode <- as.list( these_geocode )
  
  res <- expect_no_error( findlocation2( address = list_geocode ) )
  
  # expect list for multiple address query
  expect_true( inherits( res, "list" ) )
  
  expect_true( length( res ) == length( list_geocode ) )
  
  res_bind <- do.call( "rbind", res )
  
  # test unique results for each query
  expect_true( nrow( res_bind ) == length( list_geocode ) )
  
  expect_true( all( res_bind$FULLADDRESS == c( "1600 PENNSYLVANIA AVENUE NW",
                                               "101 INDEPENDENCE AVENUE SE",
                                               "1 1ST STREET NE" ) ) )
  
  # no errors with other parameters selected
  expect_no_error( findlocation2( list_geocode, verbose = FALSE ) )
  expect_no_error( findlocation2( list_geocode, verbose = FALSE, refine_result = TRUE ) )
  expect_no_error( findlocation2( list_geocode, refine_result = FALSE, method = "lv") )
  
  
} )


test_that( "capture errors", {
  
  expect_error( findlocation2( address = these_geocode, refine_result = TRUE,
               method = "lk" ) )
  
  expect_error( findlocation2( address = "1600 Pennsylvania Ave", api_key = 1234, refine_result = FALSE ) )
  
  expect_error( findlocation2( address = rep( "add", 10001), api_key = 1234, refine_result = FALSE ) )
  
  expect_error( findlocation2( these_geocode, verbose = 2 ) )
  
  expect_error( findlocation2( these_geocode, verbose = "TRUE" ) )
  
  expect_error( findlocation2( these_geocode, refine_result = 2 ) )
  
  expect_error( findlocation2( these_geocode, refine_result = "TRUE" ) )
  
  
  
} )


