

test_that( "general query; matching results, expected output", {
  
  # default behavior, expecting WARD2022 column return
  quer <- expect_no_error( findDCward( "1500 S Capitol St SE" ) )
  
  expect_true( is_tibble( quer ) )
  
  expect_true(  ncol( quer ) == 3 &  all( names( quer ) == c( "ADDRESS_MATCH", "MARID_MATCH", "WARD2022" ) ) )
  
  # expecting WARD2022 and WARD2012 column returns
  quer2 <- expect_no_error( findDCward( "1500 S Capitol St SE", cycle = c( "2022", "2012" ) ) )
  
  expect_true(  ncol( quer2 ) == 4 &  all( names( quer2 ) == c( "ADDRESS_MATCH", "MARID_MATCH", "WARD2022" , "WARD2012" ) ) )
  
  expect_true( is_tibble( quer2 ) )
  
  # expecting WARD2022, WARD2012, WARD2002 column returns
  quer3 <- expect_no_error( findDCward( "1500 S Capitol St SE", cycle = c( "2022", "2012", "2002" ) ) )
  
  expect_true(  ncol( quer3 ) == 5 &  all( names( quer3 ) == c( "ADDRESS_MATCH", "MARID_MATCH", "WARD2022" , "WARD2012", "WARD2002" ) ) )
  
  expect_true( is_tibble( quer3 ) )
  
  # EXPECT CORRECT RESULTS
  expect_true( quer3$WARD2022 == "Ward 8" & quer3$WARD2012 == "Ward 6" & quer3$WARD2002 == "Ward 6" )

  expect_true( quer$MARID_MATCH == "303109" & quer$ADDRESS_MATCH == "1500 SOUTH CAPITOL STREET SE" )
  
  expect_true( quer$MARID_MATCH == quer2$MARID_MATCH & quer2$MARID_MATCH == quer3$MARID_MATCH )
  
  expect_true( quer$ADDRESS_MATCH == quer2$ADDRESS_MATCH & quer2$ADDRESS_MATCH == quer3$ADDRESS_MATCH )
  
  }
)

test_that( "use MARID instead", {
  
  use_marid <- "303109"
  
  quer4 <- expect_no_error( findDCward( use_marid ) )
  
  expect_true( is_tibble( quer4 ) )
  
  expect_true( quer4$WARD2022 == "Ward 8" )
  
  expect_true( quer4$ADDRESS_MATCH == "1500 SOUTH CAPITOL STREET SE" )
  
  expect_true( quer4$MARID_MATCH == "303109" )
  
  quer5 <- expect_no_error( findDCward( use_marid, cycle = c( "2012", "2022" ) ) )
  
  expect_true( is_tibble( quer5 ) )
  
  expect_true( quer4$MARID_MATCH == quer5$MARID_MATCH )
  
  expect_true( quer4$ADDRESS_MATCH == quer5$ADDRESS_MATCH )
  
  expect_true( quer5$WARD2022 == "Ward 8" & quer5$WARD2012 == "Ward 6" )
  
} )


# more than one match
test_that( "greater than one match", {
  
  quer6 <- findDCward( "1600 Pennsylvania Ave" ) # not using quadrant

  expect_true( is_tibble( quer6 ) )
  
  expect_true( all( quer6$ADDRESS_MATCH == c( "1600 PENNSYLVANIA AVENUE NW", "1600 PENNSYLVANIA AVENUE SE" ) ) )
  
  expect_true( all( quer6$MARID_MATCH == c( "293211", "311021" ) ) )

})

# expect error when cycle is not correctly specified

# more than one match
test_that( "expect all years other than 2002, 2012, 2022 to prompt error", {
  
  # also will confirm integer intput for cycle works
  yrs_run <- 2000:2026
  yrs_run <- yrs_run[ !yrs_run %in% c( 2002, 2012, 2022 ) ]
  
  for( i in seq_along( yrs_run ) ){
    # default behavior, expecting WARD2022 column return
    expect_error( findDCward( "1500 S Capitol St SE" , cycle = yrs_run[i] ),
                  regexp = "cycle must be one of '2022', '2012', or '2002'")
  }
  
} )


# invalid address with no results

test_that( "capture no result found error", {
  
  expect_error( findDCward( "13048 Peter Norbeck Scenic Byway" ), regexp = "No\\sresults\\sfound\\." )
  
} )
