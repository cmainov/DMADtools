
# test single address string 
a <- "1600 Pennsylvania Ave NW"

test_that( "expect no error", {
  
  expect_no_error( dc_address_parser( address = a ) )
  
 ap <- dc_address_parser( address = a )
  testthat::expect_true( inherits( ap, "data.frame" ) & nrow( ap ) == 1 )
} )

# vector of strings
a2 <- c( "1600 Pennsylvania Ave NW", "2201 Shannon Pl SE", "1350 Pennsylvania Ave NW" )

test_that( "vector of addresses", {
  
  expect_no_error( dc_address_parser( address = a2 ) )
  
  ap <- dc_address_parser( address = a2 )
  
  expect_true( all( ap$STNAME == c( "PENNSYLVANIA", "SHANNON", "PENNSYLVANIA" ) ) )
  expect_true( all( ap$ADDRNUM == c( "1600", "2201", "1350" ) ) )
  expect_true( all( ap$STNAME == c( "PENNSYLVANIA", "SHANNON", "PENNSYLVANIA" ) ) )
  expect_true( all( ap$STREET_TYPE2 == c( "AVENUE", "PLACE", "AVENUE" ) ) )
  expect_true( all( ap$QUADRANT == c( "NW", "SE", "NW" ) ) )
} )

# with zip code

# vector of strings
a3 <- c( "1350 Pennsylvania Ave NW Suite 201 20004" )

test_that( "zip code and unit", {
  
  expect_no_error( dc_address_parser( address = a3 ) )
  
  ap <- dc_address_parser( address = a3 )
  
  expect_true( all( ap$ZIPCODE == "20004" ) )
  expect_true( all( ap$APTNUM == "SUITE 201" ) )
  expect_true( all( ap$ADDRNUM ==  "1350" ) )
  expect_true( all( ap$STNAME == "PENNSYLVANIA" ) )
  expect_true( all( ap$STREET_TYPE2 == "AVENUE" ) )
  expect_true( all( ap$STREET_TYPE == "AVE" ) )
  expect_true( all( ap$QUADRANT == "NW" ) )
} )


# non standard address that throws warning

# vector of strings
a4 <- c( "Pennsylvania Ave NW Suite 201 20004" )

test_that( "warning with no add number", {
  
  expect_warning( dc_address_parser( address = a4 ) ) 
  
} )


# non standard address that throws warning

# input a factor, expect an error
a5 <- as.factor( c( "1600 Pennsylvania Ave NW", "2201 Shannon Pl SE", "1350 Pennsylvania Ave NW" ) )

test_that( "warning with no add number", {
  
  expect_error( dc_address_parser( address = a5 ) ) 
  
} )
