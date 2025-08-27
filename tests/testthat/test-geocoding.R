library( sf )

# bring in test data from OpenDC
tmp_geojson <- tempfile( fileext = ".geojson" )

download.file(
  "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Location_WebMercator/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  tmp_geojson
)

# Let's read the downloaded geoJson file with the sf library:
d_sf <- read_sf( tmp_geojson )


test_that("expect no error", {
  
  expect_no_error( validate_address_dc( address = d_sf$ADDRESS[1:24] ) )
  
})

test_that("all 1's and 0's", {
  
  vec_address <- d_sf$ADDRESS[1:24] # address vector
  
  vec_address <- c( vec_address, "not a real address" ) 
  
  return_vector <- validate_address_dc( address = vec_address )
  
  expect_true( all( return_vector %in% c( 0, 1 ) ) ) # all should be either 0 or 1
  
  expect_true( tail( return_vector, 1 ) == 0 ) # expect last entry to be 0
  
  expect_true( head( return_vector, 1 ) == 1 ) # expect first entry to be 1
  
}
)

test_that( "single entry", {
  
  vec_address1 <- "1600 Pennsylvania Ave NW" # address that works
  
  vec_address2 <- "1600 Pennsylvania Ave" # address that doesn't work (needs quadrant)
  
  return_vector1 <- validate_address_dc( address = vec_address1 )
  
  return_vector2 <- validate_address_dc( address = vec_address2 )
  
  expect_true( return_vector1 == 1 ) # expect last entry to be 0
  
  expect_true( return_vector2 == 0 ) # expect first entry to be 1
  
}
)
