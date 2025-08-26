
tmp_geojson <- tempfile(fileext = ".geojson")
download.file(
  "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Location_WebMercator/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  tmp_geojson
)

# Let's read the downloaded geoJson file with the sf library:
library(sf)
my_sf <- read_sf(tmp_geojson)


# a vectorized function to validate dc addresses (a wrapper for 
# tidymar::find_location() )
validate_address_dc <- function( address ){
  
  purrr::map_int( .x = address,
                  .f = function( address){ 
                    tryCatch(
                      {
                        if( !is.na( tidymar::find_location( address )$MARID ) ) 1
                      },
                      error = function(e) {
                        # Custom behavior when an error occurs
                        return( 0 ) 
                      } 
                    )
                  },
                  
                  .progress = TRUE )
}



validate_address_dc( address = my_sf$ADDRESS[1:24] )


mar2_find <- function( address, field = "WARD" ){

  
  purrr::map_chr( .x = address,
                  .f = function( address){ 
                    tryCatch(
                      {
                        
                        d <- tidymar::find_location( address )
                        
                        if( !is.na( d$MARID ) ) d[[ field ]]
                      },
                      error = function(e) {
                        # Custom behavior when an error occurs
                        return( "Unregistered address" ) 
                      } 
                    )
                  },
                  
                  .progress = TRUE )
  
}

mar2_find( address = my_sf$ADDRESS[1:24] )
