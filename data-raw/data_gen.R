###------------------------------###
###  Example Data Generation     ###
###------------------------------###

library( tidyverse )
library( sf )

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we generate some fake example data for use in the examples
# of the different functions in this package.
# 
# --------------------------------------------------------------------------------------------------------------------------------------------------------

source( "data-raw/utils.R" )

##-------------------------------------##
## (1.0.0) Function: `summary_table`   ##
##-------------------------------------##

if( !file.exists( "./data/d.example.rda" ) ){
  ## (1.1.0) Generate Conditional Categorical Data ##
  # --------------------------------------------------------------------------------------------------------------------------------------------------------
  
  set.seed( 425 ) # set seed for reproducibility given stochastic nature of sampling
  
  d.example <- data.frame()
  
  for( i in 1:500 ){
    
    # variable 1
    v1 <- sample( x = c( 1:3 ), size = 1,
                  replace = TRUE,
                  prob = c( 0.33, 0.5, 0.17 ) )
    
    v1.label <- c( "Geo 1", "Geo 2", "Geo 3" )[ v1 ]
    
    # variable 2
    v2 <- sample( x = c( 1:4 ), size = 1,
                  replace = TRUE,
                  # assign conditional probabilities based on value of `v1`
                  prob = { if( v1 == 1 ) c( 0.2, 0.6, 0.1, 0.1 )
                    else if( v1 == 2 ) c( 0.3, 0.4, 0.2, 0.1 ) 
                    else if( v1 == 3 ) c( 0.5, 0.2, 0.2, 0.1 ) } )
    
    v2.label <- c( "Char 1", "Char 2", "Char 3", "Char 3" )[ v2 ]
    
    # variable 3
    v3 <- sample( x = c( 1:3 ), size = 1,
                  replace = TRUE,
                  prob = c( 0.52, 0.24, 0.24 ) )
    
    v3.label <- c( "Other Char 1", "Other Char 2", "Other Char 3" )[ v3 ]
    
    # variable 4
    
    v4 <- sample( x = c( 1, 2 ), size = 1,
                  replace = TRUE,
                  prob = { if( v1 == 1 ) c( 0.2, 0.7 )
                    else if( v1 == 2 ) c( 0.3, 0.4 ) 
                    else if( v1 == 3 ) c( 0.5, 0.5 ) } )
    
    v4.label <- c( "Y", "N" )[ v4 ]
    
    # variable 4
    
    v5 <- sample( x = c( 1, 2 ), size = 1,
                  replace = TRUE,
                  prob = { if( v1 == 1 ) c( 0.5, 0.1 )
                    else if( v1 == 2 ) c( 0.7, 0.3 ) 
                    else if( v1 == 3 ) c( 0.6, 0.4 ) } )
    
    v5.label <- c( "Y", "N" )[ v5 ]
    
    # assign to `data.frame` and bind
    d.example <- rbind( d.example,
                        data.frame( v1 = v1.label,
                                    v2 = v2.label,
                                    v3 = v3.label,
                                    v4 = v4.label,
                                    v5 = v5.label) )
    
    
  }
  
  # --------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (1.2.0)  "Population" Data ##
  # --------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # `summary_table` can accept an aggregate variable with the population count for rate calculations.
  
  
  # assign population data for computing rates (aggregate variable)
  set.seed( 5789 )
  
  pop.1 <- sample( 50000:60000, size = 1 )
  pop.2 <- sample( 350000:360000, size = 1 )
  pop.3 <- sample( 70000:860000, size = 1 )
  
  d.example$v_pop <- ifelse( d.example$v1 == "Geo 1", pop.1,
                             ifelse( d.example$v1 == "Geo 2", pop.2,
                                     ifelse( d.example$v1 == "Geo 3", pop.3,
                                             NA ) ) )
  
  # --------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (1.3.0) Missing Data ##
  # --------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # to test `NAs.footnote` in `summary_table`
  
  # generate a separate dataset with missing data
  d.example.na <- d.example
  
  d.example.na[ sample( 1:nrow( d.example.na ), size = 9 ), "v2" ] <- NA # introduce 9 NAs randomly 
  
  # --------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (1.4.0) Save example data for export ##
  # --------------------------------------------------------------------------------------------------------------------------------------------------------
  
  usethis::use_data( d.example, d.example.na,
                     overwrite = TRUE ) # this creates the `/data` folder and stores the individual datasets in that directory
  
  # --------------------------------------------------------------------------------------------------------------------------------------------------------
  
}

##-------------------------------------##
## (2.0.0) Function: `dc_mapr`         ##
##-------------------------------------##

crs.projec <- 4326 # the CRS we use for maps; we will use WGS84 as the default for all maps

if( !file.exists( "./R/sysdata.rda" ) ){
  
  ## (2.1.0) Linear Hydrology/Waterways Shapefiles ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  yr <- 2024 # year of data to download
  
  fips.to.use <- c( "11001", # Washington, DC
                    "24031", # Montgomery County, MD
                    "24033", # Prince George's County, MD
                    "51013", # Arlington County, VA
                    "51059", # Fairfax County, VA
                    "51510") # Alexandria City
  
  dir.create( "./data/data-public" )
  
  # this query is by county FIPS code and so we loop with `Map`
  linear.water.dc.md.va <- Map( function( yr, fips ){
    
    # download file into repository
    file.nm.zip <- paste0( "tl_", yr, "_", fips, "_linearwater.zip" )
    file.nm.out <- paste0( "tl_", yr, "_", fips, "_linearwater" )
    file.nm.shp <- paste0( "tl_", yr, "_", fips, "_linearwater/",
                           paste0( "tl_", yr, "_", fips, "_linearwater.shp" ) )
    
    ## hydrology files
    url.1 <- paste0( "https://www2.census.gov/geo/tiger/TIGER", yr,
                     "/LINEARWATER/tl_", yr, "_", fips, "_linearwater.zip" )
    
    
    download.file( url.1, destfile = paste0( "./data/", "/data-public/linear-hydrology-shapefiles/",
                                             file.nm.zip ) )
    
    # unzip file in repository
    unzip( paste0( "./data/", "/data-public/linear-hydrology-shapefiles/", file.nm.zip ), 
           exdir = paste0( "./data/", "/data-public/linear-hydrology-shapefiles/",
                           file.nm.out ) )
    
    # remove compressed file from download
    file.remove( paste0( "./data/", "/data-public/linear-hydrology-shapefiles/",
                         file.nm.zip ) )
    
    # read in shapefile
    sf::st_read( paste0( "./data/", "/data-public/linear-hydrology-shapefiles/",
                         file.nm.shp ) ) %>%
      mutate( GEOID = fips )
    
  }, yr = yr, fips = fips.to.use
  ) %>%
    do.call( "rbind", . ) %>% 
    mutate( LABEL_WATER = ifelse( GEOID == "11001" & str_detect( FULLNAME, "Potomac R" ),
                                  "Potomac River",
                                  ifelse( GEOID == "11001" & str_detect( FULLNAME, "Anacostia R" ),
                                          "Anacostia River", NA ) ) ) %>% 
    st_as_sf() %>% 
    st_transform( crs = crs.projec )
  

  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (2.2.0) Area Hydrology/Waterways Shapefiles ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # this query is by county FIPS code and so we loop with `Map`
  area.water.dc.md.va <- Map( function( yr, fips ){
    
    # download file into repository
    file.nm.zip <- paste0( "tl_", yr, "_", fips, "_areawater.zip" )
    file.nm.out <- paste0( "tl_", yr, "_", fips, "_areawater" )
    file.nm.shp <- paste0( "tl_", yr, "_", fips, "_areawater/",
                           paste0( "tl_", yr, "_", fips, "_areawater.shp" ) )
    
    ## hydrology files
    url.1 <- paste0( "https://www2.census.gov/geo/tiger/TIGER", yr,
                     "/AREAWATER/tl_", yr, "_", fips, "_areawater.zip" )
    
    
    download.file( url.1, destfile = paste0( "./data/", "/data-public/area-hydrology-shapefiles/",
                                             file.nm.zip ) )
    
    # unzip file in repository
    unzip( paste0( "./data/", "/data-public/area-hydrology-shapefiles/", file.nm.zip ), 
           exdir = paste0( "./data/", "/data-public/area-hydrology-shapefiles/",
                           file.nm.out ) )
    
    # remove compressed file from download
    file.remove( paste0( "./data/", "/data-public/area-hydrology-shapefiles/",
                         file.nm.zip ) )
    
    # read in shapefile
    sf::st_read( paste0( "./data/", "/data-public/area-hydrology-shapefiles/",
                         file.nm.shp ) ) %>%
      mutate( GEOID = fips )
    
  }, yr = yr, fips = fips.to.use
  ) %>%
    do.call( "rbind", . ) %>% 
    mutate( LABEL_WATER = ifelse( GEOID == "11001" & str_detect( FULLNAME, "Potomac R" ),
                                  "Potomac River",
                                  ifelse( GEOID == "11001" & str_detect( FULLNAME, "Anacostia R" ),
                                          "Anacostia River", NA ) ) ) %>% 
    st_as_sf() %>% 
    st_transform( crs = crs.projec )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (2.3.0) Get Outline of DC (STATE TIGER File) ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  url.state <- paste0( "https://www2.census.gov/geo/tiger/TIGER", yr,
                       "/STATE/tl_", yr, "_us_state.zip" )
  
  # download file into repository
  file.nm.zip <- paste0( "tl_", yr, "_us_state.zip" )
  file.nm.out <- paste0( "tl_", yr, "_us_state" )
  file.nm.shp <- paste0( "tl_", yr, "_us_state/",
                         paste0( "tl_", yr, "_us_state.shp" ) )
  
  dir.create( paste0( "./data/", "/data-public/dc-boundary-shapefiles/" ) )
  
  download.file( url.state, destfile = paste0( "./data/", "/data-public/dc-boundary-shapefiles/",
                                               file.nm.zip ) )
  
  # unzip file in repository
  unzip( paste0( "./data/", "/data-public/dc-boundary-shapefiles/", file.nm.zip ), 
         exdir = paste0( "./data/", "/data-public/dc-boundary-shapefiles/",
                         file.nm.out ) )
  
  # remove compressed file from download
  file.remove( paste0( "./data/", "/data-public/dc-boundary-shapefiles/",
                       file.nm.zip ) )
  
  # read in shapefile
  dc.st <- sf::st_read( paste0( "./data/", "/data-public/dc-boundary-shapefiles/",
                                file.nm.shp ) ) %>%
    filter( STATEFP == "11" ) %>% 
    st_as_sf() %>% 
    st_transform( crs = crs.projec )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (2.4.0) Get Surrounding Counties Shapefile (COUNTY TIGER File) ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  url.county <- paste0( "https://www2.census.gov/geo/tiger/TIGER", yr,
                        "/COUNTY/tl_", yr, "_us_county.zip" )
  
  # download file into repository
  file.nm.zip <- paste0( "tl_", yr, "_us_county.zip" )
  file.nm.out <- paste0( "tl_", yr, "_us_county" )
  file.nm.shp <- paste0( "tl_", yr, "_us_county/",
                         paste0( "tl_", yr, "_us_county.shp" ) )
  
  dir.create( paste0( "./data/", "/data-public/surrounding-counties-shapefiles/" ) )
  
  download.file( url.county, destfile = paste0( "./data/", "/data-public/surrounding-counties-shapefiles/",
                                                file.nm.zip ) )
  
  # unzip file in repository
  unzip( paste0( "./data/", "/data-public/surrounding-counties-shapefiles/", file.nm.zip ), 
         exdir = paste0( "./data/", "/data-public/surrounding-counties-shapefiles/",
                         file.nm.out ) )
  
  # remove compressed file from download
  file.remove( paste0( "./data/", "/data-public/surrounding-counties-shapefiles/",
                       file.nm.zip ) )
  
  # read in shapefile
  dc.surr.counties <- sf::st_read( paste0( "./data/", "/data-public/surrounding-counties-shapefiles/",
                                           file.nm.shp ) ) %>%
    filter( GEOID %in% c( "24031", # Montgomery County, MD
                          "24033", # Prince George's County, MD
                          "24033", # Prince George's County, MD
                          "51013", # Arlington County, VA
                          "51059", # Fairfax County, VA
                          "51510") ) %>% # Alexandria City, VA
    mutate( STATE = ifelse( STATEFP == "51", "VA",
                            ifelse( STATEFP == "24", "MD", NA ) ),
            NAMELSAD = ifelse( NAMELSAD == "Alexandria city", "Alexandria City", NAMELSAD ),
            LABEL_CTY = paste0( NAMELSAD, " (", STATE, ")" ) ) %>%
    mutate( LABEL_CTY = add_new_lines( dat = .$LABEL_CTY, n = 2 ) ) %>% 
    st_as_sf() %>% 
    st_transform( crs = crs.projec )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (2.5.0) Get 2022 Ward Shapefiles (SDLU TIGER File) ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  ## 2022 Redistricting
  
  yr <- 2022
  
  url.sldu <- paste0( "https://www2.census.gov/geo/tiger/TIGER", yr,
                      "/SLDU/tl_", yr, "_11_sldu.zip" ) # 11 is DC State FIPS code
  
  # download file into repository
  file.nm.zip <- paste0( "tl_", yr, "_11_sldu.zip" )
  file.nm.out <- paste0( "tl_", yr, "_11_sldu" )
  file.nm.shp <- paste0( "tl_", yr, "_11_sldu/",
                         paste0( "tl_", yr, "_11_sldu.shp" ) )
  
  dir.create( paste0( "./data/", "/data-public/dc-ward-shapefiles/" ) )
  
  download.file( url.sldu, destfile = paste0( "./data/", "/data-public/dc-ward-shapefiles/",
                                              file.nm.zip ) )
  
  # unzip file in repository
  unzip( paste0( "./data/", "/data-public/dc-ward-shapefiles/", file.nm.zip ), 
         exdir = paste0( "./data/", "/data-public/dc-ward-shapefiles/",
                         file.nm.out ) )
  
  # remove compressed file from download
  file.remove( paste0( "./data/", "/data-public/dc-ward-shapefiles/",
                       file.nm.zip ) )
  
  # read in shapefile
  dc.ward22 <- sf::st_read( paste0( "./data/", "/data-public/dc-ward-shapefiles/",
                                    file.nm.shp ) ) %>% 
    st_as_sf() %>% 
    st_transform( crs = crs.projec )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (2.6.0) Get 2012 Ward Shapefiles (SDLU TIGER File) ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  ## 2012 Redistricting
  
  yr <- 2012
  
  url.sldu <- paste0( "https://www2.census.gov/geo/tiger/TIGER", yr,
                      "/SLDU/tl_", yr, "_11_sldu.zip" ) # 11 is DC State FIPS code
  
  # download file into repository
  file.nm.zip <- paste0( "tl_", yr, "_11_sldu.zip" )
  file.nm.out <- paste0( "tl_", yr, "_11_sldu" )
  file.nm.shp <- paste0( "tl_", yr, "_11_sldu/",
                         paste0( "tl_", yr, "_11_sldu.shp" ) )
  
  download.file( url.sldu, destfile = paste0( "./data/", "/data-public/dc-ward-shapefiles/",
                                              file.nm.zip ) )
  
  # unzip file in repository
  unzip( paste0( "./data/", "/data-public/dc-ward-shapefiles/", file.nm.zip ), 
         exdir = paste0( "./data/", "/data-public/dc-ward-shapefiles/",
                         file.nm.out ) )
  
  # remove compressed file from download
  file.remove( paste0( "./data/", "/data-public/dc-ward-shapefiles/",
                       file.nm.zip ) )
  
  # read in shapefile
  dc.ward12 <- sf::st_read( paste0( "./data/", "/data-public/dc-ward-shapefiles/",
                                    file.nm.shp ) ) %>% 
    st_as_sf() %>% 
    st_transform( crs = crs.projec )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (2.7.0) Save Shapefiles Data for Internal Use ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  save( linear.water.dc.md.va, # liner water shapefiles
        area.water.dc.md.va, # area water shapefile
        dc.st, # dc boundary shapefile
        dc.surr.counties, # surrounding counties shapefile
        dc.ward12, # 2012 ward boundaries shapefile
        dc.ward22, # 2022 ward boundaries shapefile
        file = 'R/sysdata.rda',
        compress = 'bzip2')
  
  # remove folder with raw data since it won't be needed
  unlink( paste0( getwd(), "/data/data-public/" ), force = TRUE )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
}
