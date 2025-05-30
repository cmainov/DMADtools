###------------------------------###
###  Example Data Generation     ###
###------------------------------###

library( tidyverse )
library( sf )

## NOTE: use `usethis::use_data_raw()` to create the data-raw folder and write to .Rbuildignore

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we generate some fake example data for use in the examples
# of the different functions in this package.
# 
# --------------------------------------------------------------------------------------------------------------------------------------------------------

source( "data-R/utils.R" )

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
  linear.water.download <- Map( function( yr, fips ){
    
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
  ) 
  
  linear.water.dc.md.va <- linear.water.download %>% 
    do.call( "rbind", . ) %>% 
    mutate( LABEL_WATER = ifelse( GEOID == "11001" & str_detect( FULLNAME, "Potomac R" ),
                                  "Potomac River",
                                  ifelse( GEOID == "11001" & str_detect( FULLNAME, "Anacostia R" ),
                                          "Anacostia River", NA ) ) ) %>% 
    st_as_sf() %>% 
    st_transform( crs = crs.projec )
  
  unlink( paste0( "./data/", "/data-public/linear-hydrology-shapefiles/" ),
          recursive = TRUE )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (2.2.0) Area Hydrology/Waterways Shapefiles ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # this query is by county FIPS code and so we loop with `Map`
  area.water.download <- Map( function( yr, fips ){
    
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
      mutate( GEOID = fips,
              STATE = str_sub( fips, start = 1, end = 2 ) )
    
  }, yr = yr, fips = fips.to.use
  ) 
  
  area.water.dc.md.va <- area.water.download %>%
    do.call( "rbind", . ) %>% 
    mutate( LABEL_WATER = ifelse( GEOID == "11001" & str_detect( FULLNAME, "Potomac R" ),
                                  "Potomac River",
                                  ifelse( GEOID == "11001" & str_detect( FULLNAME, "Anacostia R" ),
                                          "Anacostia River", NA ) ) ) %>% 
    st_as_sf() %>% 
    st_transform( crs = crs.projec ) %>% 
    # nudging
    filter( MTFCC == "H3010" ) %>% # only rivers and creeks
    mutate( label.water.txtsize = ifelse( LABEL_WATER == "Potomac River", 2.9,
                                          ifelse( LABEL_WATER == "Anacostia River", 1.8, NA ) ),
            label.water.txtcol= ifelse( LABEL_WATER == "Potomac River", "gray50",
                                        ifelse( LABEL_WATER == "Anacostia River", "gray40", NA ) ),
            label.water.nudge.y = ifelse( LABEL_WATER == "Potomac River", -0.03,
                                          ifelse( LABEL_WATER == "Anacostia River", -0.0108, NA ) ),
            label.water.nudge.x = ifelse( LABEL_WATER == "Potomac River", 0.0029,
                                          ifelse( LABEL_WATER == "Anacostia River", -0.018, NA ) ),
            label.water.angle = ifelse( LABEL_WATER == "Potomac River", 78,
                                        ifelse( LABEL_WATER == "Anacostia River", 31.4, NA ) ) ) %>%
    # remove some waterways from following counties to make map less messy
    filter( !GEOID %in% c( "51013", # Arlington County, VA
                           "51059", # Fairfax County, VA
                           "51510" ) ) # Alexandria City
  
  unlink( paste0( "./data/", "/data-public/area-hydrology-shapefiles/" ),
          recursive = TRUE )
  
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
  
  unlink( paste0( "./data/", "/data-public/dc-boundary-shapefiles/" ),
          recursive = TRUE )
  
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
    st_transform( crs = crs.projec ) %>%
    # nudging of labels
    mutate( 
      label.cty = paste0( NAMELSAD, " (", STATE, ")" ),
      label.cty.nudge.y = ifelse( str_detect( NAMELSAD, "Arlington" ),
                                  -0.015, 
                                  ifelse( str_detect( NAMELSAD, "Montgomery" ),
                                          -0.162, 
                                          ifelse( str_detect( NAMELSAD, "Prince George" ),
                                                  -0.01,
                                                  0 ) ) ),
      label.cty.nudge.x = ifelse( str_detect( NAMELSAD, "Montgomery" ),
                                  0.147,
                                  ifelse( str_detect( NAMELSAD, "Prince George" ),
                                          -0.1, 
                                          ifelse( str_detect( NAMELSAD, "Arlington" ),
                                                  0.01,
                                                  0 ) ) ) )%>%
    mutate( label.cty = add_new_lines( dat = .$label.cty, n = 2 ) )
  
  unlink( paste0( "./data/", "/data-public/surrounding-counties-shapefiles/" ),
          recursive = TRUE )
  
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
    st_transform( crs = crs.projec ) %>% 
    # nudging
    mutate( ., labelward.nudge.y = ifelse( NAMELSAD == "Ward 1", -0.001,
                                           ifelse( NAMELSAD == "Ward 2", 0.01,
                                                   ifelse( NAMELSAD == "Ward 3", 0,
                                                           ifelse( NAMELSAD == "Ward 4", 0,
                                                                   ifelse( NAMELSAD == "Ward 5", 0,
                                                                           ifelse( NAMELSAD == "Ward 6", 0.015, 
                                                                                   ifelse( NAMELSAD == "Ward 7", 0, 
                                                                                           ifelse( NAMELSAD == "Ward 8", 0.015, 
                                                                                                   0 )))))))),
            labelward.nudge.x = ifelse( NAMELSAD == "Ward 1", -0.001,
                                        ifelse( NAMELSAD == "Ward 2", 0.005,
                                                ifelse( NAMELSAD == "Ward 3", 0,
                                                        ifelse( NAMELSAD == "Ward 4", 0.01,
                                                                ifelse( NAMELSAD == "Ward 5", 0,
                                                                        ifelse( NAMELSAD == "Ward 6", 0.02, 
                                                                                ifelse( NAMELSAD == "Ward 7", 0.01, 
                                                                                        ifelse( NAMELSAD == "Ward 8", 0.019, 
                                                                                                0 )))))))),
            labelward.segcolor = ifelse( mar_ward %in% c( paste0( "Ward ", 1:8 ) ), "transparent",
                                         "navyblue" ) ) 
  
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
    st_transform( crs = crs.projec ) %>% 
    # nudging
    mutate( ., labelward.nudge.y = ifelse( NAMELSAD == "Ward 1", -0.002,
                                           ifelse( NAMELSAD == "Ward 2", 0.01,
                                                   ifelse( NAMELSAD == "Ward 3", 0,
                                                           ifelse( NAMELSAD == "Ward 4", 0,
                                                                   ifelse( NAMELSAD == "Ward 5", 0,
                                                                           ifelse( NAMELSAD == "Ward 6", 0.005, 
                                                                                   ifelse( NAMELSAD == "Ward 7", 0, 
                                                                                           ifelse( NAMELSAD == "Ward 8", 0.015, 
                                                                                                   0 )))))))),
            labelward.nudge.x = ifelse( NAMELSAD == "Ward 1", 0.001,
                                        ifelse( NAMELSAD == "Ward 2", 0,
                                                ifelse( NAMELSAD == "Ward 3", 0,
                                                        ifelse( NAMELSAD == "Ward 4", 0.01,
                                                                ifelse( NAMELSAD == "Ward 5", 0,
                                                                        ifelse( NAMELSAD == "Ward 6", -0.005, 
                                                                                ifelse( NAMELSAD == "Ward 7", 0, 
                                                                                        ifelse( NAMELSAD == "Ward 8", 0.015, 
                                                                                                0 )))))))),
            labelward.segcolor = ifelse( NAMELSAD %in% c( paste0( "Ward ", 1:8 ) ), "transparent",
                                         "navyblue" ) )
  
  unlink( paste0( "./data/", "/data-public/dc-ward-shapefiles/" ),
          recursive = TRUE )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  ## (2.7.0) Get 2024 ZCTA Shapefiles (ZCTA TIGER File) ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  options( timeout = 400 ) # increasing timeout to 400s since the ZCTA file is very large
  
  yr <- 2024
  
  url.zcta <- paste0( "https://www2.census.gov/geo/tiger/TIGER", yr,
                      "/ZCTA520/tl_", yr, "_us_zcta520.zip" ) # 11 is DC State FIPS code
  
  # download file into repository
  file.nm.zip <- paste0( "tl_", yr, "_us_zcta.zip" )
  file.nm.out <- paste0( "tl_", yr, "_us_zcta" )
  file.nm.shp <- paste0( "tl_", yr, "_us_zcta/",
                         paste0( "tl_", yr, "_us_zcta520.shp" ) )
  
  dir.create( paste0( "./data/", "/data-public/dc-zcta-shapefiles/" ) )
  
  download.file( url.zcta, destfile = paste0( "./data/", "/data-public/dc-zcta-shapefiles/",
                                              file.nm.zip ) )
  
  # unzip file in repository
  unzip( paste0( "./data/", "/data-public/dc-zcta-shapefiles/", file.nm.zip ), 
         exdir = paste0( "./data/", "/data-public/dc-zcta-shapefiles/",
                         file.nm.out ) )
  
  # remove compressed file from download
  file.remove( paste0( "./data/", "/data-public/dc-zcta-shapefiles/",
                       file.nm.zip ) )
  
  # read in shapefile
  dc.zcta <- sf::st_read( paste0( "./data/", "/data-public/dc-zcta-shapefiles/",
                                    file.nm.shp ) ) %>% 
    st_as_sf() %>% 
    st_transform( crs = crs.projec ) %>% 
    # now spatially intersect with DC boundary to get only ZCTAs needed for map
    st_intersection( dc.st, dc.zcta %>% 
                       filter( str_sub( GEOID20, start = 1, end = 2 )
                               %in% c( "20", "56" ) ) )
  
  unlink( paste0( "./data/", "/data-public/dc-zcta-shapefiles/" ),
          recursive = TRUE )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ## (2.8.0) Save Shapefiles Data for Internal Use ##
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  usethis::use_data( linear.water.dc.md.va, # liner water shapefiles
                     area.water.dc.md.va, # area water shapefile
                     dc.st, # dc boundary shapefile
                     dc.surr.counties, # surrounding counties shapefile
                     dc.ward12, # 2012 ward boundaries shapefile
                     dc.ward22, # 2022 ward boundaries shapefile
                     dc.zcta,
                     internal = TRUE,
                     overwrite = TRUE )
  
  # remove folder with raw data since it won't be needed
  unlink( paste0( "./data/", "/data-public/" ),
          recursive = TRUE )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
}


## (2.8.0) Ward Dataset for Examples and Tests ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# obs
nd <- 500
d.ward <- data.frame( id = 1:nd )

# add ward variable (equal probability for each ward)
d.ward$ward <- sample( paste0( "Ward ", 1:8 ), size = nd, replace = TRUE )

table( d.ward$ward ) # check distribution

# add continuous variable (simulate age)
d.ward$age <- { 
  
  vapply( d.ward$ward, FUN = function(x){
    
    mn.ag <- switch( x, # assign mean ages for each ward
                     "Ward 1" = 48,
                     "Ward 2" = 42,
                     "Ward 3" = 43,
                     "Ward 4" = 40,
                     "Ward 5" = 41,
                     "Ward 6" = 38,
                     "Ward 7" = 34,
                     "Ward 8" = 32 )
    
    ag <- truncnorm::rtruncnorm( n = 1, a = 2, b = 120, mean = mn.ag, sd = 15 )
    
    return( ag)
    
  }, FUN.VALUE = c(1), 
  USE.NAMES = FALSE ) 
  
}

# add categorical variable
d.ward$cat <- { 
  
  vapply( d.ward$ward, FUN = function(x){
    
    pr.ag <- switch( x, # assign mean ages for each ward
                     "Ward 1" = c( 0.54, 0.32, 0.21 ),
                     "Ward 2" = c( 0.21, 0.59, 0.18 ),
                     "Ward 3" = c( 0.33, 0.32, 0.33 ),
                     "Ward 4" = c( 0.65, 0.17, 0.17 ),
                     "Ward 5" = c( 0.15, 0.27, 0.67 ),
                     "Ward 6" = c( 0.85, 0.1, 0.05 ),
                     "Ward 7" = c( 0.75, 0.2, 0.05 ),
                     "Ward 8" = c( 0.75, 0.2, 0.05 ) )
    
    ag.cat <- sample( x = c( "Cat 1", "Cat 2", "Cat 3" ), size = 1,
                      replace = TRUE,
                      prob = pr.ag )
    
    return( ag.cat )
    
  }, FUN.VALUE = "Char", 
  USE.NAMES = FALSE ) 
  
}

# add binary character variable
d.ward$bin <- { 
  
  vapply( d.ward$ward, FUN = function(x){
    
    pr.bin <- switch( x, # assign mean ages for each ward
                      "Ward 1" = c( 0.54, 0.46 ),
                      "Ward 2" = c( 0.21, 0.78 ),
                      "Ward 3" = c( 0.31, 0.69 ),
                      "Ward 4" = c( 0.65, 0.35 ),
                      "Ward 5" = c( 0.15, 0.85 ),
                      "Ward 6" = c( 0.85, 0.15 ),
                      "Ward 7" = c( 0.80, 0.2 ),
                      "Ward 8" = c( 0.82, 0.18 ) )
    
    ag.bin <- sample( x = c( "Binary 1", "Binary 2" ), size = 1,
                      replace = TRUE,
                      prob = pr.bin )
    
    return( ag.bin )
    
  }, FUN.VALUE = "Char", 
  USE.NAMES = FALSE ) 
  
}

# add binary 1/0 variable
d.ward$bin_other <- { 
  
  vapply( d.ward$ward, FUN = function(x){
    
    pr.bin <- switch( x, # assign mean ages for each ward
                      "Ward 1" = c( 0.44, 0.56 ),
                      "Ward 2" = c( 0.31, 0.68 ),
                      "Ward 3" = c( 0.21, 0.79 ),
                      "Ward 4" = c( 0.45, 0.55 ),
                      "Ward 5" = c( 0.05, 0.95 ),
                      "Ward 6" = c( 0.55, 0.45 ),
                      "Ward 7" = c( 0.60, 0.4 ),
                      "Ward 8" = c( 0.82, 0.18 ) )
    
    ag.bin <- sample( x = c( 1, 0 ), size = 1,
                      replace = TRUE,
                      prob = pr.bin )
    
    return( ag.bin )
    
  }, FUN.VALUE = c(1), 
  USE.NAMES = FALSE ) 
  
}

# add population variable (distinct to levels of `ward`)
d.ward$ward_pop <- { 
  
  vapply( d.ward$ward, FUN = function(x){
    
    pr.pop <- switch( x, # assign mean ages for each ward
                      "Ward 1" = 84000,
                      "Ward 2" = 78000,
                      "Ward 3" = 85000,
                      "Ward 4" = 86000,
                      "Ward 5" = 87000,
                      "Ward 6" = 91000,
                      "Ward 7" = 80000,
                      "Ward 8" = 86000 )
    
    return( pr.pop )
    
  }, FUN.VALUE = c(1), 
  USE.NAMES = FALSE ) 
  
}

# ensure population variable is distinct
d.ward %>% 
  distinct( ward, ward_pop )

# --------------------------------------------------------------------------------------------------------------------------------------------------------


## (2.9.0) Save example data for export ##
# --------------------------------------------------------------------------------------------------------------------------------------------------------

usethis::use_data( d.ward, overwrite = TRUE )

# --------------------------------------------------------------------------------------------------------------------------------------------------------


## (2.10.0) Ward Dataset for Examples and Tests ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# obs
nd <- 500
d.zcta <- data.frame( id = 1:nd )

# possible ZCTAs
dc.zcta
# add zcta variable (equal probability for each zcta)
d.zcta$zcta <- sample( paste0( "zcta ", 1:8 ), size = nd, replace = TRUE )

table( d.zcta$zcta ) # check distribution

# add continuous variable (simulate age)
d.zcta$age <- { 
  
  vapply( d.zcta$zcta, FUN = function(x){
    
    mn.ag <- switch( x, # assign mean ages for each zcta
                     "zcta 1" = 48,
                     "zcta 2" = 42,
                     "zcta 3" = 43,
                     "zcta 4" = 40,
                     "zcta 5" = 41,
                     "zcta 6" = 38,
                     "zcta 7" = 34,
                     "zcta 8" = 32 )
    
    ag <- truncnorm::rtruncnorm( n = 1, a = 2, b = 120, mean = mn.ag, sd = 15 )
    
    return( ag)
    
  }, FUN.VALUE = c(1), 
  USE.NAMES = FALSE ) 
  
}

# add categorical variable
d.zcta$cat <- { 
  
  vapply( d.zcta$zcta, FUN = function(x){
    
    pr.ag <- switch( x, # assign mean ages for each zcta
                     "zcta 1" = c( 0.54, 0.32, 0.21 ),
                     "zcta 2" = c( 0.21, 0.59, 0.18 ),
                     "zcta 3" = c( 0.33, 0.32, 0.33 ),
                     "zcta 4" = c( 0.65, 0.17, 0.17 ),
                     "zcta 5" = c( 0.15, 0.27, 0.67 ),
                     "zcta 6" = c( 0.85, 0.1, 0.05 ),
                     "zcta 7" = c( 0.75, 0.2, 0.05 ),
                     "zcta 8" = c( 0.75, 0.2, 0.05 ) )
    
    ag.cat <- sample( x = c( "Cat 1", "Cat 2", "Cat 3" ), size = 1,
                      replace = TRUE,
                      prob = pr.ag )
    
    return( ag.cat )
    
  }, FUN.VALUE = "Char", 
  USE.NAMES = FALSE ) 
  
}

# add binary character variable
d.zcta$bin <- { 
  
  vapply( d.zcta$zcta, FUN = function(x){
    
    pr.bin <- switch( x, # assign mean ages for each zcta
                      "zcta 1" = c( 0.54, 0.46 ),
                      "zcta 2" = c( 0.21, 0.78 ),
                      "zcta 3" = c( 0.31, 0.69 ),
                      "zcta 4" = c( 0.65, 0.35 ),
                      "zcta 5" = c( 0.15, 0.85 ),
                      "zcta 6" = c( 0.85, 0.15 ),
                      "zcta 7" = c( 0.80, 0.2 ),
                      "zcta 8" = c( 0.82, 0.18 ) )
    
    ag.bin <- sample( x = c( "Binary 1", "Binary 2" ), size = 1,
                      replace = TRUE,
                      prob = pr.bin )
    
    return( ag.bin )
    
  }, FUN.VALUE = "Char", 
  USE.NAMES = FALSE ) 
  
}

# add binary 1/0 variable
d.zcta$bin_other <- { 
  
  vapply( d.zcta$zcta, FUN = function(x){
    
    pr.bin <- switch( x, # assign mean ages for each zcta
                      "zcta 1" = c( 0.44, 0.56 ),
                      "zcta 2" = c( 0.31, 0.68 ),
                      "zcta 3" = c( 0.21, 0.79 ),
                      "zcta 4" = c( 0.45, 0.55 ),
                      "zcta 5" = c( 0.05, 0.95 ),
                      "zcta 6" = c( 0.55, 0.45 ),
                      "zcta 7" = c( 0.60, 0.4 ),
                      "zcta 8" = c( 0.82, 0.18 ) )
    
    ag.bin <- sample( x = c( 1, 0 ), size = 1,
                      replace = TRUE,
                      prob = pr.bin )
    
    return( ag.bin )
    
  }, FUN.VALUE = c(1), 
  USE.NAMES = FALSE ) 
  
}

# add population variable (distinct to levels of `zcta`)
d.zcta$zcta_pop <- { 
  
  vapply( d.zcta$zcta, FUN = function(x){
    
    pr.pop <- switch( x, # assign mean ages for each zcta
                      "zcta 1" = 84000,
                      "zcta 2" = 78000,
                      "zcta 3" = 85000,
                      "zcta 4" = 86000,
                      "zcta 5" = 87000,
                      "zcta 6" = 91000,
                      "zcta 7" = 80000,
                      "zcta 8" = 86000 )
    
    return( pr.pop )
    
  }, FUN.VALUE = c(1), 
  USE.NAMES = FALSE ) 
  
}

# ensure population variable is distinct
d.zcta %>% 
  distinct( zcta, zcta_pop )

# --------------------------------------------------------------------------------------------------------------------------------------------------------
