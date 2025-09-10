###---------------------------------------------------------------
###  `dc_mapr`: Generate Plain or Chropleth Map of DC
###---------------------------------------------------------------

#' @md
#' @title Generate a plain or choropleth map of Washington, D.C. 
#'
#' @description Create a "nice" publication-level map of Washington, D.C. by ward (2012 or 2022 redistricting),
#' Zip-Code Tabulation Area (ZCTA), Census tract, or block.
#'
#' # Citation for Shapefiles
#' U.S. Census Bureau, “2024 TIGER/Line Shapefiles”, 2024,
#' <https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html>, 
#' accessed on May 29, 2025.
#' 
#' @import sf
#' @import dplyr
#' @import ggplot2
#' @import ggpattern
#' @import eRTG3D
#' @import tidyr
#' @import ggspatial
#' @importFrom ggsflabel geom_sf_text_repel
#' @import english
#' @importFrom plyr round_any 
#' 
#' @usage dc_mapr( d, geo, var, id, bypass = FALSE,
#'                      metric = "percent", pop.var = NULL, percentages.rel = "geo",
#'                      per = 1000, count.supp = NULL, rate.supp = NULL, colorbar.bins = 5, 
#'                      colorbar.round = 1, colorbar.high = "#7a1315", colorbar.low = "#f4e2d9",
#'                      colorbar.h = 0.6, colorbar.w = 10, colorbar.position = "bottom", color.surr = "antiquewhite",
#'                      colorbar.direction = "horizontal", colorbar.name = NULL, text.color = "black", 
#'                      alt.text.color = "grey", font.family = "sans", color.thres = 0.4,
#'                      include.compass = TRUE, include.scale = TRUE, size.scale.title = 2,
#'                      size_scale_labels = 0.7, missing.pattern = "stripe", 
#'                      suppressed.pattern = "crosshatch", pattern.spacing = 0.02,
#'                     force = FALSE )
#' @details
#' Attribute data in `d` (see below) should be organized by ward, ZIP-code tabulation area (ZCTA), census tract, or block. 
#' The following DC geographies can be plotted using this function.
#' 
#' | Geography      | `geo` call      | Accepted formats in `id` |
#' | :--------------| :--------------:| :------------------------|
#' | 2022 Wards     | "ward 2022"     | i. "Ward 1"..."Ward 8"<br>ii. "ward 1"..."ward 8"<br>iii. "One"..."Eight"<br>iv. "one"..."eight"  |
#' 
#' @param d A data frame or tibble with the attribute data used to fill the shapes.
#' @param geo One of "ward 2022", "ward 2012", "zcta", "tract', or "block". The geography that is desired.
#' @param id Column name in `d` that contains the ward, census block, or ZCTA identifier.
#' @param var Column name in `d` that contains the variable to be plotted.
#' @param bypass A logical. If data supplied in `d` is already in aggregate form ready for mapping, then the function will only map the values supplied. Default is FALSE.
#' @param metric A vector of string(s). One of: "percent", "mean", "median", "rate", "count". Default if "percent"
#' @param pop.var A string or `NULL`. Variable in `d` with population count data for computing rates if "rate" is selected in `metric`. This is normally an aggregate variable. Default is NULL.
#' @param percentages.rel A string. One of "dc" or "geo". If `metric` is "percent", then this is used to specify denominator for the percentage. "dc" uses `nrow(d)` as the denominator while the "geo" option uses geography-specific (e.g., ward- or tract-specific) totals. Default is "geo".
#' @param per A numeric. The numerator "per" value to use for computing rates. Default is per `1000` (e.g., 25 births per 1,000 individuals).
#' @param count.supp an integer or  `NULL` if suppression of counts below a certain threshold is desired, specify the integer value. Any cells with counts less than or equal to `count.supp` will be suppressed. default is `NULL`.
#' @param rate.supp An integer or  `NULL` if suppression of rates where the count the rate is based off is less than some threshold. Any cells with rates based on counts less than or equal to `rate.supp` will be suppressed. default is `NULL`.
#' @param colorbar.bins An integer. Number of bins for the fill colorbar. Default is 5.
#' @param colorbar.round A multiple of 10. Used for rounding colorbar labels. Default is 1.
#' @param colorbar.high A Hex color code for the fill gradient. The value used for the "high" end of the scale. Default is "#7a1315".
#' @param colorbar.low A Hex color code for the fill gradient. The value used for the "low" end of the scale. Default is "#f4e2d9".
#' @param colorbar.h Colorbar height. Default is 0.6.
#' @param colorbar.w Colorbar width. Default is 10.
#' @param colorbar.direction  Colorbar direction. Default is "horizontal".
#' @param colorbar.position  Colorbar label position. Default is "bottom".
#' @param color.surr Surrounding counties and cities fill color. Default is "antiquewhite".
#' @param colorbar.name A string. Name to show beside the colorbar. Default is NULL (i.e., no name displayed).
#' @param text.color Text color. Default is "black".
#' @param alt.text.color An alternate text color, for when fill color and text color are too similar. Default is "grey".
#' @param font.family A string with the font desired for text on the map. Default is "Calibri Light".
#' @param color.thres A numeric. A threshold value (in (0,1)) for judging similarity of fill color and text color. Default is 0.4
#' @param include.compass A logical. Include reference compass rose on map? Default is TRUE.
#' @param include.scale A logical. Include reference scale rose on map? Default is TRUE.
#' @param size.scale.title A numeric. Scaling parameter for title sizing on map.
#' @param size_scale_labels A numeric. Scaling parameter for label sizing on map.
#' @param missing.pattern A theme from `ggpattern` for polygons with missing data. Default is "stripe". See [ggpattern](https://coolbutuseless.github.io/package/ggpattern/).
#' @param suppressed.pattern A theme from `ggpattern` for polygons with suppressed data. Default is "weave". See [ggpattern](https://coolbutuseless.github.io/package/ggpattern/).
#' @param pattern.spacing A numeric. Passed to `pattern_spacing` argument of`ggpattern::geom_pattern()`. 
#' @param force A logical. Bypass for mismatching in `geo`, where applicable. Default is FALSE.
#' 
#' @examples
#' 
#' library( DMADtools )
#' 
#' dc_mapr(
#'     d = d_ward,
#'     var = "bin_other",
#'     id = "ward",
#'     geo = "ward 2022",
#'     bypass = FALSE,
#'     metric = "percent", pop.var = NULL, 
#'     per = 1000, colorbar.bins = 5, colorbar.round = 1,
#'     colorbar.high = "#7a1315"
#'   )
#' 
#'
#' @export

dc_mapr <- function( d, geo, var, id, bypass = FALSE,
                     metric = "percent", pop.var = NULL, percentages.rel = "geo",
                     per = 1000, count.supp = NULL, rate.supp = NULL, colorbar.bins = 5, 
                     colorbar.round = 1, colorbar.high = "#7a1315", colorbar.low = "#f4e2d9",
                     colorbar.h = 0.6, colorbar.w = 10, colorbar.position = "bottom", color.surr = "antiquewhite",
                     colorbar.direction = "horizontal", colorbar.name = NULL, text.color = "black", 
                     alt.text.color = "grey", font.family = "sans", color.thres = 0.4,
                     include.compass = TRUE, include.scale = TRUE, size.scale.title = 2,
                     size_scale_labels = 0.7, missing.pattern = "stripe", 
                     suppressed.pattern = "crosshatch", pattern.spacing = 0.02,
                     force = FALSE ){

  
  ## checks ##
  if( eRTG3D::is.sf.3d( d ) ) d <- data.frame( d ) # if `sf` object, keep only attributes table
  
  if( is.null( d[[ var ]] ) ){
    stop( "`var` variable not detected in dataset." )
  }
  
  if( is.null( d[[ id ]] ) ){
    stop( "`id` variable not detected in dataset." )
  }
  
  if( metric == "percent" ){
    if( !percentages.rel %in% c( "geo", "dc" ) ){
      stop( "`percentages.rel` must be one of 'geo' or 'dc'" )
    }
  }
  
  if( is.null( pop.var ) & any( stringr::str_detect( metric, "rate" ) ) ){
    stop( "`pop.var` not specified but 'rate' was called in `metric`." )
  }
  
  if( !is.null( pop.var ) ){
    if(is.null( d[[ pop.var ]] ) ){
      stop( "`pop.var` variable not detected in dataset. Ensure that population count data (aggregate-level) are stored in `pop.var` variable." )
    }
  }
  
  if( !is.null( var ) ){
    
    empt <- sapply( var, function(x) "" %in% levels( as.factor( d[[ x ]] ) ) )
    if( any( empt ) ) stop( 'The empty string (i.e., "") was detected in ',
                            names( which( empt ) ),
                            ". Please ensure no variables in `var` have a level as the empty string." )
  }
  
  if( length( var ) > 1 & !is.null( var ) ){
    stop( "There can only be one variable, maximum, specified in `var`." )
  }
  
  if( sum( metric %in% c( "percent", "rate", "count" ) ) != length( metric ) ){
    stop( '`metric` can only be one of the following exact strings: "percent", "rate", "count"' )
  }
  
  if( length( geo ) != 1 | !geo %in% c( "ward 2022", "ward 2012", "zcta", "tract", "block" ) ){
    stop( '`geo` can only be one of the following exact strings: "ward 2022", "ward 2012", "zcta", "tract", "block"' )
  }
  
  
  ## (1.0) Aggregation step if data are not already in aggregate form ##
  
  if( !bypass ){
    
    # percent, count (binary case)
    if( metric %in% c( "percent", "count", "rate" ) ){
      
      message( "Step 1: Checking qualities of `var`..." )
      
      if( inherits( d[[ var ]], "character" ) | # ok for character variable class
          inherits( d[[ var ]], "factor" ) | # ok for factor variable class
          ( all( levels( as.factor( d[[ var ]] ) ) %in% c( 1, 0 ) ) & # if it's an integer with 1/0 values
            length( levels( as.factor( d[[ var ]] ) ) ) == 2 ) ){
        
        # get levels of the variable
        levs.binary <-  d %>% count( !!sym( var ) ) %>% pull( !!sym( var ) )
        
        # case where length( var ) == 2 and is 1/0
        if( all( levs.binary %in% c( 1, 0 ) ) ){
          
          message( "...1/0 format detected for `var`. Using level of 1 as the variable to count..." )
          
          # aggregate by `id` variable and specify denominator
          d.agg <- d %>% 
            reframe( count = sum( !!sym( var ) == 1 ),
                     .by = !!sym( id ) ) 
        }
        
        # case where length( var ) >= 2 but is not 1/0
        if( all( !levs.binary %in% c( 1, 0 ) ) ){
          
          this.ref <- levs.binary[1] # automatically assign first level as `1` if not assigned
          
          message( paste0( "...`var` is of class ",
                           class( d[[ var ]] ),
                           " and not 1/0. Using level of '",
                           this.ref, "' as the variable to count.",
                           " If another level is desired to be mapped, then it",
                           " is recommended to create dummy variables (1/0)..." ) )
          
          # aggregate by `id` variable and specify denominator
          d.agg <- d %>% 
            reframe( count = sum( !!sym( var ) == this.ref ),
                     .by = !!sym( id ) )
        }
        
        ## final aggregation step (1. find denominator and 2. compute metric) ##
        
        # constant to multiply numerator of metric by (depends on metric type)
        numer <- if( metric == "percent" ) 100 else if( metric == "rate" ) per
        
        d_aggr <- d.agg %>% 
          # denominator for when `metric` is "percent"
          { if( metric %in% c( "percent" ) ){
            if( percentages.rel == "dc" ){
              
              message( '..."percent" metric selected, denominator selected is `nrow(d)`...' )
              
              mutate( ., denom = nrow( d ) ) } else 
                if( percentages.rel == "geo" ){
                  
                  message( '..."percent" metric selected, denominator selected is `id` level-specific...' )
                  
                  piped.in <- . # aggregate data coming down pipe
                  
                  reframe( d, denom = n(),
                           .by = !!sym( id ) ) %>% 
                    left_join( piped.in, ., by = id )
                }  } else . } %>% 
          # denominator for when `metric` is "rate"
          { if( metric %in% c( "rate" ) ){
            
            piped.in <- .
            # first check there are 
            message( '..."rate" metric selected, denominator selected is `pop.var`...' )
            
            pop.distinct <- d %>% 
              distinct( !!sym( id ), !!sym( var.pop ) ) %>% 
              rename( denom = !!sym( var.pop ) )
            
            left_join( piped.in, pop.distinct,
                       by = id ) 
            
          } else . } %>% 
          # compute final metric 
          { if( metric %in% c( "rate", "percent" ) ){
            mutate( ., out_metric = ( numer * count ) / denom ) 
          } else if( metric == "count" ){
            rename( ., out_metric = count )
          } else . }
        
      } 
      
    } else stop( "If `metric` is 'percent', 'count', or 'rate' then `var` must be a binary (or dummy) of class character, factor, or a 1/0 integer." )
    
    if( "denom" %in% colnames( d.agg ) ){
      if( sum( is.na( d.agg$denom ) > 0 ) ){ warning( paste0( "NAs detected for denominator in at least one level of `",                                                       id, "`" ) )
      }
    }
    
    message( "...Data aggregation DONE" )
    
  } else if( bypass ){
    
    d_aggr <- d # if data are already preaggregated, skip all steps above
  }
  
  
  ### (2.0) Merge attributes data to shapefile ###
  
  message( "Step 2: Calling shapefiles and merging attributes data..." )
  
  ## call shapefile based on `geo` specification ##
  d_geo <- if( geo == "ward 2022" ){ 
    message( "...Ward (2022 redistricting) shapfile loaded..." )
    join_col <- "NAMELSAD" # join column
    dc_ward22
    
  } else if( geo == "ward 2012" ){
    message( "...Ward (2012 redistricting) shapfile loaded..." )
    join_col <- "NAMELSAD" # join column
    dc_ward12
  } else if( geo == "zcta" ){
    message( "...ZCTA (2024) shapfile loaded..." )
    join_col <- "GEOID20" # join column
    dc_zcta
  }
  
  ## merge shapes with attributes data in `d_aggr` ##
  
  # wards
  if( geo %in% c( "ward 2022", "ward 2012" ) ){
    
    # first possible pattern
    ward_pattern1 <- str_detect( levels( as.factor( d_aggr[[ id ]] ) ), 
                                 "^ward\\s\\d+$|^Ward\\s\\d+$" )
    
    if( all( ward_pattern1 ) ){ 
      message( paste0( "...'Ward \\d' naming pattern detected in `", 
                       id, "` variable..." ) )
      
      d_aggr <- d_aggr %>% 
        mutate( !!sym( id ) := firstup( !!sym( id ) ) ) # ensure upper case
    }
    
    # second possible pattern
    ward_pattern2 <- str_detect( levels( as.factor( d_aggr[[ id ]] ) ), 
                                 "^\\d+$" )
    
    if( all( ward_pattern2 ) ){ 
      message( paste0( "...'\\d' naming pattern detected in `", 
                       id, "` variable for ward..." ) )
      
      d_aggr <- d_aggr %>% 
        mutate( !!sym( id ) := str_remove_all( !!sym( id ), "\\s+" ),
                !!sym( id ) := paste0( "Ward ", !!sym( id ) ) )
      
    }
    
    # third possible pattern (spelled out integers)
    ward_pattern3 <- all( levels( as.factor( d_aggr[[ id ]] ) ) %in% 
                            as.character( english::as.english( 1:8 ) ) |
                            levels( as.factor( d_aggr[[ id ]] ) ) %in% 
                            firstup( as.character( english::as.english( 1:8 ) ) ) )
    
    if( all( ward_pattern3 ) ){ 
      message( paste0( "...'Spelled integers' naming pattern detected in `", 
                       id, "` variable for ward..." ) )
      
      d_aggr$id <- vapply( d_aggr$id, FUN = function(x){
        
        switch( x, # assign mean ages for each ward
                "One" = "Ward 1", "one" = "Ward 1",
                "Two" = "Ward 2", "two" = "Ward 2",
                "Three" = "Ward 3", "three" = "Ward 3",
                "Four" = "Ward 4", "four" = "Ward 4",
                "Five" = "Ward 5", "five" = "Ward 5",
                "Six" = "Ward 6", "six" = "Ward 6",
                "Seven" = "Ward 7", "seven" = "Ward 7",
                "Eight" = "Ward 8", "eight" = "Ward 8" )
        
      }, FUN.VALUE = c("char"), 
      USE.NAMES = FALSE ) 
      
    }
    
    if( all( !c( all( ward_pattern1 ), 
                 all( ward_pattern2 ), 
                 all( ward_pattern3 ) ) ) ){
      stop( "Levels of ward in `id` are in an unrecognizable format. Recommend using 'Ward \\d` naming." )
      
    }
    
    message( "...Merging attributes (i.e., `var`) to shapefile for DC wards..." )
    
  }
  
  # ZCTAs
  if( geo %in% c( "zcta" ) ){
    
    # convert to character
    if( !inherits( d_aggr[[ id ]], "character" ) ){
        d_aggr[[ id ]] <- as.character( d_aggr[[ id ]] )
  
    }
  
    # check for missing ZCTA values
    zctas_miss <- sum( !dc_zcta$GEOID20 %in% d_aggr[[ id ]] )
    zctas_miss.perc <- 100 * zctas_miss / length( dc_zcta$GEOID20 )
    
    if( !force ){
      if( zctas_miss.perc > 50 ) stop( paste( "Over 50% of DC ZCTAs were not detected in the `",
                                              id, "` variable. Ensure that ZCTAs are formatted correctly",
                                              " (e.g., 20009). Use `force = TRUE` to bypass this error." ) )
    }
    
    message( "...Merging attributes (i.e., `var`) to shapefile for DC  zctas_.." )
    
  }
  
  ## final merge ##
  
  d_map <- left_join( d_geo, d_aggr,
                      by = setNames( object = id, nm = join_col  ) ) %>% 
    # add column for missing data (to be able to add pattern to this polygon)
    mutate( missing_suppressed = ifelse( is.na( out_metric ),
                                         "Missing data", NA ) )
  
  message( "...Attribute and shapefile merging...DONE" )
  
  
  ## count or rate suppression, if desired ##
  
  # first, count suppression
  if( !is.null( count.supp ) ){
    if( metric %in% c( "count" )  & !is.null( count.supp ) ){
      
      if( count.supp < 0 ) stop( "count.supp must be >= 0" )
      
      d_map <- d_map %>% 
        mutate( out_metric = ifelse( out_metric <= count.supp, NA, out_metric ),
                missing_suppressed = ifelse( is.na( out_metric ) &
                                               is.na( missing_suppressed ),
                                             "Suppressed", missing_suppressed ) )
      
    }
  }
  
  # second, rate suppression
  if( !is.null( rate.supp ) ){
    if( metric %in% c( "rate" ) ){
      
      if( rate.supp < 0 ) stop( "rate.supp must be >= 0" )
      
      d_map <- d_map %>% 
        mutate( out_metric = ifelse( out_metric <= rate.supp, NA, out_metric ),
                missing_suppressed = ifelse( is.na( out_metric ) &
                                               is.na( missing_suppressed ),
                                             "Suppressed", missing_suppressed ) )
      
    }
  }
    
  ### (3.0) Assemble map ###
  
  message( "Step 3: Assembling final map..." )
  
  # plot tracts with `sf
  bbox1 <- sf::st_bbox( dc_st ) # bounding box
  
  ## make the breaks for the color bar ##
  no_brks <- round( colorbar.bins, 0 )
  
  brks_nr <- seq( min( d_map$out_metric, na.rm = TRUE ), max( d_map$out_metric, na.rm = TRUE ),
                  length.out = no_brks ) 
  
  ## round the breaks not equal to the limits to the 1's place using `plyr::round_any`
  brks_nr[ !brks_nr %in% c( min( d_map$out_metric ),
                            max( d_map$out_metric ) ) ] <- plyr::round_any( brks_nr[ !brks_nr %in% c( min( d_map$out_metric ),
                                                                                                      max( d_map$out_metric ) ) ], 
                                                                            colorbar.round ) # round colorbar values to this order of magnitude
  
  # first pass the fill aesthetic so we can get exact colors mapped
 p_1 <- ggplot( data = d_map ) +
    geom_sf( aes( fill = out_metric ) ) + 
    scale_fill_gradient( name = colorbar.name, low = colorbar.low, high = colorbar.high, 
                         guide = "colorbar", breaks = brks_nr, 
                         labels = pretty_num( brks_nr, round.to = 1 ) ) 
  
  # extract colors assigned to each level and compute distance metrics on text vs fill colors
  d_fill <- bind_cols( p_1$data,
                       ggplot_build( p_1 )$data[1][[1]][ "fill" ] ) %>%
    sf::st_drop_geometry() %>% 
    rowwise() %>%
    mutate( color_dist = colorDist( fill, text.color )$p.d ) %>% # compute distance between primary text color and assigned fill
    ungroup() %>%
    mutate( color_text = ifelse( color_dist > color.thres, text.color,
                                 alt.text.color ) ) %>%
    select( !!sym( join_col ), color_dist, fill, color_text )
  
  # continue adding other final layers
  p_out <- p_1 + 
    # missing values
    geom_sf_pattern( data = d_map %>%
                       filter( !is.na( missing_suppressed ) ),
                     aes( pattern = missing_suppressed ),
                     pattern_spacing = pattern.spacing ) +
    # bounding box coordinates
    ggpattern::scale_pattern_manual( values = c( missing.pattern, 
                                                 suppressed.pattern ),
                                     breaks = c( "Missing data",
                                                 "Suppressed" ),
                                     guide = guide_legend( title = NULL )) +
    geom_sf( data = dc_surr_counties,
                          fill = color.surr,
                          color = "gray67") +
    geom_sf( data = area_water_dc_md_va %>% 
               filter( STATE != "11" ), fill = "aliceblue", 
             color = "gray67" ) + # non-DC states water file (non-transparent border)
    geom_sf( data = area_water_dc_md_va %>% 
               filter( STATE == "11" ), fill = "aliceblue", 
             color = "transparent" ) + # DC water file (transparent border around water shapes)
    geom_sf( data = dc_st, fill = "transparent") + # layer the DC boundary with transparent fill
    geom_sf( data = d_map, fill = "transparent") + # relayer the `geo` boundaries with transparent fill
    coord_sf( xlim = c( bbox1[["xmin"]], bbox1[["xmax"]] ), # sets bounding box
              ylim = c( bbox1[["ymin"]], bbox1[["ymax"]] ) ) +
    xlab("") +
    ylab("") +
    theme( text = element_text( family = font.family ),
           axis.text = element_blank(),
           legend.position = colorbar.position,
           axis.ticks = element_blank(),
           plot.subtitle = element_text( face = "italic", size = (9 + size.scale.title) ) ) +
    { if( colorbar.position == "bottom" ) theme( legend.margin = margin( t = -15 ) ) } +
    guides( fill = guide_colorbar( ticks.colour = NA,
                                   frame.colour =  "black",
                                   barwidth = colorbar.w,
                                   barheight = colorbar.h,
                                   nbin = colorbar.bins,
                                   label.hjust = 1, # left-align labels
                                   show.limits = TRUE,
                                   display = "rectangles",
                                   direction = colorbar.direction,
                                   label.position = colorbar.position ) ) +
    
    { if( include.compass ){ ggspatial::annotation_north_arrow( location = "tr", which_north = "true", 
                                                                pad_x = unit( 0.2, "cm" ), pad_y = unit( 0.2, "cm" ),
                                                                style = north_arrow_fancy_orienteering, width = unit( 0.73, "cm" ), 
                                                                height = unit(1.0, "cm") ) } } +
    { if( include.scale ){ ggspatial::annotation_scale( height = unit( 0.14, "cm" ), 
                                                        style = "bar",
                                                        unit_category = "imperial",
                                                        location = "br" ) } } +
    # Potomac and Anacostia River labels #
     ggsflabel::geom_sf_text_repel( 
      data = area_water_dc_md_va,
      mapping = aes( label = LABEL_WATER ),
      segment.color = "transparent",
      colour = area_water_dc_md_va$label_water_txtcol,
      force = 0, # minimized force of repulsion between labels
      size = area_water_dc_md_va$label_water_txtsize,
      # manual location of ward labels (x and y)
      nudge_y = area_water_dc_md_va$label_water_nudge_y,
      nudge_x = area_water_dc_md_va$label_water_nudge_x,
      angle = area_water_dc_md_va$label_water_angle,
      seed = 34, family = font.family ) +
    # Surrounding County Labels #
    ggsflabel::geom_sf_text_repel( 
      data = dc_surr_counties,
      mapping = aes( label = LABEL_CTY ),
      segment.color = "transparent",
      colour = "gray50",
      force = 0, # minimized force of repulsion between labels
      size = ( 3.1 + size_scale_labels ),
      # manual location of ward labels (x and y)
      nudge_y = dc_surr_counties$label_cty_nudge_y,
      nudge_x = dc_surr_counties$label_cty_nudge_x,
      seed = 34, family = font.family ) +
    ## `geo` polygon labeling ##
    # DC Ward labels #
    { if( str_detect( geo, "ward" ) ){
      ggsflabel::geom_sf_text_repel( 
      mapping = aes( label = NAMELSAD ),
      force = 0, # minimized force of repulsion between labels
      size = ( 3.1 + size_scale_labels ),
      nudge_x = d_map$labelward_nudge_x,
      nudge_y = d_map$labelward_nudge_y,
      segment.color = "transparent",
      colour = d_fill$color_text,
      seed = 34, family = font.family ) 
    }} +
    # ZCTA labels #
    { if( str_detect( geo, "zcta" ) ){
      ggsflabel::geom_sf_text_repel( 
        mapping = aes( label = labelzip ),
        segment.color = d_map$labelzip_segcolor,
        colour = d_map$labelzip_txtcol,
        force = 0, # minimized force of repulsion between labels
        size = d_map$labelzip_txtsize,
        # manual location of ward labels (x and y)
        nudge_y = d_map$labelzip_nudge_y,
        nudge_x = d_map$labelzip_nudge_x,
        seed = 34, family = "Calibri Light" )
    }} 
  
  message( "...Mapping DONE" )
  
  
  ##  messages if rates or counts were suppressed ##
  if( !is.null( count.supp ) ){
    if( metric %in% c( "count" )  & !is.null( count.supp ) ){
      
      message( paste0( "NOTE: suppresion of counts <= ",
                       count.supp, " was requested in the function call. ",
                       " Final map contains suppressed values for at least one polygon." ) )
    }
  }
  
  if( !is.null( rate.supp ) ){
    if( metric %in% c( "rate" )  & !is.null( rate.supp ) ){
      
      message( paste0( "NOTE: suppresion of rates with counts <= ",
                       rate.supp, " was requested in the function call. ",
                       " Final map contains suppressed values for at least one polygon." ) )
    }
  }
  
  return( suppressWarnings( print( p_out ) ) )
  
}





