###---------------------------------------------------------------
###  `dc_mapr`: Generate Plain or Chropleth Map of DC
###---------------------------------------------------------------

#' @title Generate a plain or choropleth map of Washington, D.C. 
#'
#' @description Create a "nice" publication-level map of Washington, D.C. by ward (2012 or 2022 redistricting),
#' Zip-Code Tabulation Area (ZCTA), Census tract, or block.
#'
#' @details
#' Attribute data in `d` (see below) should be organized by ward, ZIP-code tabulation area (ZCTA), census tract, or block. 
#' @param d A data frame or tibble with the attribute data used to fill the shapes.
#' @param geo One of "ward 2022", "ward 2012", "zcta", "tract', or "block". The geography that is desired.
#' @param id Column name in `d` that contains the ward, census block, or ZCTA identifier.
#' @param var Column name in `d` that contains the variable to be plotted.
#' @param bypass A logical. If data supplied in `d` is already in aggregate form ready for mapping, then the function will only map the values supplied. Default is FALSE.
#' @param metric A vector of string(s). One of: "percent", "mean", "median", "rate", "count". Default if "percent"
#' @param pop.var A string or `NULL`. Variable in `d` with population count data for computing rates if "rate" is selected in `metric`. This is normally an aggregate variable. Default is NULL.
#' @param percentages.rel A string. One of "dc" or "geo". If `metric` is "percent", then this is used to specify denominator for the percentage. "dc" uses `nrow(d)` as the denominator while the "geo" option uses geography-specific (e.g., ward- or tract-specific) totals. Default is NULL.
#' @param per A numeric. The numerator "per" value to use for computing rates. Default is per `1000` (e.g., 25 births per 1,000 individuals).
#' @param colorbar.bins An integer. Number of bins for the fill colorbar. Default is 5.
#' @param colorbar.round A multiple of 10. Used for rounding colorbar labels. Default is 1.
#' @param colorbar.high A Hex color code for the fill gradient. The value used for the "high" end of the scale. Default is "#7a1315".
#' @param colorbar.low A Hex color code for the fill gradient. The value used for the "low" end of the scale. Default is "#f4e2d9".
#' @param colorbar.h Colorbar height. Default is 0.6.
#' @param colorbar.w Colorbar width Default is 10.
#' @param colorbar.direction  Colorbar direction. Default is "horizontal".
#' @param colorbar.position  Colorbar label position Default is "bottom".
#' @param text.color Text color. Default is "black".
#' @param alt.text.color An alternate text color, for when fill color and text color are too similar. Default is "grey".
#' @param font.family A string with the font desired for text on the map. Default is "Calibri Light".
#' @param color.thres A numeric. A threshold value (in [0,1]) for judging similarity of fill color and text color. Default is 0.4
#' @param include.compass A logical. Include reference compass rose on map? Default is TRUE.
#' @param include.scale A logical. Include reference scale rose on map? Default is TRUE.
#' @param size.scale.title A numeric. Scaling parameter for title sizing on map.
#' @param size.scale.labels A numeric. Scaling parameter for label sizing on map.
#' @param legend.top.margin Passed to `ggplot2::theme()`. Numeric for sizing top margin of legend.
# rate.supp
# count.supp
#' 
#' @import sf
#' @import dplyr
#' @import ggplot2
#' @import ggpattern
#' @import eRTG3D
#' @import tidyr
#' @import ggspatial
#' @export

dc_mapr <- function( d, geo, id, bypass = FALSE,
                     metric = "percent", pop.var = NULL, percentages.rel = NULL,
                     per = 1000, colorbar.bins = 5, colorbar.round = 1,
                     colorbar.high = "#7a1315", colorbar.low = "#f4e2d9",
                     colorbar.h = 0.6, colorbar.w = 10, colorbar.position = "bottom", 
                     colorbar.direction = "horizontal", text.color = "black", 
                     alt.text.color = "grey", font.family = "Calibri Light", 
                     include.compass = TRUE, include.scale = TRUE, size.scale.title = 2,
                     size.scale.labels = 0.7, legend.top.margin = -15  ){
  
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
      
      message( "Checking qualities of `var`..." )
      
      if( inherits( d[[ var ]], "character" ) | # ok for character variable class
          inherits( d[[ var ]], "factor" ) | # ok for factor variable class
          ( all( levels( as.factor( d[[ var ]] ) ) %in% c( 1, 0 ) ) & # if it's an integer with 1/0 values
            length( levels( as.factor( d[[ var ]] ) ) ) == 2 ) ){
        
        message( "...`var" )
        
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
        
        ## final aggregation step (1. find denominator and 2. compute metric)
        
        # constant to multiply numerator of metric by (depends on metric type)
        numer <- if( metric == "percent" ) 100 else if( metric == "rate" ) per
        
        d.aggr <- d.agg %>% 
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
    
    if( sum( is.na( d.agg$denom ) > 0 ) ){ warning( paste0( "NAs detected for denominator in at least one level of `", 
                                                            id, "`" ) )
    }
    
    message( "...Data aggregation DONE" )
    
  } else if( bypass ){
    
    d.aggr <- d # if data are already preaggregated, skip all steps above
  }
  
  
  ### (2.0) Merge attributes data to shapefile ###
  
  message( "Calling shapefiles and merging attributes data..." )
  
  ## call shapefile based on `geo` specification ##
  d.geo <- if( geo == "ward 2022" ){ 
    message( "...Ward (2022 redistricting) shapfiles loaded..." )
    dc.ward22
    
  } else if( geo == "ward 2012" ){
    message( "...Ward (2012 redistricting) shapfiles loaded..." )
    dc.ward12
  }
  
  ## merge shapes with attributes data in `d.aggr` ##
  
  # first if wards are desired
  if( geo %in% c( "ward 2022", "ward 2012" ) ){
    
    # first possible pattern
    ward.pattern1 <- str_detect( levels( as.factor( d.aggr[[ id ]] ) ), 
                                 "^ward\\s\\d+$|^Ward\\s\\d+$" )
    
    if( all( ward.pattern1 ) ){ 
      message( paste0( "...'Ward \\d' naming pattern detected in `", 
                       id, "` variable..." ) )
      
      d.aggr <- d.aggr %>% 
        mutate( !!sym( id ) := firstup( !!sym( id ) ) ) # ensure upper case
    }
    
    # second possible pattern
    ward.pattern2 <- str_detect( levels( as.factor( d.aggr[[ id ]] ) ), 
                                 "^\\d+$" )
    
    if( all( ward.pattern2 ) ){ 
      message( paste0( "...'\\d' naming pattern detected in `", 
                       id, "` variable for ward..." ) )
      
      d.aggr <- d.aggr %>% 
        mutate( !!sym( id ) := str_remove_all( !!sym( id ), "\\s+" ),
                !!sym( id ) := paste0( "Ward ", !!sym( id ) ) )
      
    }
    
    # third possible pattern (spelled out integers)
    ward.pattern3 <- all( levels( as.factor( d.aggr[[ id ]] ) ) %in% 
                            as.character( english::as.english( 1:8 ) ) |
                            levels( as.factor( d.aggr[[ id ]] ) ) %in% 
                            firstup( as.character( english::as.english( 1:8 ) ) ) )
    
    if( all( ward.pattern3 ) ){ 
      message( paste0( "...'Spelled integers' naming pattern detected in `", 
                       id, "` variable for ward..." ) )
      
      d.aggr$id <- vapply( d.aggr$id, FUN = function(x){
        
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
    
    if( all( !c( all( ward.pattern1 ), 
                 all( ward.pattern2 ), 
                 all( ward.pattern3 ) ) ) ){
      stop( "Levels of ward in `id` are in an unrecognizable format. Recommend using 'Ward \\d` naming." )
      
    }
    
    message( "Merging attributes (i.e., `var`) to shapefile for ward." )
    
    
    
  }
  
  ## final merge ##
  d.map <- left_join( d.geo, d.aggr,
                      by = c( "NAMELSAD" = id ) )
  
  message( "Attribute and shapefile merging...DONE" )
  
  
  ### (3.0) Assemble map ###
  
  message( "...Assembling final map..." )
  
  # plot tracts with `sf
  bbox1 <- sf::st_bbox( dc.st ) # bounding box
  
  ## make the breaks for the color bar ##
  no.brks <- round( colorbar.bins, 0 )
  
  brks.nr <- seq( min( d.map$out_metric ), max( d.map$out_metric ),
                  length.out = no.brks ) 
  
  ## round the breaks not equal to the limits to the 1's place using `plyr::round_any`
  brks.nr[ !brks.nr %in% c( min( d.map$out_metric ),
                            max( d.map$out_metric ) ) ] <- plyr::round_any( brks.nr[ !brks.nr %in% c( min( d.map$out_metric ),
                                                                                                      max( d.map$out_metric ) ) ], 
                                                                            colorbar.round ) # round colorbar values to this order of magnitude
  
  # subtitle
  # sub.t <- {if( length( y ) > 1 ){
  #   paste0( min(y), "-", max(y) )
  # } else y }
  
  # first pass the fill aesthetic so we can get exact colors mapped
 p.1 <-  ggplot( data = d.map ) +
    geom_sf( aes( fill = out_metric ) ) + 
    scale_fill_gradient( low = colorbar.low, high = colorbar.high, guide = "colorbar",
                         breaks = brks.nr, labels = pretty.num( brks.nr,
                                                                round.to = 1 ) ) 
  
  # extract colors assigned to each level and compute distance metrics on text vs fill colors
  d.fill <- bind_cols( p.1$data,
                       ggplot_build( p.1 )$data[1][[1]][ "fill" ] ) %>%
    sf::st_drop_geometry() %>% 
    rowwise() %>%
    mutate( color_dist = colorDist( fill, text.color )$p.d ) %>% # compute distance between primary text color and assigned fill
    ungroup() %>%
    mutate( color_text = ifelse( color_dist > color.thres, text.color,
                                 alt.text.color ) ) %>%
    select( NAMELSAD, color_dist, fill, color_text )
  
  
  # continue adding other final layers
  p.1 + geom_sf( data = dc.surr.counties,
                          fill = "antiquewhite",
                          color = "gray67") +
    geom_sf( data = area.water.dc.md.va, fill = "aliceblue",
             color = "gray67" ) +
    geom_sf( data = dc.st, fill = "transparent") + # relayer the ward boundaries with transparent fill
    # bounding box coordinates
    ggpattern::scale_pattern_manual( values = "stripe" ) +
    coord_sf( xlim = c( bbox1[["xmin"]], bbox1[["xmax"]] ), # min & max of x values
              ylim = c( bbox1[["ymin"]], bbox1[["ymax"]] ) ) +
    
    xlab("") +
    ylab("") +
    theme( text = element_text( family = font.family ),
           legend.title = element_blank(),
           axis.text = element_blank(),
           legend.position = colorbar.position,
           axis.ticks = element_blank(),
           plot.subtitle = element_text( face = "italic", size = (9 + size.scale.title) ) ) +
    { if( colorbar.position == "bottom" ) theme( legend.margin = margin( t = legend.top.margin ) ) } +
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
                                                                pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                                                                style = north_arrow_fancy_orienteering, width = unit(0.73, "cm"), 
                                                                height = unit(1.0, "cm") ) } } +
    { if( include.scale ){ ggspatial::annotation_scale( height = unit(0.14, "cm"), 
                                                        style = "bar",
                                                        unit_category = "imperial",
                                                        location = "br" ) } }
}




