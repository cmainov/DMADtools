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
#' @param geo One of "ward", "zcta", "tract', or "block". The geography that is desired.
#' @param id Column name in `d` that contains the ward, census block, or ZCTA identifier.
#' @param var Column name in `d` that contains the variable to be plotted.
#' @param bypass A logical. If data supplied in `d` is already in aggregate form ready for mapping, then the function will only map the values supplied. Default is FALSE.
#' @param metric A vector of string(s). One of: "percent", "mean", "median", "rate", "count". Default if "percent"
#' @param pop.var A string or `NULL`. Variable in `d` with population count data for computing rates if "rate" is selected in `metric`. This is normally an aggregate variable. Default is NULL.
#' @param percentages.rel A string. One of "dc" or "geo". If `metric` is "percent", then this is used to specify denominator for the percentage. "dc" uses `nrow(d)` as the denominator while the "geo" option uses geography-specific (e.g., ward- or tract-specific) totals. Default is NULL.
#' @param per A numeric. The denominator "per" value to use for computing rates. Default is per `1000` (e.g., 25 births per 1,000 individuals).

# rate.supp
# count.supp

dc_mapr <- function( d, geo, id, bypass = FALSE,
                     metric = "percent", pop.var = NULL, percentages.rel = NULL,
                     per = 1000 ){
  
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
      

    }
  }
  
  