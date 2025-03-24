###---------------------------------------------------------------
###  `summary_table`: Generate "Nice" `flextable` From Input Data
###---------------------------------------------------------------

#' @title Generate "Nice" `flextable` From Input Data
#'
#' @description Create a "nice" publication-level table by tabulating up to three variables at once.
#'
#' @details
#' Employs `flextable` to generate a "nice" publication-level table that can be easily exported or included in R Rmarkdown (.RMD) files.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import flextable
#' @import eRTG3D
#' @import tibble
#' @import officer
#' @import rlang
#' @importFrom magrittr "%>%"
#'
#' @usage summary_table( d, var1, var2 = NULL, table.grouping = NULL, 
#' pop.var = NULL, add.summary.row = TRUE, summary.row.name = NULL, 
#' add.summary.col = TRUE, digs.perc = 1, digs.rate = 2, summary.col.name = NULL, 
#' order.rows = NULL, order.cols = NULL, order.groups = NULL, foot.lines = c(), 
#' table.title = NULL, metric = c( "count", "rate" ), nm.var1 = NULL, 
#' count.supp = NULL, remove.cols = NULL, rate.supp = NULL, 
#' count.supp.symbol = "--", rate.supp.symbol = "*", per = 1000, 
#' NAs.footnote = FALSE, percentages.rel = "var1", include.percent.sign = TRUE )
#'
#' @param d A data frame or tibble. Data must be the exact subset of data that needs to analyzed for the table generation.
#' @param var1 A string. Name of first variable to stratify on (as it appears in the input data, `d`). Cannot be  `NULL`.
#' @param var2 A string. Name of second variable to stratify on (as it appears in the input data, `d`). Can be `NULL` if only one variable is required and there is no cross-tabulation or if `table.grouping` and `var1` are specified for a cross-tabulation instead.
#' @param metric A vector of string(s). One or two of: "percent", "rate", "count". Can dplyr::select up to two options to plot at maximum.
#' @param table.grouping A string or `NULL` (if no grouping in table is desired). Name of variable to create grouped output table with (creates grouping rows in the table). Can be 1 variable at maximum.
#' @param pop.var A string or `NULL`. Variable in `d` with population count data for computing rates if "rate" is dplyr::selected in `metric`. This is normally an aggregate variable.
#' @param add.summary.col A logical. Add a row with unaggregated results (based on `var2`). Note that this argument is inconsequential if `var2 == NULL`.
#' @param add.summary.row A logical. Add a row with unaggregated results (based on `var1`). `TRUE` is default and the name given to the row is "Summary Row". If another name is desired, it can be specified with `summary.row.name` and its order/position can be specified with the `order.rows` argument.
#' @param summary.col.name A string. Name to appear for summary column header. Default is "All". This option is only relevant if `add.summary.col == TRUE`.
#' @param summary.row.name A string. Name to appear for summary row label. Default is "Summary Row". This option is only relevant if `add.summary.row == TRUE`.
#' @param digs.perc An integer. Digits to round percents to.
#' @param digs.rate An integer. Digits to round rates to.
#' @param order.rows A vector with the levels in custom order of the stratifying variable in the rows(typically `var1` but can also be `table.grouping` depending on how the function is specified). Entries must match levels of stratifying variable exactly. If there are any levels of the row variable missing, they are omitted from the final table. Note that any rows removed from the final table do not remove that level from the data itself. If desired, that must be done outside the function with a data step.
#' @param order.cols A vector with the levels in custom order of the stratifying variable in the columns (typically `var2` but can also be `table.grouping` depending on how the function is specified). Must match the columns in the final table including the summary column name if `add.summary.col == TRUE` and `summary.col.name` is specified. If there are any levels of the column variable missing, they are omitted from the final table.
#' @param order.groups A vector with the levels in custom order of the grouping variable in the columns. Must match the grouping variable (`table.grouping`) names in the final table. if there are any levels of the grouping variable missing, they are omitted from the final table. the orders of the rows within the group are preserved as indicated in `order.rows`. this argument is only valid if `!is.null( var2 ) & !is.null( table.grouping )`.
#' @param foot.lines A vector with entries (strings) corresponding to each footline to be added to the table.
#' @param remove.cols A string. The string is fed to `dplyr::select( -contains( remove.cols` ) )` to remove undesired columns from the final table. Should be based off of columns seen in final generated table.
#' @param table.title A string. Name of table to display as the title. `NULL` is default for no title in the table.
#' @param nm.var1 A string. Name of `var1` that you would like to appear in the table. `NULL` is default, which prompts the column name as it is written in the input dataset, `d`.
#' @param count.supp an integer or  `NULL` if suppression of counts below a certain threshold is desired, specify the integer value. Any cells with counts less than or equal to `count.supp` will be suppressed. default is `NULL`.
#' @param rate.supp An integer or  `NULL` if suppression of rates where the count the rate is based off is less than some threshold. Any cells with rates based on counts less than or equal to `rate.supp` will be suppressed. default is `NULL`.
#' @param count.supp.symbol A string. The character to display in the table where the count is suppressed.
#' @param rate.supp.symbol A string. The character to display in the table where the rate is suppressed.
#' @param per A numeric. The denominator "per" value to use for computing rates. Default is per `1000` (e.g., 25 births per 1,000 individuals).
#' @param NAs.footnote A logical. Include footnotes detailing number of missing values in the dataset based on `var1` and `var2`?
#' @param percentages.rel A string. One of "var1" or "var2" or "table.grouping". Is the variables with which percentages should be calculated with respect to. `table.grouping` can only be specified if only `var1` is specified and `var2 == NULL`.
#' @param include.percent.sign A logical. Include percent (\%) sign in computed percent columns? 
#' 
#' @return Object of class \code{list} containing the following elements:
#'
#' `flextable`: An object of class `flextable` that produces the "nice" flextable.
#' 
#' `frame`: An object of class `data.frame` that contains the dataset fed to `flextable` for final tabulation.
#' 
#' NOTE: In the final output table (`flextable`), levels of `var1` appear in the rows and levels of `var2` appear in the columns if only `var1` and `var2` are specified. If `var1` and `table.grouping`
#' are only specified, then levels of `var1` appear in the columns and levels of `table.grouping` appear in the rows. If `var1`, `var2`, and `table.grouping` are specified, then levels of `var1` 
#' appear in the rows, levels of `var2` appear in the columns, and levels of `table.grouping` appear as nested rows that group the table.
#'
#' @examples
#' 
#' library( DMADtools )
#' 
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                var2 = "v2",
#'                percentages.rel = "var1" )
#' 
#' 
#' # alternate percentages.rel argument
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                var2 = "v2",
#'                percentages.rel = "var2" )
#' 
#' # specify var1 and table.grouping instead of var1 and var2, 
#' # note, the change in percentages.rel            
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                table.grouping = "v2",
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                percentages.rel = "table.grouping" )
#' 
#' # rename summary row and column and shuffle column and row orders
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                table.grouping = "v2",
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                summary.col.name = "All Geos", # change name of summary column
#'                summary.row.name = "All Char", # change name of summary row
#'                order.cols = c( "Geo 1", "Geo 2", "All Geos", "Geo 3" ), # reorder columns in output table
#'                order.rows = c( "Char 1", "Char 2", "All Char", "Char 3" ), # reorder columns in output table
#'                percentages.rel = "table.grouping" )
#' 
#' # same call as previous, but use order.cols and order.rows to remove some rows and columns from final table
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                table.grouping = "v2",
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                summary.col.name = "All Geos", # change name of summary column
#'                summary.row.name = "All Char", # change name of summary row
#'                order.cols = c( "Geo 1", "All Geos", "Geo 3" ), # reorder columns in output table
#'                order.rows = c( "Char 1", "Char 2", "All Char" ), # reorder columns in output table
#'                percentages.rel = "table.grouping" )
#' 
#' # use remove.cols instead of order.cols argument for removing an undesired column from final table
#' summary_table( d = d.example,
#'                metric = c( "count", "rate" ),
#'                var1 = "v1",
#'                var2 = "v2",
#'                add.summary.row =TRUE,
#'                add.summary.col = TRUE,
#'                remove.cols = "Char 3",
#'                percentages.rel = "var2",
#'                pop.var = "v_pop" )
#' 
#' # rename var1 column header
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                nm.var1 = "Variable 1", # for relabeling the header
#'                table.grouping = "v2",
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                summary.col.name = "All Geos", # change name of summary column
#'                summary.row.name = "All Char", # change name of summary row
#'                order.cols = c( "Geo 1", "All Geos", "Geo 3" ), # reorder columns in output table
#'                order.rows = c( "Char 1", "Char 2", "All Char" ), # reorder columns in output table
#'                percentages.rel = "table.grouping" )
#' 
#' # do a 3-way tabulation
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                var2 = "v3",
#'                table.grouping = "v2",
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                percentages.rel = "var2" )
#' 
#' # use order.groups argument to remove some grouping categories
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                var2 = "v3",
#'                table.grouping = "v2",
#'                order.groups = c( "Char 2", "Char 1" ),
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                percentages.rel = "var2" )
#' 
#' # example with rate computation and add.summary.row and add.summary.col are TRUE
#' summary_table( d = d.example,
#'                metric = c( "count", "rate" ),
#'                var1 = "v1",
#'                var2 = "v2",
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                percentages.rel = "var2",
#'                pop.var = "v_pop" )
#' 
#' # foot.lines argument and NAs.footnote
#' set.seed( 974 )
#' 
#' summary_table( d = d.example.na,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                var2 = "v2",
#'                nm.var1 = "Variable 1",
#'                add.summary.row =TRUE,
#'                add.summary.col = TRUE,
#'                foot.lines = c("This is the first footer line",
#'                               "This is the second footer line" ), # manual footer lines
#'                remove.cols = "Char 3",
#'                percentages.rel = "var2",
#'                NAs.footnote = TRUE, # produces the footnote of excluded observations
#'                pop.var = "v_pop" ) 
#' 
#' # add rate suppression rule
#' summary_table( d = d.example.na,
#'                metric = c( "count", "rate" ),
#'                var1 = "v1",
#'                var2 = "v2",
#'                nm.var1 = "Variable 1",
#'                add.summary.row =TRUE,
#'                add.summary.col = TRUE,
#'                foot.lines = c("This is the first footer line",
#'                               "This is the second footer line" ), # manual footer lines
#'                remove.cols = "Char 3",
#'                percentages.rel = "var2",
#'                count.supp = 50, # suppress values/counts in table <= 50
#'                rate.supp = 50, # suppress rate calculation when counts are <= 50
#'                NAs.footnote = TRUE, # produces the footnote of excluded observations
#'                pop.var = "v_pop" ) 
#' 
#' # give it a title
#' summary_table( d = d.example.na,
#'                metric = c( "count", "rate" ),
#'                var1 = "v1",
#'                var2 = "v2",
#'                nm.var1 = "Variable 1",
#'                add.summary.row =TRUE,
#'                add.summary.col = TRUE,
#'                foot.lines = c("This is the first footer line",
#'                               "This is the second footer line" ), # manual footer lines
#'                remove.cols = "Char 3",
#'                percentages.rel = "var2",
#'                count.supp = 50, # suppress values/counts in table <= 50
#'                rate.supp = 50, # suppress rate calculation when counts are <= 50
#'                NAs.footnote = TRUE, # produces the foornote of excluded observations
#'                pop.var = "v_pop",
#'                table.title = "This is Table 1" ) # title
#' 
#' @export

summary_table <- function( d, var1, var2 = NULL, table.grouping = NULL, pop.var = NULL,
                           add.summary.row = TRUE, summary.row.name = NULL, add.summary.col = TRUE, digs.perc = 1, digs.rate = 2,
                           summary.col.name = NULL, order.rows = NULL, order.cols = NULL, order.groups = NULL, foot.lines = c(), 
                           table.title = NULL, metric = c( "count", "rate" ), nm.var1 = NULL, count.supp = NULL, remove.cols = NULL,
                           rate.supp = NULL, count.supp.symbol = "--", rate.supp.symbol = "*", 
                           per = 1000, NAs.footnote = FALSE, percentages.rel = "var1",
                           include.percent.sign = TRUE ){
  
  ## call checks ##
  
  var1 <- cement( !!var1 )
  
  if( is.null( var1 ) ) stop( "`var1` cannot be NULL; if only one variable is desired for stratification purposes, it should be included in the `var1` argument" ) 
  
  if( eRTG3D::is.sf.3d( d ) ) d <- data.frame( d ) # if `sf` object, keep only attributes table
  
  if( !is.null( order.rows ) & sum( order.rows %in% levels( as.factor( d[[ var1 ]] ) ) ) != length( levels( as.factor( d[[ var1 ]] ) ) ) ){
    warning( "`order.rows` does not match levels of `var1` in `d` exactly; this may be due to wanting to omit some rows from the outputted table" )
  }
  
  if( !is.null( summary.row.name ) ){
    if( !inherits( summary.row.name, "character" ) )
      stop( "`summary.row.name` must be a string." )
  }
  
  if( !is.null( order.groups )){
    if ( ( !is.null( var2 ) & is.null( table.grouping ) ) |
         ( is.null( var2 ) & !is.null( table.grouping ) ) ){
      warning( "`order.groups` specified but only one of `var2` or `table.grouping` was specified. `order.groups` will be ignored. You may need to specify `order.rows` for ordering `var1` or `order.cols` for ordering `var2`" )
    }
    
    if( !is.null( order.groups ) & sum( order.groups %in% levels( as.factor( d[[ table.grouping ]] ) ) ) != length( levels( as.factor( d[[ table.grouping ]] ) ) ) ){
      warning( "`order.groups` does not match levels of `table.grouping` in `d` exactly; this may be due to wanting to omit some grouping rows from the outputted table" )
    }
    
  }
  
  if( !add.summary.row & !is.null( summary.row.name ) ){
    warning( "A name was provided to `summary.row.name` but `add.summary.row` was indicated as NULL. Note that `summary.row.name` will be irrelevant in this case unless `add.summary.row` is changed to TRUE" )
  }
  
  if( is.null( pop.var ) & any( stringr::str_detect( metric, "rate" ) ) ){
    stop( "`pop.var` variable not detected in dataset. Ensure that population count data (aggregate-level) are stored in `pop.var` variable." )
  }
  
  if( !is.null( var1 ) & !is.null( var2 ) & !percentages.rel %in% c( "var1", "var2" ) ){
    stop( "`percentages.rel` should be one of 'var1 or 'var2'. " )
  }
  
  if( !is.null( var1 ) & is.null( var2 ) & percentages.rel != "var1" & is.null( table.grouping ) ){
    stop( "`percentages.rel` can only be 'var1' since only `var1` was specified. " )
  }
  
  if( !is.null( var1 ) & !is.null( var2 ) & percentages.rel == "table.grouping" & !is.null( table.grouping ) ){
    stop( "`percentages.rel` can only be 'var1' or 'var2' since `var1`, `var2.`, AND `table.grouping` were all specified. " )
  }
  
  if( !is.null( table.title) ){
    if( !inherits( table.title, "character" ) ) stop( '`table.title` must be of class "character" ' )
  }
  
  if( is.null( d[[ var1 ]] ) ){
    stop( "`var1` variable not detected in dataset." )
  }
  
  if( is.null( var1 ) & is.null( var2 ) ){
    stop( "At least one of `var1` or `var2` must be specified in the function call." )
  }
  
  if( length( table.grouping ) > 1 & !is.null( table.grouping ) ){
    stop( "There can only be one variable, maximum, specified in `table.grouping` for grouping the table." )
  }
  
  if( !is.null( table.grouping ) ){
    if( is.null( d[[ table.grouping ]] ) ) stop( "`table.grouping` variable not detected in dataset." )
  }
  
  if( length( metric ) > 2 ){
    stop( "`metric` can only include two metrics, maximum, to be tabulated" )
  }
  
  if( sum( metric %in% c( "percent", "rate", "count" ) ) != length( metric ) ){
    stop( '`metric` can only be one or two of the following exact strings: "percent", "rate", "count" ' )
  }
  
  if( !is.null( count.supp ) ){
    if( count.supp < 1 ) stop( "`count.supp` must be > 0 if specified." )
  }
  
  if( !is.null( rate.supp ) ){
    if( rate.supp < 1 ) stop( "`rate.supp` must be > 0 if specified." )
  }
  
  if( !is.null( rate.supp ) & is.null( rate.supp.symbol )){
    stop( "If `rate.supp` is specified, then `rate.supp.symbol` must be specified as well." )
  }
  
  if( !is.null( count.supp ) & is.null( count.supp.symbol )){
    stop( "If `count.supp` is specified, then `count.supp.symbol` must be specified as well." )
  }
  
  if( is.null( var2 ) & percentages.rel == "var2" ){
    stop( "`percentages.rel = 'var2'` is specified but `var2` is NULL." )
  }
  
  if( is.null( table.grouping ) & percentages.rel == "table.grouping" ){
    stop( "`percentages.rel = 'table.grouping'` is specified but `table.grouping` is NULL." )
  }
  
  
  ## pop variable reassignment ##
  pop.nm <- if( !is.null( pop.var ) ) pop.var else "none"
  
  # variable logic #
  
  # two stratifying variables
  var.logic.2 <- !is.null( var1 ) & !is.null( var2 ) # two variables indicated for tabulation
  var.logic.1 <- !is.null( var1 ) & is.null( var2 ) # only one variable indicated for tabulation
  tb.grp.logic <- !is.null( table.grouping ) # grouping variable included
  tb.grp.var2.logic <- is.null( var2 ) & !is.null( table.grouping )
  order.rows.logic <- !is.null( order.rows ) & sum( order.rows %in% levels( as.factor( d[[ var1 ]] ) ) ) != length( levels( as.factor( d[[ var1 ]] ) ) ) # if `order.rows` is specified with the same number of levels as original variable
  tb.grp.var2.wide.logic <- FALSE # this is updated below if the subsequent logic is true
  
  
  # if var2 and table.grouping are specified and there is only 1 level being displayed in var2, we will pivot entire table to wider to ensure space
  if( !is.null( var2 ) & !is.null( table.grouping ) ){
    
    cols.left <- levels( as.factor( d[[ var2 ]] ) ) %>%
      .[ !. %in% remove.cols ]
    
    tb.grp.var2.wide.logic <- !is.null( var2 ) & !is.null( table.grouping ) & length( cols.left ) == 1 & length( metric ) <= 2
  }
  
  
  if( tb.grp.var2.wide.logic ) warning( "Pivoting table to wide format given that there is only 1 level in `var2`" )
  
  
  # if var2 is not provided but there is a var1 and table.grouping specification, reassign var2 to table.grouping
  # and reassign the logic objects; this makes tables simpler
  if( tb.grp.var2.logic ){
    
    var2 <- var1
    
    var1 <- table.grouping
    
    table.grouping <- NULL
    
    var.logic.2 <- TRUE
    
    var.logic.1 <- FALSE
    
    tb.grp.logic <- FALSE
    
    percentages.rel <- if( percentages.rel == "table.grouping" ){
      "var1" } else if( percentages.rel == "var1" ){ 
        "var2" } else{ percentages.rel }
    
  }
  
  # patch
  if( !is.null( var1 ) & !is.null( var2 ) & is.null( table.grouping ) ){
    
    tb.no.grp.var2.logic <- TRUE
    
  }
  
  # check on column names included in `order.cols` if it is specified ( must match at least one cols in final table)
  if( !is.null( order.cols ) ){
    
    sum.col.null <- ifelse( add.summary.col & !is.null( summary.col.name ), summary.col.name,
                            ifelse( add.summary.col & is.null( summary.col.name ), "All",
                                    NULL ) )
    
    cols.final <- c( names( table( d[[ var2 ]]) ),
                     sum.col.null ) %>%  # column name prefixes in final table
      { if ( !is.null( remove.cols ) ) .[ !. %in% remove.cols ] else . } # remove column names from undesired columns in final table
    
    if( sum( order.cols %in% cols.final ) < 1 ) stop( "No column names from `order.cols` identified in the final table." )
    
  }
  
  # check to ensure column names and provide warning if they are different
  other.var <- if( is.null( var2 ) & !is.null( table.grouping ) ) table.grouping else if( !is.null( var2 ) & is.null( table.grouping ) ) var2 else var2
  other.var.warn.text <- if( is.null( var2 ) & !is.null( table.grouping ) ) "table.grouping" else if( !is.null( var2 ) & is.null( table.grouping ) ) "table.grouping" else "var2"
  
  if( !is.null( order.cols ) & sum( order.cols[ !order.cols %in% c( summary.row.name) ] %in% levels( as.factor( d[[ other.var ]] ) ) ) != length( levels( as.factor( d[[ other.var ]] ) )[ !levels( as.factor( d[[ other.var ]] ) ) %in% remove.cols ] ) ){
    warning( paste0( "`order.cols` does not match levels of `", other.var.warn.text, "` in `d` exactly. This may be due to wanting to omit some columns intentionally." ) )
  }
  
  # variable with which we need to calculate percentages relative to
  
  if( percentages.rel == "var1" ) var.ref <- var1 else if( percentages.rel == "var2" ) var.ref <- var2
  
  ## check on missing data ##
  ( d.miss <- d %>%
      { if( var.logic.2 ) filter( ., is.na( get( var1 ) ) | is.na( get( var2 ) ) ) else . } %>% # if two variables are provided
      { if( var.logic.1 ) filter( ., is.na( get( var1 ) ) ) else . } %>% # if only one variable is provided
      { if( tb.grp.logic ) group_by( ., !!sym( table.grouping ) ) else . } %>%  # conditional piping line; we use this structure often throughout the function 
      { if( tb.grp.var2.logic ) group_by( ., !!sym( var1 ) ) else . } %>%  # conditional piping line; we use this structure often throughout the function    
      reframe( NAs = n() ) %>%  # NOTE: `dplyr::reframe` is a more generalized version of `dplyr::summarise`
      data.frame() ) # we save the data frame to use for adding footnotes later on
  
  d.nomiss <- d %>%
    { if( var.logic.2 ) filter( ., !is.na( !!sym( var1 ) ) & !is.na( !!sym( var2 ) ) ) else . } %>% # if two variables are provided
    { if( var.logic.1 ) filter( ., !is.na( !!sym( var1 ) ) ) else . } %>% # if only one variable is provided
    mutate( across( .cols = any_of( c( table.grouping, var1, var2 ) ),
                    .fns = ~ as.character( . ) ) ) # coerce all categorical variables to character to avoid issues downstream with row binds
  
  
  ## counts ##
  
  if( !is.null( summary.row.name ) ) sum.row.nm <- summary.row.name else sum.row.nm <- "Summary Row"
  
  if( !is.null( summary.col.name ) ) sum.col.nm <- summary.col.name else sum.col.nm <- "All"
  
  d.1 <- { if( var.logic.2 ){ # two-variable case
    bind_rows( d.nomiss %>%
                 { if( tb.grp.logic ){ # first condition is if a table grouping is specified, then the results are grouped by the `table.grouping` variable
                   reframe( ., group = !!sym( table.grouping),  # NOTE: `dplyr::reframe` is a more generalized version of `dplyr::summarise`
                            no_count = n(),
                            pop = { if( "rate" %in% metric )  unique( !!sym( pop.nm ) ) },
                            pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                            .by = c( !!sym( table.grouping ), !!sym( var1 ) , !!sym( var2 ) ) ) # grouped summarise results, grouping variable included
                 } else {
                   reframe( ., no_count = n(),
                            pop = { if( "rate" %in% metric )  unique( !!sym( pop.nm ) ) },
                            pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                            .by = c( !!sym( var1 ), !!sym( var2 ) ) ) } } %>%  # grouped summarise results, grouping variable NOT included
                 { if( tb.grp.logic ) dplyr::select( ., -!!sym( table.grouping ) ) else . } %>%  # conditional piping line     
                 distinct(),
               { if( add.summary.col ){ # now add data not stratified. if indicated in `add.summary.col` argument
                 d.nomiss %>%
                   { if( tb.grp.logic ){ 
                     reframe( ., group = !!sym( table.grouping),
                              no_count = n(),
                              pop = { if( "rate" %in% metric )  unique( !!sym( pop.nm ) ) }, # need to do sum of population over all levels, otherwise we get unidentified rows
                              pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                              .by = c( !!sym( table.grouping ), !!sym( var1 ) ) )
                   } else {
                     reframe( ., no_count = n(),
                              pop = { if( "rate" %in% metric )  sum( !!sym( pop.nm ) ) },
                              pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                              .by = !!sym( var1 ) ) } } %>%  # conditional piping line     
                   { if( tb.grp.logic ) dplyr::select( ., -!!sym( table.grouping ) ) else . } %>%  # conditional piping line     
                   distinct() %>%
                   add_column( !!sym( var2 ) := sum.col.nm ) } } ) 
  } else if( var.logic.1 ){ # single variable case
    
    d.nomiss %>%
      { if( tb.grp.logic ){ # first condition is if a table grouping is specified, then the results are grouped by the `table.grouping` variable
        reframe( ., group = !!sym( table.grouping),  # NOTE: `dplyr::reframe` is a more generalized version of `dplyr::summarise`
                 no_count = n(),
                 pop = { if( "rate" %in% metric )  unique( !!sym( pop.nm ) ) },
                 pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                 .by = c( !!sym( table.grouping ), !!sym( var1 ) ) ) # grouped summarise results, grouping variable included
      } else {
        reframe( ., no_count = n(),
                 pop = { if( "rate" %in% metric )  unique( !!sym( pop.nm ) ) },
                 pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                 .by = c( !!sym( var1 ) ) ) } } %>%  # grouped summarise results, grouping variable NOT included
      { if( tb.grp.logic ) dplyr::select( ., -!!sym( table.grouping ) ) else . } %>%  # conditional piping line     
      distinct()  # `add.summary.col` is inconsequential when is.null( var.2 ) & !is.null( var.1 )
  } } %>%
    mutate( !!sym( var1 ) := as.character( !!sym( var1 ) ),
            !!sym( var2 ) := as.character( !!sym( var2 ) ) ) %>% # coerce the vars to character before binding, otherwise it throws error
    { if( add.summary.row & tb.grp.logic ){  # if we are to include a row summarizing var2 data for all of dc 
      
      if( !add.summary.col ){ d.dc.ref <- d.nomiss %>%
        reframe( ., group = !!sym( table.grouping ),  # NOTE: `dplyr::reframe` is a more generalized version of `dplyr::summarise`
                 no_count = n(),
                 pop = { if( "rate" %in% metric )  unique( !!sym( pop.nm ) ) }, # for rates that occur over all years of data, take the sum
                 pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                 .by = c( !!sym( table.grouping ), !!sym( var2 ) ) ) %>%
        distinct() %>%
        mutate( !!sym( var1 ) := sum.row.nm ) %>%
        dplyr::select( -!!sym( table.grouping ) )
      
      bind_rows( ., d.dc.ref ) 
      
      } else if( add.summary.col ){ # if both `add.summary.col` and `add.summary.row` are specified as TRUE
        
        d.dc.ref <- bind_rows(
          d.nomiss %>%
            reframe( ., group = !!sym( table.grouping),  # NOTE: `dplyr::reframe` is a more generalized version of `dplyr::summarise`
                     no_count = n(),
                     pop = { if( "rate" %in% metric )  unique( !!sym( pop.nm ) ) },
                     pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                     .by = c( !!sym( table.grouping ), !!sym( var2 ) ) ) %>%
            distinct() %>%
            mutate( !!sym( var1 ) := sum.row.nm ) %>%
            dplyr::select( -!!sym( table.grouping ) ),
          
          d.nomiss %>%
            reframe( ., group = !!sym( table.grouping),  # NOTE: `dplyr::reframe` is a more generalized version of `dplyr::summarise`
                     no_count = n(),
                     pop = { if( "rate" %in% metric )  unique( !!sym( pop.nm ) ) },
                     pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                     .by = c( !!sym( table.grouping ) ) ) %>%
            distinct() %>%
            mutate( !!sym( var1 ) := sum.row.nm,
                    !!sym( var2 ) := sum.col.nm ) %>%
            dplyr::select( -!!sym( table.grouping ) )
        )
        
        bind_rows( ., d.dc.ref )
        
      } } else . } %>%
    { if( add.summary.row & !tb.grp.logic ) {  # if we are to include a row summarizing var2 data for all of dc 
      
      if( !add.summary.col ){ d.dc.ref <- d.nomiss %>%
        reframe( ., 
                 no_count = n(),
                 pop = { if( "rate" %in% metric )  unique( !!sym( pop.nm ) ) },
                 pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                 .by = c( !!sym( var2 ) ) ) %>%
        distinct() %>%
        mutate( !!sym( var1 ) := sum.row.nm ) 
      
      bind_rows( ., d.dc.ref )
      
      } else if( add.summary.col ){
        
        d.dc.ref <- bind_rows( 
          
          d.nomiss %>%
            reframe( ., 
                     no_count = n(),
                     pop = { if( "rate" %in% metric )  unique( !!sym( pop.nm ) ) },
                     pop_rate = { if( "rate" %in% metric )  per * no_count / pop },
                     .by = c( !!sym( var2 ) ) ) %>%
            distinct() %>%
            mutate( !!sym( var1 ) := sum.row.nm ),
          
          d.nomiss %>%
            reframe( ., 
                     no_count = n(),
                     pop = { if( "rate" %in% metric )  sum( !!sym( pop.nm ) ) },
                     pop_rate = { if( "rate" %in% metric )  per * no_count / pop } ) %>%
            distinct() %>%
            mutate( !!sym( var1 ) := sum.row.nm,
                    !!sym( var2 ) := sum.col.nm ) )
        
        bind_rows( . ,
                   d.dc.ref ) 
        
      } } else . }
  
  # if `add.summary.col` or `add.summary.row` are TRUE then set pop_rate and pop to "" for All and Summary row entries
  ## If `add.summary.col` and/or `add.summary.row` are TRUE and "rate" is in `metric`, coerce them to FALSE and give warning
  
  redo.rate <- TRUE  # for later internal use
  
  if( any( c( add.summary.col, add.summary.row ) ) & any( stringr::str_detect( metric, "rate" ) ) ){
    
    d.1 <- d.1 %>%
      filter( if_any( .cols = any_of( c( var1, var2, table.grouping ) ),
                      .fns = ~ . %in% c( sum.col.nm, sum.row.nm ) ) ) %>%
      mutate( across( .cols = c( pop, pop_rate ) ,
                      .fns = ~ "" ) ) %>% 
      distinct() %>%
      bind_rows( d.1 %>%
                   filter( !if_any( .cols = any_of( c( var1, var2, table.grouping ) ),
                                    .fns = ~ . %in% c( sum.col.nm, sum.row.nm ) ) ) %>%
                   mutate( across( .cols = contains( "rate" ),
                                   .fns = ~ ifelse( . < 1.0,
                                                   formatC( signif( ., 
                                                                    digits = digs.rate ), 
                                                            digits = digs.rate, 
                                                            format = "fg", 
                                                            flag = "#" ),
                                                   sprintf( paste0( "%.", digs.rate, "f" ), . ) ) ),
                           across( .cols = c( pop, pop_rate ) ,
                                   .fns = ~ as.character( . ) ) ),
                 . )
    
    redo.rate <- FALSE  # for later internal use
    
    warning( 'Either `add.summary.col` or `add.summary.row` was called (TRUE) but "rate" was indicated in `metric`. However, rates in the summary row and column are not provided in the output table. A future version of this function may allow for computation of rates for the summary row and column.')
  }

  ## get total count by stratifying variables for calculating percentages ##
  
  bths.yr <- { if( var.logic.2 ){ # two-variable case
    
    bind_rows( d.nomiss %>%
                 { if( tb.grp.logic ){                             # grouped summarise results, grouping variable included
                   reframe( ., group = !!sym( table.grouping),
                            tot_count_yr = n(),
                            .by = c( !!sym( table.grouping ), !!sym( var.ref ) ) )
                 } else {
                   reframe( ., tot_count_yr = n(),              # grouped summarise results, grouping variable NOT included
                            .by = !!sym( var.ref ) ) } } %>%
                 { if( tb.grp.logic ) dplyr::select( ., -!!sym( table.grouping ) ) else . } %>%
                 distinct(), 
               { if( add.summary.col & tb.grp.logic ){ # now add data not stratified. if indicated in `add.summary.col` argument
                 d.nomiss %>%
                   { if( tb.grp.logic ){ 
                     reframe( ., group = !!sym( table.grouping ), # grouped summarise results, grouping variable included
                              tot_count_yr = n(),
                              .by = !!sym( table.grouping ) )
                   } else { reframe( ., tot_count_yr = n() ) } } %>%           # grouped summarise results, grouping variable NOT included
                   { if( tb.grp.logic ) dplyr::select( ., -!!sym( table.grouping ) ) else . } %>%
                   distinct() %>%
                   add_column( !!sym( var.ref ) := sum.col.nm) } } )
    
  } else if( var.logic.1 ){ # single variable case
    
    d.nomiss %>%
      { if( tb.grp.logic ){                             # grouped summarise results, grouping variable included
        reframe( ., group = !!sym( table.grouping),
                 tot_count_yr = n(),
                 .by = c( !!sym( table.grouping ) ) )
      } else {
        reframe( ., tot_count_yr = n()) } } %>%             # grouped summarise results, grouping variable NOT included
      { if( tb.grp.logic ) dplyr::select( ., -!!sym( table.grouping ) ) else . } %>%
      distinct() # `add.summary.col` is inconsequential when is.null( var.2 ) & !is.null( var.1 )
    
  } } %>%
    mutate( across( .cols = -tot_count_yr,
                    .fns = ~ as.character( . ) ) ) %>%
    { if( add.summary.row & tb.grp.logic ) {  # if we are to include a row summarizing var2 data for all of dc 
      
      d.dc.ref.2 <- d.nomiss %>%
        reframe( ., group = !!sym( table.grouping),
                 tot_count_yr = n(),
                 .by = c( !!sym( table.grouping ) ) ) %>%
        distinct() %>%
        mutate( !!sym( var1 ) := sum.row.nm ) %>%
        dplyr::select( -!!sym( table.grouping ) )
      
      bind_rows( ., d.dc.ref.2 ) } else . } %>%
    { if( ( add.summary.row & !tb.grp.logic ) |
          ( add.summary.col & !add.summary.row & !tb.grp.logic ) ) {  # if we are to include a row summarizing var2 data for all of dc 
      
      d.dc.ref.2 <- d.nomiss %>% # problematic section
        reframe( .,
                 tot_count_yr = n() )%>%
        distinct() %>%
        { if( tb.grp.var2.logic |
              ( add.summary.col & !add.summary.row & !tb.grp.logic ) ){ mutate(., !!sym( var.ref ) := sum.col.nm )
        } else if ( !tb.grp.var2.logic ){ mutate(., !!sym( var.ref ) := sum.col.nm ) 
        } else . }
      
      bind_rows( ., d.dc.ref.2 ) } else . }
  
  ### (Patch 3/18/2025 CMV) -- fixes issue with .x and .y variables appearing
  
  
  # compute percentages (percent of total count that each row in the table comprises)
  
  symb.1 <- if( include.percent.sign ) "%" else ""
  
  d.2 <- if( var.logic.2 ){ # two-variable case
    
    { if( tb.grp.logic ){ left_join( d.1, bths.yr, by = c( "group", var.ref ) ) 
    } else { left_join( d.1, bths.yr, by = c( var.ref ) ) } } %>%
      mutate( count_perc = paste0( sprintf( paste0( "%.", digs.perc, "f" ), ( 100 * no_count ) / tot_count_yr ), symb.1 ) )
    
  } else if( var.logic.1 ){ # single-variable case
    
    { if( tb.grp.logic ){ left_join( d.1, bths.yr, by = c( "group" ) ) 
    } else { cross_join( d.1, bths.yr ) } } %>% # cross join since this case does not have any variables in common between `d.1` and `bths.yr`
      mutate( count_perc = paste0( sprintf( paste0( "%.", digs.perc, "f" ), ( 100 * no_count ) / tot_count_yr ), symb.1 ) )
    
  }
  
  
  ### Patch work bug fix (3/10/2025 CMV) -- this corrects issue with column names
  rem.this <- names( which( sapply( d.2, function(x) sum(is.na( x ) ) == nrow( d.2 ) ) ) )
  

    d.2 <- d.2 %>%
      dplyr::select( -all_of( rem.this ) ) %>%
      rename_with( .cols = everything(),
                   .fn = ~str_remove_all( ., "\\.x$|\\.y$") )
  
  
  ### Patch work bug fix (3/18/2025 CMV) -- this corrects issues with NAs when `add.summary.col` is called
  totals.patch <- bths.yr %>%
    filter( !!sym( var.ref ) == sum.col.nm ) %>%
    pull( tot_count_yr )
  
  d.2 <- d.2 %>%
    mutate( tot_count_yr = ifelse( is.na( tot_count_yr ) & 
                                     !!sym( var.ref ) == sum.row.nm, totals.patch, tot_count_yr ),
            count_perc = ifelse( ( count_perc %in% c( "NA", paste0( "NA", symb.1 ) ) | is.na( count_perc ) ) & 
                                   !!sym( var.ref ) == sum.row.nm, paste0( sprintf( paste0( "%.", digs.perc, "f" ), ( 100 * no_count ) / tot_count_yr ), symb.1 ),
                                 count_perc ) )
  
  
  ##  counts ##
  if( "count" %in% metric ){
    d.3 <- if( var.logic.2 ){ # two-variable case
      
      d.1 %>% 
        dplyr::select( -any_of( c( "pop", "pop_rate"  ) ) ) %>%
        distinct() %>%
        pivot_wider( names_from = !!sym( var2 ), values_from = no_count ) %>%
        { if( tb.grp.logic ){ rename_with( ., .fn = ~ paste0( .x, ",count", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                                           .cols = -c( group, !!sym( var1 ) ) ) 
        } else { rename_with( ., .fn = ~ paste0( .x, ",count", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                              .cols = -c( !!sym( var1 ) ) ) } }
      
    } else if( var.logic.1 ){ # single-variable case
      
      d.1 %>% 
        dplyr::select( -any_of( c( "pop", "pop_rate"  ) ) ) %>%
        distinct() %>%
        rename( tabulation = no_count ) %>% # important for rate-suppression step below to work
        { if( tb.grp.logic ){ rename_with( ., .fn = ~ paste0( .x, ",count", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                                           .cols = -c( group, !!sym( var1 ) ) ) 
        } else { rename_with( ., .fn = ~ paste0( .x, ",count", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                              .cols = -c( !!sym( var1 ) ) ) } }
      
    }
  }
  
  ##  rates ##
  if( "rate" %in% metric ){
    d.4 <- if( var.logic.2 & "rate" %in% metric ){ # two-variable case
      
      d.1 %>% 
        dplyr::select( -any_of( c( "no_count", "pop" ) ) ) %>%
        distinct() %>%
        pivot_wider( names_from = !!sym( var2 ), values_from = pop_rate ) %>%
        { if( tb.grp.logic ){ rename_with( ., .fn = ~ paste0( .x, ",rate", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                                           .cols = -c( group, !!sym( var1 ) ) ) 
        } else { rename_with( ., .fn = ~ paste0( .x, ",rate", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                              .cols = -c( !!sym( var1 ) ) ) } }
      
    } else if( var.logic.1 ){ # single-variable case
      
      d.1 %>% 
        dplyr::select( -any_of( c( "no_count", "pop" ) ) ) %>%
        distinct() %>%
        { if( "rate" %in% metric ){ rename( tabulation = pop_rate ) } else . } %>% # important for rate-suppression step below to work
        { if( tb.grp.logic ){ rename_with( ., .fn = ~ paste0( .x, ",rate", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                                           .cols = -c( group, !!sym( var1 ) ) ) 
        } else { rename_with( ., .fn = ~ paste0( .x, ",rate", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                              .cols = -c( !!sym( var1 ) ) ) } }
    }
  }
  
  ##  percentages ##
  
  
  if( "percent" %in% metric ){
    
    d.5 <- if( var.logic.2 ){ # two-variable case
      
      d.2 %>% 
        dplyr::select( -any_of( c( "no_count", "pop", "tot_count_yr", "pop_rate"  ) ) ) %>%
        distinct() %>%
        pivot_wider( names_from = !!sym( var2 ), values_from = count_perc ) %>%
        { if( tb.grp.logic ){ rename_with( ., .fn = ~ paste0( .x, ",percent", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                                           .cols = -c( group, !!sym( var1 ) ) ) 
        } else { rename_with( ., .fn = ~ paste0( .x, ",percent", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                              .cols = -c( !!sym( var1 ) ) ) } }  
      
    } else if( var.logic.1 ){ # single-variable case
      
      d.2 %>% 
        dplyr::select( -any_of( c( "no_count", "pop", "tot_count_yr", "pop_rate"  ) ) ) %>%
        distinct() %>%
        rename( tabulation = count_perc ) %>% # important for rate-suppression step below to work
        { if( tb.grp.logic ){ rename_with( ., .fn = ~ paste0( .x, ",percent", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                                           .cols = -c( group, !!sym( var1 ) ) ) 
        } else { rename_with( ., .fn = ~ paste0( .x, ",percent", recycle0 = TRUE ), # recycle0 = TRUE at recommendation of `dplyr` authors
                              .cols = -c( !!sym( var1 ) ) ) } }  
      
    }
  }
  
  # change NAs to 0's for any particular category since presence of NAs indicates no count in that particular race/eth cat
  if( "count" %in% metric ) d.3[ is.na( d.3 ) ] <- 0
  if( "rate" %in% metric ) d.4[ is.na( d.4 ) ] <- 0
  if( "percent" %in% metric ) d.5[ is.na( d.5 ) ] <- "0.0%"
  
  # check for if there are different years of data that are being used to calculate rates
  if( "rate" %in% metric ){
    if( nrow( d.3 ) < nrow( d.4 ) & "count" %in% metric) stop( "Potentially multiple years of population data detected in the dataset making computation of rates unreliable. Ensure that only the year you desire a rate for is included in the dataset; otherwise do not dplyr::select 'rate' in `metric` or use the `table.grouping` argument")
  }
  
  # decimal places for rates base on significant figures for values < 1.0 and decimal places for >= 1.0
  these.d <- as.vector( sapply( metric, function(x){
    
    switch( x,
            count = "d.3",
            percent = "d.5",
            rate =  "d.4" )
  }) )
  
  d.6 <- { if( length( metric  ) == 3 ){ if( tb.grp.logic ){ left_join( d.3, d.4, by = c( "group", var1 ) ) %>%
      left_join( ., d.5, by = c( "group", var1 ) ) 
  } else{ left_join( d.3, d.4, by = c( var1 ) ) %>%
      left_join( ., d.5, by = c( var1 ) ) } 
  } else if( length( metric  ) == 2 ){ if( tb.grp.logic ){ 
    
    left_join( get( these.d[1] ), get( these.d[2] ), by = c( "group", var1 ) ) 
    
  } else{ left_join( get( these.d[1] ), get( these.d[2] ), by = c( var1 ) ) } 
  } else if( length( metric == 1 ) ){
    get( these.d[1] )
    
  } } %>%
    .[, sort( names(.) ) ] %>% # arrange columns so count, perc, or rate of each level of the stratifying variable are together
    { if( tb.grp.logic ){ relocate( ., group, .before = 1 ) } else . } %>%
    relocate( !!sym( var1 ), .after = 1 ) %>%
    # decimal places for rates base on significant figures for values < 1.0 and decimal places for >= 1.0
    { if( redo.rate ){
      mutate( ., across( .cols = contains( "rate" ),
                         .fns = ~ ifelse( . < 1.0,
                                         formatC( signif( ., 
                                                          digits = digs.rate ), 
                                                  digits = digs.rate, 
                                                  format = "fg", 
                                                  flag = "#" ),
                                         sprintf( paste0( "%.", digs.rate, "f" ), . ) ) ) ) } else . }
  
  
  ## arrange/order rows if indicated with `order.rows` argument ##
  
  arrange.rows <- if( !is.null( order.rows ) ) order.rows else levels( as.factor( d[[ var1 ]] ) )
  
  
  if( add.summary.row & 
      !sum.row.nm %in% arrange.rows ) arrange.rows <- c( sum.row.nm, arrange.rows ) # by default put summary row as first row if it is requested
  
  
  ## cell rate suppression if desired ##
  
  if( !is.null( rate.supp ) & any( stringr::str_detect( names( d.6 ), "rate" ) )  & "rate" %in% metric ){
    
    cats <- names( d.6 )[ str_which( names( d.6 ), "rate" ) ] %>%
      str_remove( ., ",rate")
    
    # loop and replace rates of numerators (count) < `rate.supp` with `rate.supp.symbol`
    for( i in seq_along( cats ) ){
      
      for( j in 1:nrow( d.6 ) ){
        
        d.6[ j, paste0( cats[i], ",rate2" ) ] <- ifelse( d.6[ j, paste0( cats[i], ",count" ) ] <= rate.supp,
                                                         rate.supp.symbol, paste0( d.6[ j, paste0( cats[i], ",rate" ) ] ) )
      }
    }
    
    # drop original  rate column for new modified column with suppressed values
    d.6 <- d.6 %>%
      dplyr::select( -matches( "rate$", # regex for column name ending in "rate"
                        perl = TRUE ) ) %>% # `perl` argument is for indicating a regex expression
      rename_with( .fn = ~ str_replace( ., "rate2", "rate" ), # recycle0 = TRUE at recommendation of `dplyr` authors
                   .cols = contains( "rate2" ) )
    
  }
  
  # which metrics to not include in final table
  drop.this <- c( "percent", "rate", "count" )[ !c( "percent", "rate", "count" ) %in% metric ]
  
  d.7 <- d.6 %>% 
    dplyr::select( -contains( drop.this ) ) %>%  # drop metrics not to be displayed in table
    .[, sort( names(.) ) ] %>% # arrange columns so count and rate of same age group are side by side
    { if( tb.grp.logic ){
      mutate( ., group = as.character( group ) ) %>%
        relocate( group, .before = 1 ) %>%
        relocate( !!sym( var1 ), .after = 1 ) %>%
        group_by( group ) 
    } else{ relocate( ., !!sym( var1 ), .before = 1 ) } } %>%
    filter( !!sym( var1 ) %in% arrange.rows ) %>% # this filters out any levels of var1 indicated in `order.rows` that are not desired in the table
    arrange( match( !!sym( var1 ), arrange.rows ), # arrange rows in custom order;
             .by_group = TRUE ) %>%
    ungroup() %>%
    distinct()
  
  ## cell count suppression if desired ##
  if( !is.null( count.supp ) & any( stringr::str_detect( names( d.7 ), "count" ) ) ){
    
    d.7 <- d.7 %>%
      mutate( ., across( .cols = c( contains( "count" ) ),
                         .fns = ~ ifelse( . <= count.supp & . > 0, count.supp.symbol, . ) ) )
    
    # get column name prefixes 
    these.cols.pref <- names( d.7 ) %>%
      .[ !. %in% c( var1, "group" ) ] %>%
      str_remove_all( ., "percent|rate|count" ) %>%
      unique()
    
    # loop to also suppress rates and percents if the counts are suppressed
    for( i in 1:nrow( d.7 ) ){
      
      for( j in seq_along( these.cols.pref ) ){
        
        ij.supp <- d.7[ i, paste0( these.cols.pref[j], "count" ) ] == count.supp.symbol 
        
        if( ij.supp){ 
          
          if( "percent" %in% metric ){
            d.7[ i, paste0( these.cols.pref[j], "percent" ) ] <- count.supp.symbol
          }
          
          # if( "rate" %in% metric ){
          #   d.7[ i, paste0( these.cols.pref[j], "rate" ) ] <- count.supp.symbol
          # }
        }
        
      }
    }
    
    
  }
  
  ## pivoting to wider if there is only 1 level of `var2 ##`
  if ( tb.grp.var2.wide.logic ) {
    
    if( all( c( "percent", "count" ) %in% metric ) ){
      
      d.7 <- d.7 %>%
        mutate( !!sym( cols.left ) := paste0( !!sym( paste0( cols.left, ",count" ) ), ";",
                                              !!sym( paste0( cols.left, ",percent" ) ) ) ) %>%
        dplyr::select( -contains( "percent" ), -contains( "count" ) ) %>%
        pivot_wider( values_from = !!sym( cols.left ),
                     names_from = group ) %>% 
        separate_wider_delim( cols = c( -!!sym( var1 ) ) ,
                              delim = ";", 
                              names_sep = "," ) %>%
        rename_with( .cols = c( -!!sym( var1 ) ),
                     .fn = ~  str_replace_all(., "(\\,)1", "\\1count" ) ) %>%
        rename_with( .cols = c( -!!sym( var1 ) ),
                     .fn = ~  str_replace_all( ., "(\\,)2", "\\1percent" ) )
      
      
      
    }
    
    if( all( c( "rate", "count" ) %in% metric ) ){
      
      d.7 <- d.7 %>%
        mutate( !!sym( cols.left ) := paste0( !!sym( paste0( cols.left, ",count" ) ), ";",
                                              !!sym( paste0( cols.left, ",rate" ) ) ) ) %>%
        dplyr::select( -contains( "rate" ), -contains( "count" ) ) %>%
        pivot_wider( values_from = !!sym( cols.left ),
                     names_from = group ) %>% 
        separate_wider_delim( cols = c( -!!sym( var1 ) ) ,
                              delim = ";", 
                              names_sep = "," ) %>%
        rename_with( .cols = c( -!!sym( var1 ) ),
                     .fn = ~  str_replace_all(., "(\\,)1", "\\1count" ) ) %>%
        rename_with( .cols = c( -!!sym( var1 ) ),
                     .fn = ~  str_replace_all( ., "(\\,)2", "\\1rate" ) )
      
      
      
    }
    
    if( all( c( "rate", "percent" ) %in% metric ) ){
      
      d.7 <- d.7 %>%
        mutate( !!sym( cols.left ) := paste0( !!sym( paste0( cols.left, ",percent" ) ), ";",
                                              !!sym( paste0( cols.left, ",rate" ) ) ) ) %>%
        dplyr::select( -contains( "rate" ), -contains( "percent" ) ) %>%
        pivot_wider( values_from = !!sym( cols.left ),
                     names_from = group ) %>% 
        separate_wider_delim( cols = c( -!!sym( var1 ) ) ,
                              delim = ";", 
                              names_sep = "," ) %>%
        rename_with( .cols = c( -!!sym( var1 ) ),
                     .fn = ~  str_replace_all(., "(\\,)1", "\\1percent" ) ) %>%
        rename_with( .cols = c( -!!sym( var1 ) ),
                     .fn = ~  str_replace_all( ., "(\\,)2", "\\1rate" ) )
      
      
      
    }
    
    if( length( metric ) == 1 ){
      
      d.7 <- d.7 %>%
        mutate( !!sym( cols.left ) := paste0( !!sym( paste0( cols.left, ",", metric ) ) ) ) %>%
        dplyr::select( -contains( metric ) ) %>%
        pivot_wider( values_from = !!sym( cols.left ),
                     names_from = group ) %>% 
        separate_wider_delim( cols = c( -!!sym( var1 ) ) ,
                              delim = ";", 
                              names_sep = "," ) %>%
        rename_with( .cols = c( -!!sym( var1 ) ),
                     .fn = ~  str_replace_all( ., "(\\,)1", paste0( "\\1", metric ) ) )
      
      
    }
    
  }
  
  ## if summary column for all is included, move it to front ##
  if( add.summary.col & var.logic.2 ){
    
    d.7 <- d.7 %>%
      relocate( contains( "all" ), .after = !!sym( var1 ) )
    
  }
  
  
  ## remove columns from final table if desired ##
  if( !is.null( remove.cols ) ){
    
    d.7 <- d.7 %>%
      dplyr::select( -contains( remove.cols ) ) 
  }
  
  ## order columns in specific order if specified ##
  if( !is.null( order.cols ) ){
    
    # get exact order of columns as a vector of strings
    c.out <- vector()
    for( i in seq_along( order.cols ) ){
      
      c.out <- c( c.out, paste0(order.cols[i], ",", metric ) )
      
    }
    
    if( !tb.grp.logic){
      # reorder by passing to `dplyr::select`
      d.7 <- d.7 %>%
        dplyr::select( !!sym( var1 ), all_of( c.out ) )
    }
    
    if( tb.grp.logic){
      # reorder by passing to `dplyr::select`
      d.7 <- d.7 %>%
        dplyr::select( group, !!sym( var1 ), all_of( c.out ) )
    }
    
  }
  
  ## order groups in specific order if specified ##
  if( !is.null( order.groups ) ){
    
    d.7 <- d.7 %>%
      { if( !is.null( arrange.rows ) ){
        group_by( ., group ) %>%
          arrange( match( !!sym( var1 ), arrange.rows ), # arrange rows in custom order;
                   .by_group = TRUE ) %>%
          ungroup() } else . } %>%
      filter( group %in% order.groups ) %>%
      arrange( match( group, order.groups ) ) # arrange rows in custom order
    
  }
  
  
  ## generate the final table with `flextable` ##
  
  # footnote for table
  if( !is.null( foot.lines ) ){
    
    fn <- as_paragraph( foot.lines )
    
  }
  
  # no.grouping variables (used for formula in `colwidths` within `add_header_row` below)
  ng <- if( tb.grp.logic & !tb.grp.var2.wide.logic ) 1 else 0
  
  
  ## spanning headers and column names if `length( metric )` > 1 ##
  
  if( length( metric > 1 ) ){ # spanning headers will only be required if more than one metric is being tabulated for the levels of `var2`
    
    # column names relabeling
    new.nms <- colnames( d.7 ) %>%
      ifelse( stringr::str_detect( ., "count" ), "No.", . ) %>%
      ifelse( stringr::str_detect( ., "percent" ), "%", . ) %>%
      ifelse( stringr::str_detect( ., "rate" ), "Rate", . )
    
    new.nms <- stats::setNames( new.nms, colnames( d.7 ) ) %>% # this object gets fed to `set_header_labels`
      .[ . != "group" ] # remove group from names list because it is a grouping variable and will not appear as a column in final table
    # otherwise it messes up the counting/indices of the column names
    
    # race/eth categories vector
    h.cats <- names( d.7 ) %>%
      str_remove( ., ",count") %>%
      str_remove( ., ",rate" ) %>%
      str_remove( ., ",percent" ) %>%
      .[ !. %in% c( "group", var1 ) ] %>%
      unique(.)
    
    sp.h <- c( "", h.cats  ) # actual spanning headers that account for first empty column
    
    # column lengths for spanning headers
    col.w.cats <- rep( length( metric ), length( h.cats ) ) 
    col.w <- c( ( ncol( d.7 ) - sum( col.w.cats ) - ng ), col.w.cats ) # final lengths which include first column which is not a rate/percent/count column
    
  }
  
  # names if `length( metric )` == 1
  if( length( metric  ) == 1 ){
    
    new.nms <- names( d.7 ) %>%
      str_remove( ., ",count") %>%
      str_remove( ., ",rate" ) %>%
      str_remove( ., ",percent" )
    
    new.nms <- stats::setNames( new.nms, colnames( d.7 ) ) %>% # this object gets fed to `set_header_labels`
      .[ . != "group" ] # remove group from names list because it is a grouping variable and will not appear as a column in final table
    # otherwise it messes up the counting/indices of the column names
    
  }
  
  # columns to add the vertical border lines to (to the right of)
  pos.vec <- vector()
  for( i in 1:( length( col.w ) - 1 ) ){
    
    if( i == 1 ){
      
      pos.vec[i] <- 1
      
    } else{
      
      pos.vec[i] <- pos.vec[i-1] + length( metric )
      
    }
    
    
  }
  
  
  # change column name of `var1` if `nm.var1` is specified
  if( !is.null( nm.var1 ) ) {
    
    new.nms[ var1 ] <- nm.var1 
    
  }
  
  # remove columns from final table if desired
  if( !is.null( remove.cols ) ){
    
    d.7 <- d.7 %>%
      dplyr::select( -contains( remove.cols ) ) 
  }
  
  
  # generate the final table with `flextable`
  if( !tb.grp.var2.wide.logic ){
    
    t.out <- { if( tb.grp.logic ){ as_grouped_data( d.7, groups = "group" ) %>%
        flextable::as_flextable( hide_grouplabel = TRUE )  %>% # first two lines of this command are used to create the grouping headers within the table
        surround( i = ~ !is.na( group ), # this `surround` command is to add the top horizontal and bottom horizonal lines to the grouping header title
                  border.top = fp_border_default( width = 0.1 ), 
                  border.bottom = fp_border_default( width = 0.1 ),
                  part = "body") 
    } else{ flextable::flextable( d.7 ) } } %>%
      { if( length( metric ) > 1 & var.logic.2 ){ # spanning headers for `var2` only if more than one metric is requested and if there is cross-tabulation
        add_header_row( ., colwidths = col.w,
                        values = sp.h ) 
      } else{ . } } %>% 
      theme_zebra() %>%
      theme_vanilla() %>%
      set_header_labels( values = new.nms ) %>% # change names based on new.nms vector created outside this flextable command
      align( part = "header", i = 1, align = "center" ) %>% # center align spanning headers
      { if( !is.null( table.title ) ) add_header_lines( ., values = table.title ) %>% # title line
          align( part = "header", i = 1, align = "left" ) else . } %>% #left-align title
      vline( j = pos.vec, 
             part = "body",
             border = fp_border_default( width = 0.1 ) ) %>% # add vertical line borders after each spanning header group
      border_inner_h( border = fp_border_default( width = 0.1 ), part = "header" ) %>% # make header border lines thinner
      hline_top( border = fp_border_default( width = 0.1 ), part = "body" ) %>% # make body top border line thinner
      flextable::font( fontname = "Arial", part = "all" ) %>% # change font for entire table and ensure arial
      fontsize( size = 8, part = "all" ) %>%
      fontsize( size = 7, part = "footer" ) %>%
      width( width = 0.45 ) %>% # change column widths to fit on one page
      line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
      line_spacing( space = 0.7, part = "body" ) %>% # make spacing in footnote single and not double
      padding( padding.top = 2.5, 
               padding.bottom = 2.5, 
               padding.left = 4, 
               padding.right = 4, 
               part = "body" ) %>%
      padding( padding.top = 5, 
               padding.bottom = 5, 
               padding.left = 0, 
               padding.right = 0, 
               part = "header" ) %>%
      padding( padding.top = 2.5, 
               padding.bottom = 0,
               part = "footer" ) %>%
      padding( padding.top = 2.5, padding.bottom = 2.5,part = "footer", i = 1 ) %>%
      width( width = 0.95, j = var1 ) %>% # change column widths to fit on one page
      flextable::height( height = 0.002, part = "body", unit = "cm" ) %>%
      bg( bg = "white", part = "footer" ) %>% # color footer white (undoes `theme_zebra` styling)
      border_inner_h( border = fp_border(color = "white", style = "solid", width = 0),
                      part = "footer")%>% # remove footer horizontal line (undoes `theme_zebra` styling)
      bold( bold = FALSE, part = "footer" ) # unbold footer text (undoes `theme_zebra` bolding)
  }
  
  # for wider table when there is only 1 level of var2 but var2 and table.grouping are specified
  if( tb.grp.var2.wide.logic ){
    
    t.out <- flextable::flextable( d.7) %>%
      { if( length( metric ) > 1 & var.logic.2 ){ # spanning headers for `var2` only if more than one metric is requested and if there is cross-tabulation
        add_header_row( ., colwidths = col.w,
                        values = sp.h ) 
      } else{ . } } %>% 
      theme_zebra() %>%
      theme_vanilla() %>%
      set_header_labels( values = new.nms ) %>% # change names based on new.nms vector created outside this flextable command
      align( part = "header", i = 1, align = "center" ) %>% # center align spanning headers
      { if( !is.null( table.title ) ) add_header_lines( ., values = table.title ) %>% # title line
          align( part = "header", i = 1, align = "left" ) else . } %>% #left-align title
      vline( j = pos.vec, 
             part = "body",
             border = fp_border_default( width = 0.1 ) ) %>% # add vertical line borders after each spanning header group
      border_inner_h( border = fp_border_default( width = 0.1 ), part = "header" ) %>% # make header border lines thinner
      hline_top( border = fp_border_default( width = 0.1 ), part = "body" ) %>% # make body top border line thinner
      flextable::font( fontname = "Arial", part = "all" ) %>% # change font for entire table and ensure arial
      fontsize( size = 8, part = "all" ) %>%
      fontsize( size = 7, part = "footer" ) %>%
      width( width = 0.45 ) %>% # change column widths to fit on one page
      line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
      line_spacing( space = 0.7, part = "body" ) %>% # make spacing in footnote single and not double
      padding( padding.top = 2.5, 
               padding.bottom = 2.5, 
               padding.left = 4, 
               padding.right = 4, 
               part = "body" ) %>%
      padding( padding.top = 5, 
               padding.bottom = 5, 
               padding.left = 0, 
               padding.right = 0, 
               part = "header" ) %>%
      padding( padding.top = 2.5, 
               padding.bottom = 0,
               part = "footer" ) %>%
      padding( padding.top = 2.5, padding.bottom = 2.5,part = "footer", i = 1 ) %>%
      width( width = 0.95, j = var1 ) %>% # change column widths to fit on one page
      flextable::height( height = 0.002, part = "body", unit = "cm" ) %>%
      bg( bg = "white", part = "footer" ) %>% # color footer white (undoes `theme_zebra` styling)
      border_inner_h( border = officer::fp_border(color = "white", style = "solid", width = 0),
                      part = "footer")%>% # remove footer horizontal line (undoes `theme_zebra` styling)
      bold( bold = FALSE, part = "footer" ) # unbold footer text (undoes `theme_zebra` bolding)
    
  }
  
  ## footnotes ##
  
  # add foot notes for missing values iteratively
  
  
  if( NAs.footnote & nrow( d.miss > 0 ) ){
    
    for( p in 1:nrow( d.miss ) ){
      
      if( d.miss[ p, "NAs"] > 0 ){
        
        d.8 <- t.out$body$dataset
        
        # if there is a grouping variable and only `var1` specified, footnote is added to each grouping variable row within the body
        if( tb.grp.var2.logic ){ 
          
          lvs <- levels( as.factor( d.7[[ var1 ]] ) )
          row.id <- which( d.7[[ var1 ]] == d.miss[ p, var1 ] )
          
          t.out <- t.out %>%
            footnote( value = as_paragraph( paste0( "n = ", d.miss[ p, "NAs"], " records excluded due to missing data." ) ),
                      i = row.id, 
                      j = var1,
                      ref_symbols = as.integer( p ) ) %>%
            fontsize( size = 7, part = "footer" ) %>%
            line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
            padding( padding.top = 2.5, padding.bottom = 0, part = "footer", i = 1 )
          
          
        }
        
        # if there is a grouping variable, footnote is added to the grouping variable row
        else if( tb.grp.logic ){ 
          
          lvs <- levels( as.factor( d.7[[ var1 ]] ) )
          row.id <- which( d.8[[ "group" ]] == d.miss[ p, table.grouping ] )
          
          t.out <- t.out %>%
            footnote( value = as_paragraph( paste0( "n = ", d.miss[ p, "NAs"], " records excluded due to missing data." ) ),
                      i = row.id, 
                      j = var1,
                      ref_symbols = as.integer( p ) ) %>%
            fontsize( size = 7, part = "footer" ) %>%
            line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
            padding( padding.top = 2.5, padding.bottom = 0, part = "footer", i = 1 )
          
          
        }
        
        # if there is not a grouping variable, add to the column name of first column
        else if( !tb.grp.logic ){ 
          
          t.out <- t.out %>%
            footnote( value = as_paragraph( paste0( "n = ", d.miss[ p, "NAs"], " records excluded due to missing data." ) ),
                      i = nrow_part( ., part = "header" ) , # add to last row which is the header row
                      j = var1,
                      ref_symbols = as.integer( p ),
                      part = "header" ) %>%
            fontsize( size = 7, part = "footer" ) %>%
            line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
            padding( padding.top = 2.5, padding.bottom = 0, part = "footer", i = 1 )
          
        }
        
      }
      
    }
    
  }
  
  # footnote, if desired
  if( !is.null( foot.lines ) ){
    
    t.out <- t.out %>%
      add_footer_lines( values = fn ) %>% # add footer
      fontsize( size = 7, part = "footer" ) %>%
      line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
      padding( padding.top = 2.5, padding.bottom = 0, part = "footer", i = 1 )
    
  }
  
  # footnotes for suppressed cell values and rates
  include.ft.nt.cell.supp <- length( d.7[ d.7 == count.supp.symbol ] ) > 0 # logical for if there are any suppressed cells in the table, otherwise footnote not needed
  
  if( !is.null( count.supp ) & "count" %in% metric & include.ft.nt.cell.supp ){
    
    if( count.supp > 0 ){
      
      t.out <- t.out %>%
        add_footer_lines( values = paste0( count.supp.symbol, " Cell counts \U2264", count.supp," were suppressed to protect confidentiality." ) ) %>% # note the use of the UNICODE in this string
        fontsize( size = 7, part = "footer" ) %>%
        line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
        padding( padding.top = 2.5, padding.bottom = 0, part = "footer", i = 1 )
      
    } 
    
  }
  
  include.ft.nt.rate.supp <- length( d.7[ d.7 == rate.supp.symbol ] ) > 0 # logical for if there are any suppressed cells in the table, otherwise footnote not needed
  
  if( !is.null( rate.supp ) & "rate" %in% metric & include.ft.nt.rate.supp ){
    
    if( rate.supp > 0 ){
      
      t.out <- t.out %>%
        add_footer_lines( values = paste0( rate.supp.symbol, " Rates based on < ", rate.supp, " events are not presented since such rates are subject to instability." ) ) %>%
        fontsize( size = 7, part = "footer" ) %>%
        line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
        padding( padding.top = 2.5, padding.bottom = 0, part = "footer", i = 1 )
    }
  }
  
  return( list( flextable = t.out,
                frame = d.7 ) )
  
}



