###---------------------------------------------------------------
###  `summary_table`: Generate "Nice" `flextable` From Input Data
###---------------------------------------------------------------

#' @title Generate "Nice" `flextable` From Input Data
#'
#' @description Create a "nice" publication-level table by cross-tabulating up to three variables at once.
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
#' @usage summary_table( d, var1, var2 = NULL, table.grouping = NULL, pop.var = NULL,
#' add.summary.row = TRUE, summary.row.name = NULL, add.summary.col = TRUE, font.family = "Calibri Light",
#' digs.perc = 1, digs.rate = 2, summary.col.name = NULL, summary.col.pos = "front", order.rows = NULL, order.cols = NULL, 
#' order.groups = NULL, foot.lines = c(), table.title = NULL, metric = c( "count", "rate" ), nm.var1 = NULL, 
#' count.supp = NULL, remove.cols = NULL, rate.supp = NULL, count.supp.symbol = "--", rate.supp.symbol = "*", 
#' per = 1000, NAs.footnote = FALSE, percentages.rel = "var1", include.percent.sign = TRUE, 
#' row.variable.labels = "default", col.variable.labels = "default", row.variable.col = "#eed8a4" )
#'
#' @param d A data frame or tibble. Data must be the exact subset of data that needs to analyzed for the table generation.
#' @param var1 A string. Name of first variable to stratify on (as it appears in the input data, `d`). Cannot be  `NULL`.
#' @param var2 A string. Name of second variable to stratify on (as it appears in the input data, `d`). Can be `NULL` if only one variable is required and there is no cross-tabulation or if `table.grouping` and `var1` are specified for a cross-tabulation instead.
#' @param metric A vector of string(s). One or two of: "percent", "rate", "count". Can dplyr::select up to two options to plot at maximum.
#' @param table.grouping A string or `NULL` (if no grouping in table is desired). Name of variable to create grouped output table with (creates grouping rows in the table). Can be 1 variable at maximum.
#' @param pop.var A string or `NULL`. Variable in `d` with population count data for computing rates if "rate" is selected in `metric`. This is normally an aggregate variable.
#' @param add.summary.col A logical. Add a row with unaggregated results (based on `var2`). Note that this argument is inconsequential if `var2 == NULL`.
#' @param add.summary.row A logical. Add a row with unaggregated results (based on `var1`). `TRUE` is default and the name given to the row is "Summary Row". If another name is desired, it can be specified with `summary.row.name` and its order/position can be specified with the `order.rows` argument.
#' @param summary.col.name A string. Name to appear for summary column header. Default is "All". This option is only relevant if `add.summary.col == TRUE`.
#' @param summary.col.pos One of "front", "end", or an integer >= 0 and < the number of all levels of the categories in `var2` + 1.  Only a relevant argument if `length( var2 ) > 1` & `add.summary.col` is `TRUE`. 
#' @param summary.row.name A string. Name to appear for summary row label. Default is "Summary Row". This option is only relevant if `add.summary.row == TRUE`.
#' @param font.family A string with the font desired, to be applied to all sections of the table.
#' @param digs.perc An integer. Digits to round percents to.
#' @param digs.rate An integer. Digits to round rates to.
#' @param order.rows A vector with the levels in custom order of the stratifying variable in the rows(typically `var1` but can also be `table.grouping` depending on how the function is specified). Entries must match levels of stratifying variable exactly. If there are any levels of the row variable missing, they are omitted from the final table. Note that any rows removed from the final table do not remove that level from the data itself. If desired, that must be done outside the function with a data step. If multiple variables are listed in `var1`, then a named list with the variables specified in `var1` as the entry names and a character string with the desired order of each of those variable levels should be specified.
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
#' @param row.variable.labels "default", "none" or a named list with the variables specified in `var1` as the entry names and a character string with the desired label for that variable in the rows.  If "default", the default variable names (i.e., those listed in `var1` are printed). If "none", no labels are printed. Default is "default".
#' @param col.variable.labels "default", "none" or a named list with the variables specified in `var2` as the entry names and a character string with the desired label for that variable in the columns  If "default", the default variable names (i.e., those listed in `var2` are printed). If "none", no labels are printed. Default is "default".
#' @param row.variable.col A string Hex code or color code for the background in the label row or "none". This argument is only relevant if `length( var1 ) > 1` (i.e., multiple variables are desired in the rows of the table). Default is "#eed8a4". If "none" the default `flextable::theme_zebra` theme is used for that row.
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
#'                order.cols = c( "Geo 1", 
#'                "Geo 2", "All Geos", "Geo 3" ), # reorder columns in output table
#'                order.rows = c( "Char 1", 
#'                "Char 2", "All Char", "Char 3" ), # reorder columns in output table
#'                percentages.rel = "table.grouping" )
#' 
#' # same call as previous, but use order.cols and order.rows to remove some 
#' # rows and columns from final table
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                table.grouping = "v2",
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                summary.col.name = "All Geos", # change name of summary column
#'                summary.row.name = "All Char", # change name of summary row
#'                order.cols = c( "Geo 1", 
#'                "All Geos", "Geo 3" ), # reorder columns in output table
#'                order.rows = c( "Char 1", 
#'                "Char 2", "All Char" ), # reorder columns in output table
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
#'                order.cols = c( "Geo 1", 
#'                "All Geos", "Geo 3" ), # reorder columns in output table
#'                order.rows = c( "Char 1",
#'                "Char 2", "All Char" ), # reorder columns in output table
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
#' 
#' summary_table( d = d.example.na,
#'                metric = c( "count", "percent" ),
#'                var1 = "v1",
#'                var2 = "v2",
#'                nm.var1 = "Variable 1",
#'                add.summary.row =TRUE,
#'                add.summary.col = TRUE,
#'                foot.lines = c("This is the first footer line",
#'                               "This is the second footer line" ), 
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
#'                               "This is the second footer line" ),
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
#'                               "This is the second footer line" ), 
#'                remove.cols = "Char 3",
#'                percentages.rel = "var2",
#'                count.supp = 50, # suppress values/counts in table <= 50
#'                rate.supp = 50, # suppress rate calculation when counts are <= 50
#'                NAs.footnote = TRUE, # produces the foornote of excluded observations
#'                pop.var = "v_pop",
#'                table.title = "This is Table 1" ) # title
#'                
#' # when providing more than 1 variable for the rows
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = c( "v1", "v3" ),
#'                order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
#'                                   v3 = c( "Other Char 2", "Other Char 3" )),
#'                var2 = "v2",
#'                add.summary.row = FALSE,
#'                add.summary.col = FALSE,
#'                rate.supp = 5,
#'                count.supp = 5,
#'                percentages.rel = "var2",
#'                pop.var = "v_pop" )
#'         
#' # use of  `row.variable.labels`     
#' summary_table( d = d.example,
#'                metric = c( "count", "percent" ),
#'                var1 = c( "v1", "v3" ),
#'                order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
#'                                   v3 = c( "Other Char 2", "Other Char 3" )),
#'                var2 = "v2",
#'                add.summary.row = TRUE,
#'                add.summary.col = TRUE,
#'                rate.supp = 5,
#'                count.supp = 5,
#'                percentages.rel = "var2",
#'                row.variable.labels = list( v1 = "var1",
#'                                            v3 = "var3"),
#'                pop.var = "v_pop" )
#' 
#' @export

summary_table <- function( d, var1, var2 = NULL, table.grouping = NULL, pop.var = NULL,
                           add.summary.row = TRUE, summary.row.name = NULL, add.summary.col = TRUE, font.family = "Calibri Light",
                           digs.perc = 1, digs.rate = 2, summary.col.name = NULL, summary.col.pos = "front", order.rows = NULL, order.cols = NULL, 
                           order.groups = NULL, foot.lines = c(), table.title = NULL, metric = c( "count", "rate" ), nm.var1 = NULL, 
                           count.supp = NULL, remove.cols = NULL, rate.supp = NULL, count.supp.symbol = "--", rate.supp.symbol = "*", 
                           per = 1000, NAs.footnote = FALSE, percentages.rel = "var1", include.percent.sign = TRUE, 
                           row.variable.labels = "default", col.variable.labels = "default", row.variable.col = "#eed8a4" ){
  
  ## checks for all conditions ##
  
  if( is.null( var1 ) ) stop( "`var1` cannot be NULL; if only one variable is desired for stratification purposes, it should be included in the `var1` argument" ) 
  
  if( eRTG3D::is.sf.3d( d ) ) d <- data.frame( d ) # if `sf` object, keep only attributes table
  
  if( length( var1 ) == 1 ){
    if( !is.null( order.rows ) & sum( order.rows %in% levels( as.factor( d[[ var1 ]] ) ) ) != length( levels( as.factor( d[[ var1 ]] ) ) ) ){
      warning( "`order.rows` does not match levels of `var1` in `d` exactly; this may be due to wanting to omit some rows from the outputted table" )
    }
    
    if( is.null( d[[ var1 ]] ) ){
      stop( "`var1` variable not detected in dataset." )
    }
    
    if( length( var2 ) == 1 ){
      if( !is.null( order.rows ) ){
        if( inherits( order.rows, "list" ) ){ 
          warning( "Length of `var1` is 1 and `order.rows` is a list but should be a vector with strings corresponding to the order of rows desired for `var1.` Ignoring `order.rows`.")
          
          order.rows <- NULL # coerce `order.rows` in this condition, otherwise throws error (in example "no errors with proper usage of `row.variable.labels`" in the test-summary_table.R script)
        }
      }
      
    }
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
    stop( "`pop.var` not specified but 'rate' was called in `metric`." )
  }
  
  if( !is.null( pop.var ) ){
    if(is.null( d[[ pop.var ]] ) ){
      stop( "`pop.var` variable not detected in dataset. Ensure that population count data (aggregate-level) are stored in `pop.var` variable." )
    }
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
  
  if( is.null( var1 ) & is.null( var2 ) ){
    stop( "At least one of `var1` or `var2` must be specified in the function call." )
  }
  
  if( length( table.grouping ) > 1 & !is.null( table.grouping ) ){
    stop( "There can only be one variable, maximum, specified in `table.grouping` for grouping the table." )
  }
  
  if( length( var2 ) > 1 & add.summary.col & ( !summary.col.pos %in% c( "front", "end" ) &
                                               !( inherits( summary.col.pos, "numeric" ) | inherits( summary.col.pos, "integer" ) ) ) ){
    stop( '`summary.col.pos` must be one of "front", "end", or an integer >= 0' )
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
  
  
  ## subsection where if length of `var1` is > 1 ##
  
  if( length( var1 ) > 1 | 
      length( var2 ) > 1 ){
    
    if( !is.null( table.grouping ) ) stop( "Current functionality of summary_table only supports multiple variables in the rows when only `var1` and `var2` are called but `table.grouping` is NULL" )
    
    if( length( var1 ) > 1 & !is.null( order.rows ) ){ 
      
      if( !inherits( order.rows, "list") ) stop( "If length( var1 ) > 1, `order.rows` must be an object of class 'list'. Otherwise, it should be an object of class 'character' " )
      
      if( !all( names( order.rows ) %in% var1 ) ) stop( "If length( var1 ) > 1, `order.rows` should be a named list consisting of character vectors with the elements of that list named the exact column name the ordering is being prescribed for." )
      
    }
    
    if( !is.null( row.variable.labels ) ){
      if( any( row.variable.labels != "none" ) ){
        
        if( !( inherits( row.variable.labels, "list" ) |
               inherits( row.variable.labels, "character" ) ) ) stop( "row.variable.labels must be either `none` or a named list with the variables specified in `var1` as the entry names and a character string with the desired label." )
        
        if( ( inherits( row.variable.labels, "list" ) |
              inherits( row.variable.labels, "character" ) ) &
            any( !names( row.variable.labels ) %in% var1 ) ) stop( "Names of the vector/list in `row.variable.labels` did not match the names of the variables in `var1` vector. row.variable.labels must be either `none` or a named list with the variables specified in `var1` as the entry names and a character string with the desired label." )
        
      }
    }
    
    # combination of variable calls that need to be made
    this.combo <- expand.grid( var1, var2 )
    
    # order the rows we want displayed
    colnames( this.combo ) <- c( "row", "col" )
    
    
    # get environment
    call_env <- summary_table_1( d, var1 = var1[1], var2 = var2[1], table.grouping = table.grouping, 
                                 pop.var = pop.var, add.summary.row = add.summary.row, summary.row.name = summary.row.name, 
                                 add.summary.col = add.summary.col, digs.perc = digs.perc, digs.rate = digs.rate, summary.col.name = summary.col.name, 
                                 order.rows = NULL, order.cols = NULL, order.groups = order.groups, foot.lines = foot.lines, 
                                 table.title = table.title, metric = metric, nm.var1 = nm.var1, 
                                 count.supp = count.supp, remove.cols = remove.cols, rate.supp = rate.supp, 
                                 count.supp.symbol = count.supp.symbol, rate.supp.symbol = rate.supp.symbol, per = per, 
                                 NAs.footnote = NAs.footnote, percentages.rel = percentages.rel, include.percent.sign = include.percent.sign )$env
    
    # load function call environment
    list2env( x = call_env, envir = current_env() ) # load elements needed from function call into current environment
    
    
    d.out <- lapply( 1:nrow( this.combo ), function( i ){
      
      row.var <- as.character( this.combo[ i, "row" ] )
      col.var <- as.character( this.combo[ i, "col" ] )
      
      if( inherits( order.rows, "list" ) ){
        arrange.rows <- order.rows[[ row.var ]]
      } else if( inherits( order.rows, "character" ) ){
        arrange.rows <- order.rows 
      } else if( is.null( order.rows ) ){
        arrange.rows <- c( sum.row.nm, levels( as.factor( d[[ row.var ]] )))
      }
      
      arrange.cols <- if( !is.null( order.cols[[ col.var ]] ) ) unique( c( sum.col.nm, order.cols[[ col.var ]] )) else c( sum.col.nm, levels( as.factor( d[[ col.var ]] ) ) )
      
      
      call1 <- summary_table_1( d, var1 = row.var, var2 = col.var, table.grouping = table.grouping, 
                                pop.var = pop.var, add.summary.row = add.summary.row, summary.row.name = summary.row.name, 
                                add.summary.col = add.summary.col, digs.perc = digs.perc, digs.rate = digs.rate, summary.col.name = summary.col.name, 
                                order.rows = NULL, order.cols = NULL, order.groups = order.groups, foot.lines = foot.lines, 
                                table.title = table.title, metric = metric, nm.var1 = nm.var1, 
                                count.supp = count.supp, remove.cols = remove.cols, rate.supp = rate.supp, 
                                count.supp.symbol = count.supp.symbol, rate.supp.symbol = rate.supp.symbol, per = per, 
                                NAs.footnote = NAs.footnote, percentages.rel = percentages.rel, include.percent.sign = include.percent.sign )
      
      
      d.call.out <- call1$frame %>%
        select( !!sym( row.var ), contains( arrange.cols ) ) %>% # this arranges the columns in the desired order (note that the column with the value `sum.col.nm` will be moved later)
        filter( !!sym( row.var ) %in% arrange.rows )  %>% # this filters out any levels of var1 indicated in `order.rows` that are not desired in the table
        arrange( match( !!sym( row.var ), arrange.rows ), # arrange rows in custom order;
                 .by_group = TRUE ) %>%
        rename( `var1` = !!sym( row.var ) ) %>%
        mutate( row_var_name = row.var ) %>% 
        { if( add.summary.row ){
          mutate( ., row_var_name = ifelse( `var1` == sum.row.nm, "", row_var_name ) ) # make summary rows the same so that `distinct` picks up and deletes redundant versions of them
        } else . }
      
      return( list( frame = d.call.out, sp.h = call1$env$sp.h, col.w = call1$env$col.w, 
                    new.nms = call1$env$new.nms, pos.vec = call1$env$pos.vec, col.var = col.var ) )
    })
    
    # extract frames
    frames.list <- lapply( seq_along( d.out ), function( i ) d.out[[ i ]]$frame )
    
    # since columns are only set once, we only need to keep one instance of each of `sp.h` and `col.w`
    col.ids <- sapply( d.out, function( x ) x$col.var ) # get column ids
    
    same.col.vars <- sapply( var2, function( x ) which( col.ids == x ) ) 
    
    # `same.col.vars` will not be in required matrix form in some cases, this converts vector form back to matrix form
    if( inherits( same.col.vars, "integer" ) ){
      
      same.col.vars <- matrix( same.col.vars, ncol = length( same.col.vars ) )
      
      colnames( same.col.vars ) <- var2
      
    }
    
    # get unique `sp.h` and `col.w` and `new.nms`
    sp.h.list <- lapply( as.vector( same.col.vars[1,] ), # first row only since we keep only 1 copy of each column
                         function( i ) d.out[[ i ]]$sp.h ) # spanning header labels
    
    col.w.list <- lapply( as.vector( same.col.vars[1,] ), # first row only since we keep only 1 copy of each column
                          function( i ) d.out[[ i ]]$col.w ) # spanning header widths
    
    new.nms.list <- lapply( as.vector( same.col.vars[1,] ), # first row only since we keep only 1 copy of each column
                            function( i ){
                              
                              this.out <- d.out[[ i ]]$new.nms 
                              
                              rmv.this.var1 <- which( var1 %in% names( this.out ) )
                              
                              names( this.out )[ names( this.out ) == var1[ rmv.this.var1 ] ] <- "var1" # this is a bug fix
                              
                              return( this.out )
                              
                              }) # header labels
    
    col.vars.list <- lapply( as.vector( same.col.vars[1,] ), # first row only since we keep only 1 copy of each column
                             function( i ) d.out[[ i ]]$col.var ) # this will be used for the spanning header of the multiple variables
    
    # spanning headers for all variables (second layer for a spanning header)
    if( all( col.variable.labels !=  "none" ) ){
      sp.h.2 <- lapply( seq_along( sp.h.list ), function( i ){
        
        data.frame( level = sp.h.list[[i]],
                    variable = col.vars.list[[i]] )
        
      }) %>% do.call( "rbind", . ) %>% 
        filter( !level %in% c( "", sum.col.nm ) ) %>% # first assign column widths to all variable levels that are not the summary column and the `var1` column which is length( metric )
        group_by( variable ) %>% 
        mutate( col.w = length( metric ) ) %>% 
        ungroup() %>% bind_rows( ., # now do the same but for summary column and `var1` column, which are length( metric ) and 1, respectively
                                 lapply( seq_along( sp.h.list ), function( i ){
                                   
                                   data.frame( level = sp.h.list[[i]],
                                               variable = col.vars.list[[i]] )
                                   
                                 }) %>% do.call( "rbind", . ) %>% 
                                   filter( level %in% c( "", sum.col.nm ) ) %>% 
                                   mutate( col.w = ifelse( level == sum.col.nm, length( metric ),
                                                           ifelse( level == "", 1, NA ) ),
                                           variable = ifelse( level == sum.col.nm, sum.col.nm,
                                                              ifelse( level == "", "", NA ) ) ) %>% 
                                   distinct() )
      
    }
    
    if( length( sp.h.list ) > 1 ){
      # assign names so we can track which column labels (for the levels) belong to which variable
      names( sp.h.list ) <- colnames( same.col.vars )
      
      names( col.w.list ) <- colnames( same.col.vars )
      
      names( new.nms.list ) <- colnames( same.col.vars )
      
      # remove unnecesary "All" column from all other sets of columns besides first variable in `var2`
      remove.all.these <- var2[-1]
      
      these.col.w.rem <- which( sp.h.list[[ remove.all.these ]] %in% c( "", sum.col.nm ) )
      
      these.new.nms.rem <- which( str_detect( names( new.nms.list[[ remove.all.these ]] ), paste0( "var1", "|", 
                                                                                                   sum.col.nm ) ) )
      
      # now remove
      sp.h.list[[ remove.all.these ]] <- sp.h.list[[ remove.all.these ]] %>%
        .[ !. %in% c( "", sum.col.nm ) ]
      
      col.w.list[[ remove.all.these ]] <- col.w.list[[ remove.all.these ]] %>%
        .[ -these.col.w.rem ]
      
      new.nms.list[[ remove.all.these ]] <- new.nms.list[[ remove.all.these ]] %>%
        .[ -these.new.nms.rem ]
      
    }
    
    sp.h.all <- as.vector( unlist( sp.h.list ) ) # final spanning header vector
    
    col.w.all <- as.vector( unlist( col.w.list ) ) # final spanning header column widths vector
    
    new.nms.all <- as.vector( unlist( new.nms.list ) ) # final spanning header column widths vector
    
    
    ## second layer of spanning header (for the various variables in `var2`) ##
    if( all( col.variable.labels !=  "none" ) ){
      # build the vector for the spanning header
      order.sp.h.2.all <- c( "", { if( add.summary.col  ) sum.col.nm }, var2 )
      
      sp.h.2.specs <- sp.h.2 %>% 
        summarise( col.w = sum( col.w ),
                   .by = variable ) %>% 
        arrange( match( variable,
                        order.sp.h.2.all ) ) # order frame in specified order
      
      # create final vector of second spanning header labels
      sp.h.2.all <- sp.h.2.specs %>% pull( variable ) %>% 
        { if( any( str_detect( sp.h.2.specs$variable, sum.col.nm ) ) ) str_replace( ., sum.col.nm, "" ) else . } # label for summary column will be "" if present
      
      # and its column widths
      col.w.2.all <- sp.h.2.specs %>% pull( col.w )
      
      names( col.w.2.all ) <- sp.h.2.all
    }
    
    # first we will join dataframes with same row_var_name since entries in d.out are organized by row variables first
    row.ids <- sapply( seq_along( frames.list ), function( x ) unique( frames.list[[ x ]][ "row_var_name" ] ) %>% 
                         .[ . != "" ] )
    
    same.row.vars <- sapply( var1, function( x ) which( row.ids == x ) ) %>% 
      data.frame()
    
    # join the datasets with same rows and then rowbind everything
    d.out2 <- suppressMessages( # suppress messages because of the join message that is generated
      if( ncol( same.row.vars ) > 1 | ( length( var1 ) == 1 & length( var2 ) > 1 ) ){
        
        lapply( 1:ncol( same.row.vars ), function( i ){
          
          frames.list[ same.row.vars[,i] ] %>% 
            do.call( "left_join", . )
          
        } ) %>% 
          do.call( "bind_rows", . )
      } else if ( length( var1 ) > 1 & length( var2 ) == 1 ) {
        frames.list  %>% 
          do.call( "bind_rows", . )
      } 
      
    )
    
    
    ## now onward
    
    ## add row separators/labels if desired for the different variables in the rows
    
    if( all( row.variable.labels == "default" ) | inherits( row.variable.labels, "list" ) ){
      
      for( i in seq_along( order.rows ) ){
        
        row.labels <- order.rows[[i]] %>% .[ .!= sum.row.nm ] # fix to get issue with summary row name throwing off positioning of the separator
        
        before.grp <- which( d.out2$var1 == row.labels[1] & d.out2$row_var_name == names( order.rows )[i] )
        
        label.it.row <- if( inherits( row.variable.labels, "list" ) ) unlist( row.variable.labels )[ names(order.rows)[i] ] else if( row.variable.labels == "default" ) var1[i]
        
        d.out2 <- d.out2 %>%
          add_row( var1 = label.it.row, .before = before.grp )
        
      }
      
      # vector of final outputted names in the table for bolding and coloring below in the flextable code
      row.var.labs <- if( inherits( row.variable.labels, "list" ) ) unlist( row.variable.labels ) else if( row.variable.labels == "default" ) var1
      
      row.var.labs.which <- which( d.out2$var1 %in% row.var.labs )
    }
    
    ## remove "row_var_name" column since it was only needed for arranging rows
    d.out3 <- d.out2 %>% select( -row_var_name )
    
    
    ## if desired, move position of summary column ##
    
    if( ( inherits( summary.col.pos, "numeric" ) | inherits( summary.col.pos, "integer" ) ) & !summary.col.pos %in% c( 0,1 ) & summary.col.pos >= 0 ){
      
      if( all( col.variable.labels !=  "none" ) ){
        col.variable.labels <- "none"
        
        warning( paste0( "Position of summary column was specified as the integer, ", summary.col.pos,
                         ", but spanning headers for variable names in `var2` cannot be generated when `summary.col.pos` is an integer.",
                         ' Coercing `col.variable.labels` to "none" and suppressing spanning headers for `var2`. If spanning headers ',
                         ' are desired, try specifying "front" or "end" for `summary.col.pos`.' ) )
        
      }
    }
    
    if( summary.col.pos != "front" ){
      
      if( summary.col.pos == "end" ){
        
        d.out3 <- d.out3 %>% 
          select( - contains( sum.col.nm ) ) %>% 
          bind_cols( ., d.out3 %>% 
                       select( contains( sum.col.nm ) ) )
        
        # reorder the `sp.h` vector (second layer for level names) for the spanning labels based on the desired repositioning
        sp.h.all <- sp.h.all %>% 
          .[ .!= sum.col.nm ] %>% 
          c( ., sum.col.nm  )
        
        
        if( all( col.variable.labels !=  "none" ) ){
          # reorder the `sp.h.all.2` vector (second layer for variable names) for the spanning labels based on the desired repositioning
          
          col.w.2.all <- c( sp.h.2.specs %>% 
                              filter( !variable %in% sum.col.nm ) %>% 
                              pull( col.w ), 
                            sp.h.2.specs %>% 
                              filter( variable %in% sum.col.nm ) %>% 
                              pull( col.w ) )
          
          names( col.w.2.all ) <- c( sp.h.2.specs %>% 
                                       filter( !variable %in% sum.col.nm ) %>% 
                                       pull( variable ), 
                                     sp.h.2.specs %>% 
                                       filter( variable %in% sum.col.nm ) %>% 
                                       pull( variable ) )
          
          sp.h.2.all <- names( col.w.2.all ) %>% 
            { if( any( str_detect( ., sum.col.nm ) ) ) str_replace( ., sum.col.nm, "" ) else . } # label for summary column will be "" if present
          
          sp.h.2.specs <- sp.h.2.specs %>% 
            arrange( match( variable, names( col.w.2.all ) ) )
        }
        
      } else if( inherits( summary.col.pos, "numeric" ) | inherits( summary.col.pos, "integer" ) ){
        
        
        if( summary.col.pos >= 0 ){
          if( summary.col.pos > length( sp.h.all %>% .[ .!= "" ] ) ) stop( paste0( "`summary.col.pos` cannot be > than ",
                                                                                   length( sp.h.all %>% .[ .!= "" ] ),
                                                                                   " which is the total number of levels for all variables listed in `var2`." ) )
          
          if( summary.col.pos <= length( sp.h.all %>% .[ .!= "" ] ) ){
            
            d.out3 <- if( summary.col.pos %in% c( 0, 1 ) ) d.out3 else{
              
              this.bump <- sp.h.all %>% .[ .!= "" ] %>% .[ summary.col.pos ] # the affected position
              
              this.new.pos <- max( which( str_detect( names( d.out3 ) , # take the first column where the level appears
                                                      paste0( "^",
                                                              this.bump, "\\," ) ) ) )
              suppressWarnings( #`this.new.pos` in the `.after` argument creates a warning message that can be ignored for now
                d.out3 %>%
                  relocate( contains( sum.col.nm ), .after = this.new.pos )
              )
            }
            
            # reorder the `sp.h` vector for the spanning labels based on the desired repositioning
            sp.h.all <- if( !summary.col.pos %in% c( 0, 1 ) ) sp.h.all[ 1:( which( sp.h.all == this.bump ) ) ] %>% 
              .[ .!= sum.col.nm ] %>% 
              c( ., sum.col.nm, sp.h.all[ ( which( sp.h.all == this.bump ) + 1 ): length( sp.h.all ) ] ) else sp.h.all
            
          }
          
        }
      }
      
      if( !summary.col.pos %in% c( "front", "end" ) &
          !( summary.col.pos >= 0 ) ) stop( '`summary.col.pos` must be one of "front", "end", or an integer >= 0' )
    }
    
    
    ## generate the final table with `flextable` ##
    
    # recompute `pos.vec.all`: columns to add the vertical border lines to (to the right of)
    pos.vec.all <- vector()
    for( i in 1:( length( col.w.all ) - 1 ) ){
      
      if( i == 1 ){
        
        pos.vec.all[i] <- 1
        
      } else{
        
        pos.vec.all[i] <- pos.vec.all[i-1] + length( metric )
        
      }
      
    }
    
    ## 2nd spanning header labels relabeling and vertical line borders ##
    if( all( col.variable.labels !=  "none" ) ){
      
      ## reassign spanning header labels (second layer) if assigned in `col.variable.labels`
      sp.h.2.all <- if( all( col.variable.labels == "default" ) ) sp.h.2.all else if( inherits( col.variable.labels, "list" ) & all( names( col.variable.labels ) %in% var2 ) ){
        
        sapply( sp.h.2.all, function( x ){
          if( x %in% names( col.variable.labels ) ) col.variable.labels[[x]] else x
        } )
      } else stop( '`col.variable.labels` should be a named list with variables in var2 as the entry names or "default" if original column names are desired or "none" if spanning headers are not desired.' )
      
      ## positions for vertical lines for second layer spanning header ##
      pos.vec.2.all <- c( sum( col.w.2.all[ names( col.w.2.all ) == "" ] ) ) # start position for first line
      
      for( i in seq_along( col.w.2.all ) ){
        
        if( names( col.w.2.all )[i] == "" ) next
        
        if( names( col.w.2.all )[i] != "" ){
          if( i == length( col.w.2.all ) ) next else{
            pos.vec.2.all <- c( pos.vec.2.all,
                                tail( pos.vec.2.all, 1 ) + col.w.2.all[i] )
          }
        }
      }
    }
    
    # footnote for table
    if( !is.null( foot.lines ) ){
      
      fn <- as_paragraph( foot.lines )
      
    }
    
    
    # generate the final table with `flextable`
    
    if( !tb.grp.var2.wide.logic ){
      
      t.out <- { if( tb.grp.logic ){ as_grouped_data( d.out3, groups = "group" ) %>%
          flextable::as_flextable( hide_grouplabel = TRUE )  %>% # first two lines of this command are used to create the grouping headers within the table
          surround( i = ~ !is.na( group ), # this `surround` command is to add the top horizontal and bottom horizonal lines to the grouping header title
                    border.top = fp_border_default( width = 0.1 ), 
                    border.bottom = fp_border_default( width = 0.1 ),
                    part = "body") 
      } else{ flextable::flextable( d.out3 ) } } %>%
        { if( length( metric ) > 1 & var.logic.2 ){ # spanning headers for `var2` only if more than one metric is requested and if there is cross-tabulation
          add_header_row( ., colwidths = col.w.all,
                          values = sp.h.all ) 
        } else{ . } } %>% 
        { if( all( col.variable.labels !=  "none" ) ){
          add_header_row( ., colwidths = as.numeric( col.w.2.all ),
                          values = sp.h.2.all ) 
        } else . } %>% 
        theme_zebra() %>%
        theme_vanilla() %>%
        set_header_labels( values = new.nms.all ) %>% # change names based on new.nms vector created outside this flextable command
        { if( !is.null( nm.var1 ) ){
          set_header_labels( x = ., values = list( var1 = nm.var1 ) ) } else .} %>% # if nm.var is specified
        align( part = "header", 
               align = "center" ) %>% 
        { if( !is.null( table.title ) ) add_header_lines( ., values = table.title ) %>% # title line
            align( part = "header", i = 1, align = "left" ) else . } %>% #left-align title
        { if( all( col.variable.labels !=  "none" ) ){
          vline( ., j = pos.vec.2.all, 
                 part = "header",
                 border = fp_border_default( width = 0.1 ) ) # add vertical line borders after each spanning header group for second layer (variable names spanning header)
        } else . } %>% 
        vline( j = pos.vec.all, 
               part = "body",
               border = fp_border_default( width = 0.1 ) ) %>% # add vertical line borders after each spanning header group
        border_inner_h( border = fp_border_default( width = 0.1 ), part = "header" ) %>% # make header border lines thinner
        hline_top( border = fp_border_default( width = 0.1 ), part = "body" ) %>% # make body top border line thinner
        flextable::font( fontname = font.family, part = "all" ) %>% # change font for entire table based on `font.family`
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
        padding( padding.top = 8, 
                 padding.bottom = 8, 
                 padding.left = 4, 
                 padding.right = 4, 
                 part = "header" ) %>%
        padding( padding.top = 2.5, 
                 padding.bottom = 0,
                 part = "footer" ) %>%
        padding( padding.top = 2.5, padding.bottom = 2.5,part = "footer", i = 1 ) %>%
        flextable::height( height = 0.002, part = "body", unit = "cm" ) %>%
        bg( bg = "white", part = "footer" ) %>% # color footer white (undoes `theme_zebra` styling)
        border_inner_h( border = fp_border( color = "white", style = "solid", width = 0 ),
                        part = "footer") %>% # remove footer horizontal line (undoes `theme_zebra` styling)
        bold( bold = FALSE, part = "footer" ) %>% # unbold footer text (undoes `theme_zebra` bolding)
        # below is the formatting for the variable label rows
        { if( all( row.variable.labels =="default" ) | inherits( row.variable.labels, "list" ) ){
          bold( x = ., i = row.var.labs.which ) %>%
            merge_h_range( i = row.var.labs.which,
                           j1 = 1, j2 = ncol_keys(.), part = "body" ) %>% # merge all cells in the header variable label rows
            { if ( row.variable.col != "none" ) bg( x = ., i = row.var.labs.which, bg = row.variable.col ) else . } 
        } else . }
    }
    
    # for wider table when there is only 1 level of var2 but var2 and table.grouping are specified
    if( tb.grp.var2.wide.logic ){
      
      t.out <- flextable::flextable( d.out3) %>%
        { if( length( metric ) > 1 & var.logic.2 ){ # spanning headers for `var2` only if more than one metric is requested and if there is cross-tabulation
          add_header_row( ., colwidths = col.w.all,
                          values = sp.h.all ) 
        } else{ . } } %>% 
        theme_zebra() %>%
        theme_vanilla() %>%
        set_header_labels( values = new.nms.all ) %>% # change names based on new.nms vector created outside this flextable command
        { if( !is.null( nm.var1 ) ){
          set_header_labels( x = ., values = list( var1 = nm.var1 ) ) } else .} %>% # if nm.var is specified
        align( part = "header", i = 1, align = "center" ) %>% # center align spanning headers
        { if( !is.null( table.title ) ) add_header_lines( ., values = table.title ) %>% # title line
            align( part = "header", i = 1, align = "left" ) else . } %>% #left-align title
        vline( j = pos.vec.all, 
               part = "body",
               border = fp_border_default( width = 0.1 ) ) %>% # add vertical line borders after each spanning header group
        border_inner_h( border = fp_border_default( width = 0.1 ), part = "header" ) %>% # make header border lines thinner
        hline_top( border = fp_border_default( width = 0.1 ), part = "body" ) %>% # make body top border line thinner
        flextable::font( fontname = font.family, part = "all" ) %>% # change font for entire table based on `font.family`
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
        padding( padding.top = 8, 
                 padding.bottom = 8, 
                 padding.left = 4, 
                 padding.right = 4, 
                 part = "header" ) %>%
        padding( padding.top = 2.5, 
                 padding.bottom = 0,
                 part = "footer" ) %>%
        padding( padding.top = 2.5, padding.bottom = 2.5,part = "footer", i = 1 ) %>%
        width( width = 0.95, j = var1 ) %>% # change column widths to fit on one page
        flextable::height( height = 0.002, part = "body", unit = "cm" ) %>%
        bg( bg = "white", part = "footer" ) %>% # color footer white (undoes `theme_zebra` styling)
        border_inner_h( border = officer::fp_border(color = "white", style = "solid", width = 0),
                        part = "footer") %>% # remove footer horizontal line (undoes `theme_zebra` styling)
        bold( bold = FALSE, part = "footer" ) %>% # unbold footer text (undoes `theme_zebra` bolding)
        # below is the formatting for the variable label rows
        { if( row.variable.labels =="default" | inherits( row.variable.labels, "list" ) ){
          bold( x = ., i = row.var.labs.which ) %>%
            merge_h_range( i = row.var.labs.which,
                           j1 = 1, j2 = ncol_keys(.), part = "body" ) %>% # merge all cells in the header variable label rows
            { if ( row.variable.col != "none" ) bg( x = ., i = row.var.labs.which, bg = row.variable.col ) else . } 
        } else . }
    }
    
    ## footnotes ##
    
    # add foot notes for missing values iteratively
    
    
    if( NAs.footnote ){
      
      warning( "Parameter `NAs.footnote` not operational if length( v1 ) > 1. Note, no footnotes regarding missing values will be outputted in the final table. You can add manual footnotes using `flextable` functions (e.g., `add_footer-lines` or `footnote`) directly to the `flextable` object outputted from this function." )
      
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
    include.ft.nt.cell.supp <- length( d.out3[ d.out3 == count.supp.symbol ] ) > 0 # logical for if there are any suppressed cells in the table, otherwise footnote not needed
    
    if( !is.null( count.supp ) & "count" %in% metric & include.ft.nt.cell.supp ){
      
      if( count.supp > 0 ){
        
        t.out <- t.out %>%
          add_footer_lines( values = paste0( count.supp.symbol, " Cell counts \U2264 ", count.supp," were suppressed to protect confidentiality." ) ) %>% # note the use of the UNICODE in this string
          fontsize( size = 7, part = "footer" ) %>%
          line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
          padding( padding.top = 2.5, padding.bottom = 0, part = "footer", i = 1 )
        
      } 
      
    }
    
    include.ft.nt.rate.supp <- length( d.out3[ d.out3 == rate.supp.symbol ] ) > 0 # logical for if there are any suppressed cells in the table, otherwise footnote not needed
    
    if( !is.null( rate.supp ) & "rate" %in% metric & include.ft.nt.rate.supp ){
      
      if( rate.supp > 0 ){
        
        t.out <- t.out %>%
          add_footer_lines( values = paste0( rate.supp.symbol, " Rates based on \U2264 ", rate.supp, " events are not presented since such rates are subject to instability." ) ) %>% # note the use of the UNICODE in this string
          fontsize( size = 7, part = "footer" ) %>%
          line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
          padding( padding.top = 2.5, padding.bottom = 0, part = "footer", i = 1 )
      }
    }
    
    
    
    
    return( list( flextable = t.out,
                  frame = d.out3 ) )
    
  }
  
  else{ 
    
    ## subsection if length `var1` == 1
    call2 <- summary_table_1( d, var1 = var1, var2 = var2, table.grouping = table.grouping, 
                              pop.var = pop.var, add.summary.row = add.summary.row, summary.row.name = summary.row.name, 
                              add.summary.col = add.summary.col, digs.perc = digs.perc, digs.rate = digs.rate, summary.col.name = summary.col.name, 
                              order.rows = order.rows, order.cols = order.cols, order.groups = order.groups, foot.lines = foot.lines, 
                              table.title = table.title, metric = metric, nm.var1 = nm.var1, 
                              count.supp = count.supp, remove.cols = remove.cols, rate.supp = rate.supp, 
                              count.supp.symbol = count.supp.symbol, rate.supp.symbol = rate.supp.symbol, per = per, 
                              NAs.footnote = NAs.footnote, percentages.rel = percentages.rel, include.percent.sign = include.percent.sign )
    
    
    return( list( flextable = call2$flextable,
                  frame = call2$frame ) )
  }
}
