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
#' @export

summary_table <- function( d, var1, var2 = NULL, table.grouping = NULL, pop.var = NULL,
                           add.summary.row = TRUE, summary.row.name = NULL, add.summary.col = TRUE, digs.perc = 1, digs.rate = 2,
                           summary.col.name = NULL, order.rows = NULL, order.cols = NULL, order.groups = NULL, foot.lines = c(), 
                           table.title = NULL, metric = c( "count", "rate" ), nm.var1 = NULL, count.supp = NULL, remove.cols = NULL,
                           rate.supp = NULL, count.supp.symbol = "--", rate.supp.symbol = "*", 
                           per = 1000, NAs.footnote = FALSE, percentages.rel = "var1",
                           include.percent.sign = TRUE ){
  
  if( length( var1 ) > 1 & 
      !is.null( var2 ) & is.null( table.grouping ) ){
    
    if( length( var1 ) > 1 & !is.null( order.rows ) ){ 
      
      if( !inherits( order.rows, "list") ) stop( "If length( var1 ) > 1, `order.rows` must be an object of class 'list'. Otherwise, it should be an object of class 'character' " )
      
      if( !all( names( order.rows ) %in% var1 ) ) stop( "If length( var1 ) > 1, `order.rows` should be a named list consisting of character vectors with the elements of that list named the exact column name the ordering is being prescribed for." )
      
    }
    
    
    # get environment
    call_env <- summary_table_1( d, var1 = var1[1], var2 = var2, table.grouping = table.grouping, 
                                 pop.var = pop.var, add.summary.row = add.summary.row, summary.row.name = summary.row.name, 
                                 add.summary.col = add.summary.col, digs.perc = digs.perc, digs.rate = digs.rate, summary.col.name = summary.col.name, 
                                 order.rows = NULL, order.cols = order.cols, order.groups = order.groups, foot.lines = foot.lines, 
                                 table.title = table.title, metric = metric, nm.var1 = nm.var1, 
                                 count.supp = count.supp, remove.cols = remove.cols, rate.supp = rate.supp, 
                                 count.supp.symbol = count.supp.symbol, rate.supp.symbol = rate.supp.symbol, per = per, 
                                 NAs.footnote = NAs.footnote, percentages.rel = percentages.rel, include.percent.sign = include.percent.sign )$env
    
    # load function call environment
    list2env( x = call_env, envir = current_env() ) # load elements needed from function call into current environment
    
    
    d.out <- lapply( var1, function(x){
      
      arrange.rows <- if( !is.null( order.rows[[x]] ) ) order.rows[[x]] else levels( as.factor( d[[ x ]] ) )
      
      if( 
          !sum.row.nm %in% arrange.rows ) arrange.rows <- c( sum.row.nm, arrange.rows ) # by default put summary row as first row if it is requested
      
      call1 <- summary_table_1( d, var1 = x, var2 = var2, table.grouping = table.grouping, 
                                pop.var = pop.var, add.summary.row = add.summary.row, summary.row.name = summary.row.name, 
                                add.summary.col = add.summary.col, digs.perc = digs.perc, digs.rate = digs.rate, summary.col.name = summary.col.name, 
                                order.rows = NULL, order.cols = order.cols, order.groups = order.groups, foot.lines = foot.lines, 
                                table.title = table.title, metric = metric, nm.var1 = nm.var1, 
                                count.supp = count.supp, remove.cols = remove.cols, rate.supp = rate.supp, 
                                count.supp.symbol = count.supp.symbol, rate.supp.symbol = rate.supp.symbol, per = per, 
                                NAs.footnote = NAs.footnote, percentages.rel = percentages.rel, include.percent.sign = include.percent.sign )
      
      
      call1$frame %>%
        filter( !!sym( x ) %in% arrange.rows )  %>% # this filters out any levels of var1 indicated in `order.rows` that are not desired in the table
        arrange( match( !!sym( x ), arrange.rows ), # arrange rows in custom order;
                 .by_group = TRUE ) %>%
        rename( `var1` = !!sym( x ) ) 
    }) %>%
      do.call( "rbind", . ) %>%
      distinct() # ensures add.summary.row row appears only once in table when binding from different calls
    
    
    
    if( !is.null( nm.var1 ) ) colnames( d.out )[ colnames( d.out ) == "var1" ] <- nm.var1
    
    ## generate the final table with `flextable` ##
    
    # footnote for table
    if( !is.null( foot.lines ) ){
      
      fn <- as_paragraph( foot.lines )
      
    }
    
    
    ### make
    
    # generate the final table with `flextable`
    
    if( !tb.grp.var2.wide.logic ){
      
      t.out <- { if( tb.grp.logic ){ as_grouped_data( d.out, groups = "group" ) %>%
          flextable::as_flextable( hide_grouplabel = TRUE )  %>% # first two lines of this command are used to create the grouping headers within the table
          surround( i = ~ !is.na( group ), # this `surround` command is to add the top horizontal and bottom horizonal lines to the grouping header title
                    border.top = fp_border_default( width = 0.1 ), 
                    border.bottom = fp_border_default( width = 0.1 ),
                    part = "body") 
      } else{ flextable::flextable( d.out ) } } %>%
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
        border_inner_h( border = fp_border(color = "white", style = "solid", width = 0),
                        part = "footer") %>% # remove footer horizontal line (undoes `theme_zebra` styling)
        bold( bold = FALSE, part = "footer" ) # unbold footer text (undoes `theme_zebra` bolding)
    }
    
    # for wider table when there is only 1 level of var2 but var2 and table.grouping are specified
    if( tb.grp.var2.wide.logic ){
      
      t.out <- flextable::flextable( d.out) %>%
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
                        part = "footer")%>% # remove footer horizontal line (undoes `theme_zebra` styling)
        bold( bold = FALSE, part = "footer" ) # unbold footer text (undoes `theme_zebra` bolding)
      
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
    include.ft.nt.cell.supp <- length( d.out[ d.out == count.supp.symbol ] ) > 0 # logical for if there are any suppressed cells in the table, otherwise footnote not needed
    
    if( !is.null( count.supp ) & "count" %in% metric & include.ft.nt.cell.supp ){
      
      if( count.supp > 0 ){
        
        t.out <- t.out %>%
          add_footer_lines( values = paste0( count.supp.symbol, " Cell counts \U2264 ", count.supp," were suppressed to protect confidentiality." ) ) %>% # note the use of the UNICODE in this string
          fontsize( size = 7, part = "footer" ) %>%
          line_spacing( space = 0.7, part = "footer" ) %>% # make spacing in footnote single and not double
          padding( padding.top = 2.5, padding.bottom = 0, part = "footer", i = 1 )
        
      } 
      
    }
    
    include.ft.nt.rate.supp <- length( d.out[ d.out == rate.supp.symbol ] ) > 0 # logical for if there are any suppressed cells in the table, otherwise footnote not needed
    
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
                  frame = d.out ) )
    
  }
  
  else{
    
    call2 <- summary_table_1( d, var1 = var1, var2 = var2, table.grouping = table.grouping, 
                              pop.var = pop.var, add.summary.row = add.summary.row, summary.row.name = summary.row.name, 
                              add.summary.col = add.summary.col, digs.perc = digs.perc, digs.rate = digs.rate, summary.col.name = summary.col.name, 
                              order.rows = NULL, order.cols = order.cols, order.groups = order.groups, foot.lines = foot.lines, 
                              table.title = table.title, metric = metric, nm.var1 = nm.var1, 
                              count.supp = count.supp, remove.cols = remove.cols, rate.supp = rate.supp, 
                              count.supp.symbol = count.supp.symbol, rate.supp.symbol = rate.supp.symbol, per = per, 
                              NAs.footnote = NAs.footnote, percentages.rel = percentages.rel, include.percent.sign = include.percent.sign )
    
    
    return( list( flextable = call2$flextable,
                  frame = call2$frame ) )
  }
}

