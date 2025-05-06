test_that("`pop.var` missing", {
  expect_error( summary_table( d = d.example.na %>%
                                 select( -v_pop ),
                               metric = c( "count", "rate" ),
                               var1 = "v1",
                               var2 = "v2",
                               nm.var1 = "Variable 1",
                               add.summary.row =T,
                               add.summary.col = T,
                               foot.lines = c("This is the first footer line",
                                              "This is the second footer line" ), # manual footer lines
                               remove.cols = "Char 3",
                               percentages.rel = "var2",
                               count.supp = 50, # suppress values/counts in table <= 50
                               rate.supp = 50, # suppress rate calculation when counts are <= 50
                               NAs.footnote = TRUE, # produces the foornote of excluded observations
                               pop.var = "v_pop",
                               table.title = "This is Table 1" ) # title
  )
})


test_that("`metric` called 'rate' but no `pop.var` specified", {
  expect_error( summary_table( d = d.example.na,
                               metric = c( "count", "rate" ),
                               var1 = "v1",
                               var2 = "v2",
                               nm.var1 = "Variable 1",
                               add.summary.row =T,
                               add.summary.col = T,
                               foot.lines = c("This is the first footer line",
                                              "This is the second footer line" ), # manual footer lines
                               remove.cols = "Char 3",
                               percentages.rel = "var2",
                               count.supp = 50, # suppress values/counts in table <= 50
                               rate.supp = 50, # suppress rate calculation when counts are <= 50
                               NAs.footnote = TRUE, # produces the foornote of excluded observations
                               pop.var = NULL,
                               table.title = "This is Table 1" ) # title
  )
})


test_that( "Ensure `summary.col.name` column total equals number of rows in input dataset.", {
  
  t1 <- summary_table( d = d.example,
                       metric = c( "count", "percent" ),
                       var1 = "v1",
                       add.summary.row = TRUE,
                       add.summary.col = TRUE,
                       summary.col.name = "All",
                       summary.row.name = "Summ Row",
                       var2 = "v2",
                       percentages.rel = "var1" )$frame
  
  # column total
  count.tot.c <- t1 %>% 
    filter( v1 != "Summ Row" ) %>%
    select( contains( "All" ) & contains( "count" ) ) %>%
    pull() %>%
    sum(.)
  
  # row total
  count.tot.r <- t1 %>% 
    filter( v1 == "Summ Row" ) %>%
    select( contains( "All" ) & contains( "count" ) ) %>%
    pull()
  
  expect_equal( nrow( d.example ), count.tot.c )
  
})


test_that( "Ensure `summary.row.name` row total equals number of rows in input dataset.", {
  
  t1 <- summary_table( d = d.example,
                       metric = c( "count", "percent" ),
                       var1 = "v1",
                       add.summary.row = TRUE,
                       add.summary.col = TRUE,
                       summary.col.name = "All",
                       summary.row.name = "Summ Row",
                       var2 = "v2",
                       percentages.rel = "var1" )$frame
  
  
  # row total
  count.tot.r <- t1 %>% 
    filter( v1 == "Summ Row" ) %>%
    select( contains( "All" ) & contains( "count" ) ) %>%
    pull()
  
  expect_equal( nrow( d.example ), count.tot.r )
  
})

test_that( "Two metrics, max", {
  
  expect_error( summary_table( d = d.example,
                               metric = c( "count", "percent", "rate" ),
                               var1 = "v1",
                               add.summary.row = TRUE,
                               add.summary.col = TRUE,
                               summary.col.name = "All",
                               summary.row.name = "Summ Row",
                               var2 = "v2",
                               percentages.rel = "var1" ) )
  
})


test_that( "ensure that 0's are not suppressed.", {
  
  ## ensure that 0's are not suppressed in counts, percentages and rates
  
  # create some zero counts in a cross-tab
  d.zero <- d.example %>%
    mutate( v2 = ifelse( v1 == "Geo 1" & v2 == "Char 2", "Char 3", 
                         v2 ) ) # the cross tab of Geo 1 and Char 2 should now be 0
  
  # run the function
  test1 <- summary_table( d = d.zero,
                          metric = c( "percent", "count" ),
                          var1 = "v1",
                          var2 = "v2",
                          add.summary.row = TRUE,
                          add.summary.col = TRUE,
                          rate.supp = 5,
                          count.supp = 5,
                          percentages.rel = "var2",
                          pop.var = "v_pop" )$frame
  
  # test for rates
  expect_equal( 0, test1 %>%
                  filter( v1 == "Geo 1" ) %>%
                  select( contains( "Char 2" ) &
                            contains( "percent" ) ) %>%
                  pull() %>% 
                  str_remove_all( ., "\\%" ) %>% as.numeric() )
  
  # test for counts
  expect_equal( 0, test1 %>%
                  filter( v1 == "Geo 1" ) %>%
                  select( contains( "Char 2" ) &
                            contains( "count" ) ) %>%
                  pull() %>% as.numeric() )
})


test_that( "ensure that 0's are not suppressed v2.", {
  ## ensure that 0's are not suppressed in counts, percentages and rates but set add.summary.col and add.summary.row to FALSE (Both)
  
  # create some zero counts in a cross-tab
  d.zero <- d.example %>%
    mutate( v2 = ifelse( v1 == "Geo 1" & v2 == "Char 2", "Char 3", 
                         v2 ) ) # the cross tab of Geo 1 and Char 2 should now be 0
  
  # run the function
  test1 <- summary_table( d = d.zero,
                          metric = c( "percent", "count" ),
                          var1 = "v1",
                          var2 = "v2",
                          add.summary.row = FALSE,
                          add.summary.col = FALSE,
                          rate.supp = 5,
                          count.supp = 5,
                          percentages.rel = "var2",
                          pop.var = "v_pop" )$frame
  
  # test for rates
  expect_equal( 0, test1 %>%
                  filter( v1 == "Geo 1" ) %>%
                  select( contains( "Char 2" ) &
                            contains( "percent" ) ) %>%
                  pull() %>% 
                  str_remove_all( ., "\\%" ) %>% as.numeric() )
  
  # test for counts
  expect_equal( 0, test1 %>%
                  filter( v1 == "Geo 1" ) %>%
                  select( contains( "Char 2" ) &
                            contains( "count" ) ) %>%
                  pull() %>% as.numeric() )
})


test_that( "ensure no error when rate is called but not count.", {
  
  expect_no_error( summary_table( d = d.example,
                                  metric = c( "percent", "rate" ),
                                  var1 = "v1",
                                  var2 = "v2",
                                  add.summary.row = FALSE,
                                  add.summary.col = FALSE,
                                  rate.supp = 5,
                                  count.supp = 5,
                                  percentages.rel = "var2",
                                  pop.var = "v_pop" )
  ) 
  
  expect_warning(
    expect_no_error( summary_table( d = d.example,
                                    metric = c( "percent", "rate" ),
                                    var1 = "v1",
                                    var2 = "v2",
                                    add.summary.row = TRUE,
                                    add.summary.col = FALSE,
                                    rate.supp = 5,
                                    count.supp = 5,
                                    percentages.rel = "var2",
                                    pop.var = "v_pop" )
    )
  )
  
  expect_warning(
    expect_no_error( summary_table( d = d.example,
                                    metric = c( "percent", "rate" ),
                                    var1 = "v1",
                                    var2 = "v2",
                                    add.summary.row = TRUE,
                                    add.summary.col = TRUE,
                                    rate.supp = 5,
                                    count.supp = 5,
                                    percentages.rel = "var2",
                                    pop.var = "v_pop" ) )
  )
  
})


test_that( "check equality of contents of table (percentages, counts) when `percentages.rel` = 'var2'.", {
  
  
  # call function
  func.1 <- summary_table( d = d.example,
                           metric = c( "count", "percent" ),
                           var1 = "v1",
                           var2 = "v2",
                           add.summary.row = TRUE,
                           add.summary.col = TRUE,
                           rate.supp = 5,
                           count.supp = 5,
                           percentages.rel = "var2",
                           pop.var = "v_pop" )
  
  # extract values from function-generated table
  ths <- names( func.1$frame )[ str_which( names( func.1$frame ), "percent" ) ] %>%
    str_remove( ., ",percent" ) %>%
    .[ .!= "All"]
  
  # use base functions to generate same table of counts
  base.1 <- table( d.example$v1, d.example$v2) %>%
    as.matrix()
  
  
  # use base functions to generate same table of percentages
  base.perc <- base.1
  for( i in 1:ncol( base.1 ) ){
    
    base.perc[, i ] <- ( 100 * base.1[, i ] / sum( base.1[, i ] ) ) %>%
      
      sprintf( paste0( "%.", 1, "f" ), . )
  }
  
  # check equality of counts
  for( i in seq_along( ths ) ){
    
    func.vec <- func.1$frame %>%
      filter( v1 != "Summary Row" ) %>%
      pull( !!sym( paste0( ths[i], ",count") ) )
    
    base.vec <- base.1[ , ths[i] ]
    
    # final check of condition, ensuring equality
    expect_true( all( func.vec == base.vec ) )
    
  }
  
  
  # check equality of percentages
  for( i in seq_along( ths ) ){
    
    func.vec <- func.1$frame %>%
      filter( v1 != "Summary Row" ) %>%
      pull( !!sym( paste0( ths[i], ",percent") ) ) %>%
      str_remove_all( ., "\\%" )
    
    base.vec <- base.perc[ , ths[i] ]
    
    # final check of condition, ensuring equality
    expect_true( all( func.vec == base.vec ) )
    
  }
}
)


test_that( "check equality of contents of table (percentages, counts) when `percentages.rel` = 'var1'.", {
  
  
  # call function
  func.1 <- summary_table( d = d.example,
                           metric = c( "count", "percent" ),
                           var1 = "v1",
                           var2 = "v2",
                           add.summary.row = TRUE,
                           add.summary.col = TRUE,
                           rate.supp = 5,
                           count.supp = 5,
                           percentages.rel = "var1",
                           pop.var = "v_pop" )
  
  # extract values from function-generated table
  ths <- names( func.1$frame )[ str_which( names( func.1$frame ), "percent" ) ] %>%
    str_remove( ., ",percent" ) %>%
    .[ .!= "All"]
  
  # use base functions to generate same table of counts
  base.1 <- table( d.example$v1, d.example$v2) %>%
    as.matrix()
  
  
  # use base functions to generate same table of percentages
  base.perc <- base.1
  for( i in 1:nrow( base.1 ) ){
    
    base.perc[ i, ] <- ( 100 * base.1[ i, ] / sum( base.1[ i, ] ) ) %>%
      
      sprintf( paste0( "%.", 1, "f" ), . )
  }
  
  # check equality of counts
  for( i in seq_along( ths ) ){
    
    func.vec <- func.1$frame %>%
      filter( v1 != "Summary Row" ) %>%
      pull( !!sym( paste0( ths[i], ",count") ) )
    
    base.vec <- base.1[ , ths[i] ]
    
    # final check of condition, ensuring equality
    expect_true( all( func.vec == base.vec ) )
    
  }
  
  
  # check equality of percentages
  for( i in seq_along( ths ) ){
    
    func.vec <- func.1$frame %>%
      filter( v1 != "Summary Row" ) %>%
      pull( !!sym( paste0( ths[i], ",percent") ) ) %>%
      str_remove_all( ., "\\%" )
    
    base.vec <- base.perc[ , ths[i] ]
    
    # final check of condition, ensuring equality
    expect_true( all( func.vec == base.vec ) )
    
  }
}
)


test_that( "check equality of contents of table (percentages, counts) when `percentages.rel` = 'var2' & add.summary.row and add.summary.col are FALSE.", {
  
  
  # call function
  func.1 <- summary_table( d = d.example,
                           metric = c( "count", "percent" ),
                           var1 = "v1",
                           var2 = "v2",
                           add.summary.row = FALSE,
                           add.summary.col = FALSE,
                           rate.supp = 5,
                           count.supp = 5,
                           percentages.rel = "var2",
                           pop.var = "v_pop" )
  
  # extract values from function-generated table
  ths <- names( func.1$frame )[ str_which( names( func.1$frame ), "percent" ) ] %>%
    str_remove( ., ",percent" ) %>%
    .[ .!= "All"]
  
  # use base functions to generate same table of counts
  base.1 <- table( d.example$v1, d.example$v2) %>%
    as.matrix()
  
  
  # use base functions to generate same table of percentages
  base.perc <- base.1
  for( i in 1:ncol( base.1 ) ){
    
    base.perc[, i ] <- ( 100 * base.1[, i ] / sum( base.1[, i ] ) ) %>%
      
      sprintf( paste0( "%.", 1, "f" ), . )
  }
  
  # check equality of counts
  for( i in seq_along( ths ) ){
    
    func.vec <- func.1$frame %>%
      filter( v1 != "Summary Row" ) %>%
      pull( !!sym( paste0( ths[i], ",count") ) )
    
    base.vec <- base.1[ , ths[i] ]
    
    # final check of condition, ensuring equality
    expect_true( all( func.vec == base.vec ) )
    
  }
  
  
  # check equality of percentages
  for( i in seq_along( ths ) ){
    
    func.vec <- func.1$frame %>%
      filter( v1 != "Summary Row" ) %>%
      pull( !!sym( paste0( ths[i], ",percent") ) ) %>%
      str_remove_all( ., "\\%" )
    
    base.vec <- base.perc[ , ths[i] ]
    
    # final check of condition, ensuring equality
    expect_true( all( func.vec == base.vec ) )
    
  }
}
)


test_that( "check equality of contents of table (percentages, counts) when `percentages.rel` = 'var1' & add.summary.row and add.summary.col are FALSE.", {
  
  
  # call function
  func.1 <- summary_table( d = d.example,
                           metric = c( "count", "percent" ),
                           var1 = "v1",
                           var2 = "v2",
                           add.summary.row = FALSE,
                           add.summary.col = FALSE,
                           rate.supp = 5,
                           count.supp = 5,
                           percentages.rel = "var1",
                           pop.var = "v_pop" )
  
  # extract values from function-generated table
  ths <- names( func.1$frame )[ str_which( names( func.1$frame ), "percent" ) ] %>%
    str_remove( ., ",percent" ) %>%
    .[ .!= "All"]
  
  # use base functions to generate same table of counts
  base.1 <- table( d.example$v1, d.example$v2) %>%
    as.matrix()
  
  
  # use base functions to generate same table of percentages
  base.perc <- base.1
  for( i in 1:nrow( base.1 ) ){
    
    base.perc[i, ] <- ( 100 * base.1[i, ] / sum( base.1[i, ] ) ) %>%
      
      sprintf( paste0( "%.", 1, "f" ), . )
  }
  
  # check equality of counts
  for( i in seq_along( ths ) ){
    
    func.vec <- func.1$frame %>%
      filter( v1 != "Summary Row" ) %>%
      pull( !!sym( paste0( ths[i], ",count") ) )
    
    base.vec <- base.1[ , ths[i] ]
    
    # final check of condition, ensuring equality
    expect_true( all( func.vec == base.vec ) )
    
  }
  
  
  # check equality of percentages
  for( i in seq_along( ths ) ){
    
    func.vec <- func.1$frame %>%
      filter( v1 != "Summary Row" ) %>%
      pull( !!sym( paste0( ths[i], ",percent") ) ) %>%
      str_remove_all( ., "\\%" )
    
    base.vec <- base.perc[ , ths[i] ]
    
    # final check of condition, ensuring equality
    expect_true( all( func.vec == base.vec ) )
    
  }
}
)


# ensure all levels of two variables are present if more than 2 variables are called in `var1`
test_that( "`var1` vector check.", {
  
  
  # call function
  call.it <- summary_table( d = d.example,
                            metric = c( "count", "percent" ),
                            var1 = c( "v1", "v3" ),
                            var2 = "v2",
                            add.summary.row = TRUE,
                            add.summary.col = TRUE,
                            rate.supp = 5,
                            count.supp = 5,
                            percentages.rel = "var2",
                            pop.var = "v_pop" )
  
  var.tab <- call.it$frame$var1 %>%
    .[ .!= "Summary Row" ] 
  
  expect_true( all( var.tab %in% c( levels( as.factor( d.example[["v1"]] ) ),
                                    levels( as.factor( d.example[["v3"]] ) ) ) ) )
} )


# ensure all levels of two variables are present if more than 2 variables are called in `var1`
test_that( "`var1` vector check when specifying `order.rows.", {
  
  
  # call function
  call.it <- summary_table( d = d.example,
                            metric = c( "count", "percent" ),
                            var1 = c( "v1", "v3" ),
                            order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                               v3 = c( "Other Char 2", "Other Char 3" )),
                            var2 = "v2",
                            add.summary.row = TRUE,
                            add.summary.col = TRUE,
                            rate.supp = 5,
                            count.supp = 5,
                            percentages.rel = "var2",
                            pop.var = "v_pop" )
  
  var.tab <- call.it$frame$var1 %>%
    .[ .!= "Summary Row" ] 
  
  expect_true( all( var.tab %in% c( "v1", "v3",levels( as.factor( d.example[["v1"]] ) ),
                                    levels( as.factor( d.example[["v3"]] ) ) ) ) )
  
  # call function if row.variable.labels is specific
  call.it <- summary_table( d = d.example,
                            metric = c( "count", "percent" ),
                            var1 = c( "v1", "v3" ),
                            order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                               v3 = c( "Other Char 2", "Other Char 3" )),
                            var2 = "v2",
                            add.summary.row = TRUE,
                            add.summary.col = TRUE,
                            rate.supp = 5,
                            count.supp = 5,
                            percentages.rel = "var2",
                            row.variable.labels = list( v1 = "var1",
                                                        v3 = "var3"),
                            pop.var = "v_pop" )
  
  var.tab <- call.it$frame$var1 %>%
    .[ .!= "Summary Row" ] 
  
  expect_true( all( var.tab %in% c( "var1", "var3",levels( as.factor( d.example[["v1"]] ) ),
                                    levels( as.factor( d.example[["v3"]] ) ) ) ) )
  
  # same as right above but use vector instead
  call.it <- summary_table( d = d.example,
                            metric = c( "count", "percent" ),
                            var1 = c( "v1", "v3" ),
                            order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                               v3 = c( "Other Char 2", "Other Char 3" )),
                            var2 = "v2",
                            add.summary.row = TRUE,
                            add.summary.col = TRUE,
                            rate.supp = 5,
                            count.supp = 5,
                            percentages.rel = "var2",
                            row.variable.labels = c( v1 = "var1",
                                                     v3 = "var3"),
                            pop.var = "v_pop" )
  
  var.tab <- call.it$frame$var1 %>%
    .[ .!= "Summary Row" ] 
  
  expect_true( all( var.tab %in% c( "var1", "var3",levels( as.factor( d.example[["v1"]] ) ),
                                    levels( as.factor( d.example[["v3"]] ) ) ) ) )
} )


# ensure `order.rows` argument works when provided a list when length( var1 ) > 1
test_that( "`order.rows` check when length( var1 ) > 1", {
  
  
  # call function when row.variable.labels is != "none"
  call.it <- summary_table( d = d.example,
                            metric = c( "count", "percent" ),
                            var1 = c( "v1", "v3" ),
                            order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                               v3 = c( "Other Char 2", "Other Char 3" )),
                            var2 = "v2",
                            add.summary.row = TRUE,
                            add.summary.col = TRUE,
                            rate.supp = 5,
                            count.supp = 5,
                            percentages.rel = "var2",
                            pop.var = "v_pop" )
  
  var.tab <- call.it$frame$var1 %>%
    .[ .!= "Summary Row" ] 
  
  expect_equal( var.tab, c( "v1", "Geo 2", "Geo 3", "Geo 1", "v3", "Other Char 2", "Other Char 3" ) )
  
  
  # call function when row.variable.labels is == "none"
  call.it <- summary_table( d = d.example,
                            metric = c( "count", "percent" ),
                            var1 = c( "v1", "v3" ),
                            order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                               v3 = c( "Other Char 2", "Other Char 3" )),
                            var2 = "v2",
                            add.summary.row = TRUE,
                            add.summary.col = TRUE,
                            rate.supp = 5,
                            count.supp = 5,
                            row.variable.labels = "none",
                            percentages.rel = "var2",
                            pop.var = "v_pop" )
  
  var.tab <- call.it$frame$var1 %>%
    .[ .!= "Summary Row" ] 
  
  expect_equal( var.tab, c( "Geo 2", "Geo 3", "Geo 1", "Other Char 2", "Other Char 3" ) )
  
  # call function when row.variable.labels is a named list with custom names
  call.it <- summary_table( d = d.example,
                            metric = c( "count", "percent" ),
                            var1 = c( "v1", "v3" ),
                            order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                               v3 = c( "Other Char 2", "Other Char 3" )),
                            var2 = "v2",
                            add.summary.row = TRUE,
                            add.summary.col = TRUE,
                            rate.supp = 5,
                            count.supp = 5,
                            percentages.rel = "var2",
                            row.variable.labels = list( v1 = "this is var 1",
                                                        v3 = "this is var 3"),
                            pop.var = "v_pop" )
  
  var.tab <- call.it$frame$var1 %>%
    .[ .!= "Summary Row" ] %>%
    as.character()
  
  expect_equal( var.tab, c( "this is var 1", "Geo 2", "Geo 3", "Geo 1", "this is var 3", "Other Char 2", "Other Char 3" ) )
  
} )


# ensure `order.rows` argument works when provided a list when length( var1 ) > 1
test_that( "`order.rows` check when length( var1 ) > 1 when add.summary.row and add.summary.col are FALSE", {
  
  
  # call function
  call.it <- summary_table( d = d.example,
                            metric = c( "count", "percent" ),
                            var1 = c( "v1", "v3" ),
                            order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                               v3 = c( "Other Char 2", "Other Char 3" )),
                            var2 = "v2",
                            add.summary.row = FALSE,
                            add.summary.col = FALSE,
                            rate.supp = 5,
                            count.supp = 5,
                            percentages.rel = "var2",
                            pop.var = "v_pop",
                            row.variable.labels = "none" )
  
  var.tab <- call.it$frame$var1 %>%
    .[ .!= "Summary Row" ] 
  
  expect_equal( var.tab, c( "Geo 2", "Geo 3", "Geo 1", "Other Char 2", "Other Char 3" ) )
  
} )


# ensure `order.rows` argument works when provided a list when length( var1 ) > 1
test_that( "throw error when names of elements in list in `order.rows` do not match column names in `var1` when length( var1 ) > 1", {
  
  
  expect_error( summary_table( d = d.example,
                               metric = c( "count", "percent" ),
                               var1 = c( "v1", "v3" ),
                               order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                                  v2 = c( "Other Char 2", "Other Char 3" )),
                               var2 = "v2",
                               add.summary.row = FALSE,
                               add.summary.col = FALSE,
                               rate.supp = 5,
                               count.supp = 5,
                               percentages.rel = "var2",
                               pop.var = "v_pop" ) )
  
} )


# ensure `order.rows` argument works when provided a list when length( var1 ) > 1
test_that( "throw error when `row.variable.labels` is not a list or vector of strings", {
  
  
  expect_error( summary_table( d = d.example,
                               metric = c( "count", "percent" ),
                               var1 = c( "v1", "v3" ),
                               order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                                  v2 = c( "Other Char 2", "Other Char 3" )),
                               var2 = "v2",
                               add.summary.row = FALSE,
                               add.summary.col = FALSE,
                               rate.supp = 5,
                               count.supp = 5,
                               percentages.rel = "var2",
                               row.variable.labels = data.frame(),
                               pop.var = "v_pop" ) )
  
  # error when names of entries in `row.variable.labels` do not match the variables in `var1`
  expect_error( summary_table( d = d.example,
                               metric = c( "count", "percent" ),
                               var1 = c( "v1", "v3" ),
                               order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                                  v3 = c( "Other Char 2", "Other Char 3" )),
                               var2 = "v2",
                               add.summary.row = FALSE,
                               add.summary.col = FALSE,
                               rate.supp = 5,
                               count.supp = 5,
                               percentages.rel = "var2",
                               row.variable.labels = list( v1 = "variable 1",
                                                           v2 = "variable 3"),
                               pop.var = "v_pop" ) )
  
} )


test_that( "no errors with proper usage of `row.variable.labels`", {
  
  expect_warning( summary_table( d = d.example,
                                 metric = c( "count", "percent" ),
                                 var1 = c( "v1" ),
                                 order.rows = list( v1 = c( "Geo 2", "Geo 3", "Geo 1" ),
                                                    v3= c( "Other Char 2", "Other Char 3" )),
                                 var2 = "v2",
                                 add.summary.row = FALSE,
                                 add.summary.col = FALSE,
                                 rate.supp = 5,
                                 count.supp = 5,
                                 percentages.rel = "var2",
                                 row.variable.labels = list( v1 = "variable 1",
                                                             v3 = "variable 3"),
                                 pop.var = "v_pop" ) )
  
  expect_no_error( summary_table( d = d.example,
                                  metric = c( "count", "percent" ),
                                  var1 = c( "v1" ),
                                  order.rows = c( "Geo 2", "Geo 3", "Geo 1" ),
                                  var2 = "v2",
                                  add.summary.row = FALSE,
                                  add.summary.col = FALSE,
                                  rate.supp = 5,
                                  count.supp = 5,
                                  percentages.rel = "var2",
                                  row.variable.labels = list( v1 = "variable 1",
                                                              v3 = "variable 3"),
                                  pop.var = "v_pop" ) )
  
} )


test_that( "multiple variables in var1 but remove some levels (rows) of each of those variables from the final printed table", {
  
  call.it <- summary_table( d = d.example,
                          metric = c( "count", "percent" ),
                          var1 = c( "v1", "v3" ),
                          order.rows = list( v1 = c( "Geo 2", "Geo 3" ),
                                             v3 = c( "Other Char 2" )),
                          var2 = "v2",
                          add.summary.row = TRUE,
                          add.summary.col = TRUE,
                          rate.supp = 5,
                          count.supp = 5,
                          percentages.rel = "var1",
                          row.variable.labels = list( v1 = "var1",
                                                      v3 = "var3") )

var.tab <- call.it$frame$var1 %>%
  .[ .!= "Summary Row" ] 

expect_true( all( var.tab %in% c( "var1", "var3","Geo 2", "Geo 3", "Other Char 2" ) ) )
}


)


test_that( "`nm.var1` can still be specified if `length( var1 )` > 1", {
  
 expect_no_error( summary_table( d = d.example,
                          metric = c( "count", "percent" ),
                          var1 = c( "v1", "v3" ),
                          order.rows = list( v1 = c( "Geo 2", "Geo 3" ),
                                             v3 = c( "Other Char 2" )),
                          var2 = "v2",
                          add.summary.row = TRUE,
                          add.summary.col = TRUE,
                          rate.supp = 5,
                          count.supp = 5,
                          nm.var1 = "new name",
                          percentages.rel = "var1",
                          row.variable.labels = list( v1 = "var1",
                                                      v3 = "var3") ) )
})


test_that( "correct previous bug where order of `row.variable.labels` was consequential.", {
  
  expect_no_error( summary_table( d = d.example,
               metric = c( "count", "percent" ),
               var1 = c( "v1", "v3" ),
               order.rows = list( v1 = c( "Geo 2", "Geo 3" ),
                                  v3 = c( "Other Char 2" )),
               var2 = "v2",
               add.summary.row = TRUE,
               add.summary.col = TRUE,
               rate.supp = 5,
               count.supp = 5,
               nm.var1 = "new name",
               percentages.rel = "var1",
               row.variable.labels = list(  v3 = "var3",
                                            v1 = "var1" ) ) )
} )


test_that( "ensure row labels are being ordered correctly when variables in rows have the same levels.", {
  
  
  run.samelevs <- summary_table( d = d.example,
                                 metric = c( "count", "percent" ),
                                 var1 = c( "v4", "v5" ),
                                 var2 = "v2",
                                 add.summary.row = TRUE,
                                 add.summary.col = TRUE,
                                 summary.row.name = "Summary",
                                 order.rows = list( v4 = c( "Summary", "Y", "N" ),
                                                    v5 = c( "Y", "N" )),
                                 rate.supp = 5,
                                 count.supp = 5,
                                 nm.var1 = "new name",
                                 percentages.rel = "var1",
                                 row.variable.labels = list(  v4 = "var4",
                                                              v5 = "var5" ) )
  
  expect_true( run.samelevs$frame$var1[ 2 ] == "var4" & run.samelevs$frame$var1[ 5 ] == "var5" )
  
  raw.count.v4 <- table( d.example$v4, d.example$v2 )
  
  expect_true( raw.count.v4[ "Y", "Char 2" ] == run.samelevs$frame[ 3, "Char 2,count" ] )
  
  expect_true( raw.count.v4[ "N", "Char 3" ] == run.samelevs$frame[ 4, "Char 3,count" ] )
  
  raw.count.v5 <- table( d.example$v5, d.example$v2 )
  
  expect_true( raw.count.v5[ "Y", "Char 1" ] == run.samelevs$frame[ 6, "Char 1,count" ] )
})
  

test_that( "same test as prior but change position of summary row label", {
  
  
  run.samelevs <- summary_table( d = d.example,
                                 metric = c( "count", "percent" ),
                                 var1 = c( "v4", "v5" ),
                                 var2 = "v2",
                                 add.summary.row = TRUE,
                                 add.summary.col = TRUE,
                                 summary.row.name = "Summary",
                                 order.rows = list( v4 = c( "Y", "N" ),
                                                    v5 = c( "Y", "N", "Summary" )),
                                 rate.supp = 5,
                                 count.supp = 5,
                                 nm.var1 = "new name",
                                 percentages.rel = "var1",
                                 row.variable.labels = list(  v4 = "var4",
                                                              v5 = "var5" ) )
  
  expect_true( run.samelevs$frame$var1[ 1 ] == "var4" & run.samelevs$frame$var1[ 4 ] == "var5" )
  
  raw.count.v4 <- table( d.example$v4, d.example$v2 )
  
  expect_true( raw.count.v4[ "Y", "Char 2" ] == run.samelevs$frame[ 2, "Char 2,count" ] )
  
  expect_true( raw.count.v4[ "N", "Char 3" ] == run.samelevs$frame[ 3, "Char 3,count" ] )
  
  raw.count.v5 <- table( d.example$v5, d.example$v2 )
  
  expect_true( raw.count.v5[ "Y", "Char 1" ] == run.samelevs$frame[ 5, "Char 1,count" ] )
})


test_that( "ensure row labels are being ordered correctly when variables in rows have are different and .", {
  

run.difflevs <- summary_table( d = d.example,
                               metric = c( "count", "percent" ),
                               var1 = c( "v1", "v5" ),
                               order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                                  v5 = c( "Y", "N" )),
                               var2 = "v2",
                               add.summary.row = TRUE,
                               add.summary.col = TRUE,
                               summary.row.name = "Summary",
                               rate.supp = 5,
                               count.supp = 5,
                               nm.var1 = "new name",
                               percentages.rel = "var1",
                               row.variable.labels = list( v5 = "var5",
                                                            v1 = "geovar" ) )

expect_true( run.difflevs$frame$var1[ 2 ] == "geovar" & run.difflevs$frame$var1[ 6 ] == "var5" )

raw.count.v1 <- table( d.example$v1, d.example$v2 )

expect_true( raw.count.v1[ "Geo 2", "Char 2" ] == run.difflevs$frame[ 3, "Char 2,count" ] )

expect_true( raw.count.v1[ "Geo 1", "Char 3" ] == run.difflevs$frame[ 5, "Char 3,count" ] )

raw.count.v5 <- table( d.example$v5, d.example$v2 )

expect_true( raw.count.v5[ "Y", "Char 1" ] == run.difflevs$frame[ 7, "Char 1,count" ] )

})



test_that( "checks on order of spanning headers when multiple variables are specified in `var2`.", {
  
  
  run.mult.cols <- summary_table( d = d.example,
                                 metric = c( "count", "percent" ),
                                 var1 = c( "v1", "v5" ),
                                 order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                                    v5 = c( "Y", "N", "" )),
                                 var2 = c( "v2", "v3" ),
                                 add.summary.row = TRUE,
                                 add.summary.col = TRUE,
                                 summary.col.name = "All Col",
                                 summary.col.pos = "end",
                                 summary.row.name = "Summary",
                                 rate.supp = 5,
                                 count.supp = 5,
                                 nm.var1 = "new name",
                                 percentages.rel = "var1",
                                 row.variable.labels = list( v5 = "var5",
                                                             v1 = "geovar" ) )
  
  expect_true( run.mult.cols$frame$var1[ 2 ] == "geovar" & run.mult.cols$frame$var1[ 6 ] == "var5" )
  
  raw.count.v1 <- table( d.example$v1, d.example$v2 )
  
  expect_true( raw.count.v1[ "Geo 2", "Char 2" ] == run.mult.cols$frame[ 3, "Char 2,count" ] )
  
  expect_true( raw.count.v1[ "Geo 1", "Char 3" ] == run.mult.cols$frame[ 5, "Char 3,count" ] )
  
  raw.count.v5 <- table( d.example$v5, d.example$v2 )
  
  expect_true( raw.count.v5[ "Y", "Char 1" ] == run.mult.cols$frame[ 7, "Char 1,count" ] )
  
  ## check on column positions
  
  expect_true( all( str_detect( tail( names( run.mult.cols$frame ), 2 ), "All Col" ) ) )
  
  ## run again and specify "front"
  run.mult.cols.v2 <- summary_table( d = d.example,
                                  metric = c( "count", "percent" ),
                                  var1 = c( "v1", "v5" ),
                                  order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                                     v5 = c( "Y", "N", "" )),
                                  var2 = c( "v2", "v3" ),
                                  add.summary.row = TRUE,
                                  add.summary.col = TRUE,
                                  summary.col.name = "All Col",
                                  summary.col.pos = "front",
                                  summary.row.name = "Summary",
                                  rate.supp = 5,
                                  count.supp = 5,
                                  nm.var1 = "new name",
                                  percentages.rel = "var1",
                                  row.variable.labels = list( v5 = "var5",
                                                              v1 = "geovar" ) )


expect_true( all( str_detect( head( names( run.mult.cols.v2$frame[-1] ), 2 ), "All Col" ) ) )

## run again and specify integers
run.mult.cols.v3 <- summary_table( d = d.example,
                                   metric = c( "count", "percent" ),
                                   var1 = c( "v1", "v5" ),
                                   order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                                      v5 = c( "Y", "N", "" )),
                                   var2 = c( "v2", "v3" ),
                                   add.summary.row = TRUE,
                                   add.summary.col = TRUE,
                                   summary.col.name = "All Col",
                                   summary.col.pos = 3,
                                   summary.row.name = "Summary",
                                   rate.supp = 5,
                                   count.supp = 5,
                                   nm.var1 = "new name",
                                   percentages.rel = "var1",
                                   row.variable.labels = list( v5 = "var5",
                                                               v1 = "geovar" ))


expect_true( all( str_detect( names( run.mult.cols.v3$frame )[c(6,7)], "All Col" ) ) )


## check that all column values are the same in both tables despite different position of "All Column"

check.cols <- sapply( 1:ncol( run.mult.cols.v2$frame ), function( i ){
  
  col.to.check <- names( run.mult.cols.v2$frame )[i]
  
  all( run.mult.cols.v2$frame[, col.to.check ] ==  run.mult.cols.v3$frame[, col.to.check ],
       na.rm = TRUE )
} )

expect_true( all( check.cols ) )

})


test_that( "expect error when summary.col.pos < 0.", {
  
## run again and specify integers
expect_error( summary_table( d = d.example,
                                   metric = c( "count", "percent" ),
                                   var1 = c( "v1", "v5" ),
                                   order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                                      v5 = c( "Y", "N", "" )),
                                   var2 = c( "v2", "v3" ),
                                   add.summary.row = TRUE,
                                   add.summary.col = TRUE,
                                   summary.col.name = "All Col",
                                   summary.col.pos = -1,
                                   summary.row.name = "Summary",
                                   rate.supp = 5,
                                   count.supp = 5,
                                   nm.var1 = "new name",
                                   percentages.rel = "var1",
                                   row.variable.labels = list( v5 = "var5",
                                                               v1 = "geovar" ) ) )
  
  expect_error( summary_table( d = d.example,
                               metric = c( "count", "percent" ),
                               var1 = c( "v1", "v5" ),
                               order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                                  v5 = c( "Y", "N", "" )),
                               var2 = c( "v2", "v3" ),
                               add.summary.row = TRUE,
                               add.summary.col = TRUE,
                               summary.col.name = "All Col",
                               summary.col.pos = "notback",
                               summary.row.name = "Summary",
                               rate.supp = 5,
                               count.supp = 5,
                               nm.var1 = "new name",
                               percentages.rel = "var1",
                               row.variable.labels = list( v5 = "var5",
                                                           v1 = "geovar" ) ) )


})


test_that( "ensure second layer of spanning headers works.", {
  
span.head.2.a <- summary_table( d = d.example,
               metric = c( "count", "percent" ),
               var1 = c( "v1", "v5" ),
               order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                  v5 = c( "Y", "N", "" )),
               var2 = c( "v2", "v3" ),
               add.summary.row = TRUE,
               add.summary.col = TRUE,
               summary.col.name = "All Col",
               summary.col.pos = "front",
               summary.row.name = "Summary",
               rate.supp = 5,
               count.supp = 5,
               nm.var1 = "new name",
               percentages.rel = "var1",
               row.variable.labels = list( v5 = "var5",
                                           v1 = "geovar" ),
               col.variable.labels = list( v3 = "var3",
                                           v2 = "var2" ) )

expect_true( all( str_detect( span.head.2.a$flextable$header$dataset[ 1, 4:9 ], "var2" ) ) )

expect_true( all( str_detect( span.head.2.a$flextable$header$dataset[ 1, 10:15 ], "var3" ) ) )

# change `summary.col.pos` to "end"
span.head.2.b <- summary_table( d = d.example,
                                metric = c( "count", "percent" ),
                                var1 = c( "v1", "v5" ),
                                order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                                   v5 = c( "Y", "N", "" )),
                                var2 = c( "v2", "v3" ),
                                add.summary.row = TRUE,
                                add.summary.col = TRUE,
                                summary.col.name = "All Col",
                                summary.col.pos = "end",
                                summary.row.name = "Summary",
                                rate.supp = 5,
                                count.supp = 5,
                                nm.var1 = "new name",
                                percentages.rel = "var1",
                                row.variable.labels = list( v5 = "var5",
                                                            v1 = "geovar" ),
                                col.variable.labels = list( v3 = "var3",
                                                            v2 = "var2" ) )

expect_true( all( str_detect( names( span.head.2.b$flextable$header$dataset[ 1, 14:15 ] ), "All\\sCol" ) ) )

expect_true( all( str_detect( span.head.2.b$flextable$header$dataset[ 1, 8:13 ], "var3" ) ) )

expect_true( all( str_detect( span.head.2.b$flextable$header$dataset[ 1, 2:7 ], "var2" ) ) )


# change `col.variable.labels` to "none"
span.head.2.c <- summary_table( d = d.example,
                                metric = c( "count", "percent" ),
                                var1 = c( "v1", "v5" ),
                                order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                                   v5 = c( "Y", "N", "" )),
                                var2 = c( "v2", "v3" ),
                                add.summary.row = TRUE,
                                add.summary.col = TRUE,
                                summary.col.name = "All Col",
                                summary.col.pos = "end",
                                summary.row.name = "Summary",
                                rate.supp = 5,
                                count.supp = 5,
                                nm.var1 = "new name",
                                percentages.rel = "var1",
                                row.variable.labels = list( v5 = "var5",
                                                            v1 = "geovar" ),
                                col.variable.labels = "none" )

expect_true( all( str_detect( span.head.2.c$flextable$header$dataset[ 1, 14:15 ], "All\\sCol" ) ) )

expect_true( all( str_detect( span.head.2.c$flextable$header$dataset[ 1, 8:13 ], "Other Char\\s" ) ) )

expect_true( all( str_detect( span.head.2.c$flextable$header$dataset[ 1, 2:7 ], "^Char\\s" ) ) )


# change order of entries in col.variable.labels
span.head.2.d <- summary_table( d = d.example,
                                metric = c( "count", "percent" ),
                                var1 = c( "v1", "v5" ),
                                order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                                   v5 = c( "Y", "N", "" )),
                                var2 = c( "v2", "v3" ),
                                add.summary.row = TRUE,
                                add.summary.col = TRUE,
                                summary.col.name = "All Col",
                                summary.col.pos = "end",
                                summary.row.name = "Summary",
                                rate.supp = 5,
                                count.supp = 5,
                                nm.var1 = "new name",
                                percentages.rel = "var1",
                                row.variable.labels = list( v5 = "var5",
                                                            v1 = "geovar" ),
                                col.variable.labels = list( v2 = "var2",
                                                            v3 = "var3" ) )

expect_true( all( str_detect( names( span.head.2.d$flextable$header$dataset[ 1, 14:15 ] ), "All\\sCol" ) ) )

expect_true( all( str_detect( span.head.2.d$flextable$header$dataset[ 1, 8:13 ], "var3" ) ) )

expect_true( all( str_detect( span.head.2.d$flextable$header$dataset[ 1, 2:7 ], "var2" ) ) )


})
            


test_that( "spanning header suppression when ", {
  
  
  expect_warning( summary_table( d = d.example,
                                 metric = c( "count", "percent" ),
                                 var1 = c( "v1", "v5" ),
                                 order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                                    v5 = c( "Y", "N", "" )),
                                 var2 = c( "v2", "v3" ),
                                 add.summary.row = TRUE,
                                 add.summary.col = TRUE,
                                 summary.col.name = "All Col",
                                 summary.col.pos = 4,
                                 summary.row.name = "Summary",
                                 rate.supp = 5,
                                 count.supp = 5,
                                 nm.var1 = "new name",
                                 percentages.rel = "var1",
                                 row.variable.labels = list( v5 = "var5",
                                                             v1 = "geovar" ),
                                 col.variable.labels = list( v2 = "var2",
                                                             v3 = "var3" ) ) )
  
  # check that summary.col.pos %in% 0,1 works fine
  d.0 <- summary_table( d = d.example,
                 metric = c( "count", "percent" ),
                 var1 = c( "v1", "v5" ),
                 order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                    v5 = c( "Y", "N", "" )),
                 var2 = c( "v2", "v3" ),
                 add.summary.row = TRUE,
                 add.summary.col = TRUE,
                 summary.col.name = "All Col",
                 summary.col.pos = 0,
                 summary.row.name = "Summary",
                 rate.supp = 5,
                 count.supp = 5,
                 nm.var1 = "new name",
                 percentages.rel = "var1",
                 row.variable.labels = list( v5 = "var5",
                                             v1 = "geovar" ),
                 col.variable.labels = list( v2 = "var2",
                                             v3 = "var3" ) )
  
  d.1 <- summary_table( d = d.example,
                        metric = c( "count", "percent" ),
                        var1 = c( "v1", "v5" ),
                        order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                           v5 = c( "Y", "N", "" )),
                        var2 = c( "v2", "v3" ),
                        add.summary.row = TRUE,
                        add.summary.col = TRUE,
                        summary.col.name = "All Col",
                        summary.col.pos = 1,
                        summary.row.name = "Summary",
                        rate.supp = 5,
                        count.supp = 5,
                        nm.var1 = "new name",
                        percentages.rel = "var1",
                        row.variable.labels = list( v5 = "var5",
                                                    v1 = "geovar" ),
                        col.variable.labels = list( v2 = "var2",
                                                    v3 = "var3" ) )
  
  d.front <- summary_table( d = d.example,
                        metric = c( "count", "percent" ),
                        var1 = c( "v1", "v5" ),
                        order.rows = list( v1 = c( "Summary", "Geo 2", "Geo 3", "Geo 1" ),
                                           v5 = c( "Y", "N", "" )),
                        var2 = c( "v2", "v3" ),
                        add.summary.row = TRUE,
                        add.summary.col = TRUE,
                        summary.col.name = "All Col",
                        summary.col.pos = "front",
                        summary.row.name = "Summary",
                        rate.supp = 5,
                        count.supp = 5,
                        nm.var1 = "new name",
                        percentages.rel = "var1",
                        row.variable.labels = list( v5 = "var5",
                                                    v1 = "geovar" ),
                        col.variable.labels = list( v2 = "var2",
                                                    v3 = "var3" ) )
  
  
  expect_true( all.equal( d.1, d.front ) )
  
  expect_true( all.equal( d.0, d.front ) )
  
})


test_that( "test that var1 length 1 and var2 length > 1 produces desired result", {
  

  short.var1 <- summary_table( d = d.example,
                          metric = c( "count", "percent" ),
                          var1 = c( "v1"),
                          order.rows = c("Summary", "Geo 2", "Geo 3", "Geo 1" ),
                          var2 = c( "v2", "v3" ),
                          add.summary.row = TRUE,
                          add.summary.col = TRUE,
                          summary.col.name = "All Col",
                          summary.col.pos = "front",
                          summary.row.name = "Summary",
                          rate.supp = 5,
                          count.supp = 5,
                          nm.var1 = "new name",
                          percentages.rel = "var1",
                          row.variable.labels = list( v1 = "geovar" ),
                          col.variable.labels = list( v2 = "var2",
                                                      v3 = "var3" ) )
  
  
  expect_true( nrow( short.var1$frame ) == 4 )
  
  expect_true( short.var1$frame$var1[ 3 ] == "Geo 3" )
  
  expect_true( short.var1$frame$var1[ 3 ] == "Geo 3" )
  
 expect_true( short.var1$frame %>%
    filter( var1 == "Geo 2" ) %>% 
    pull( `Char 2,count`) == table( d.example$v1, d.example$v2 )[ "Geo 2", "Char 2" ] )
 
 expect_true( short.var1$frame[4, 14 ]  == table( d.example$v1, d.example$v3 )[ "Geo 1", "Other Char 3" ] )
 
})    
                  
# next do example where v1 only has 1 level  

# test_that( "test that var1 length 1 and var2 length > 1 produces desired result when var1 only has 1 level", {
#   
#   
#   short.var1 <- summary_table( d = d.example %>%
#                                  filter( v3 == "Other Char 1" ),
#                                metric = c( "count", "percent" ),
#                                var1 = c( "v1"),
#                                order.rows = c("Summary", "Geo 2", "Geo 3", "Geo 1" ),
#                                var2 = c( "v2", "v3" ),
#                                add.summary.row = FALSE,
#                                add.summary.col = TRUE,
#                                summary.col.name = "All Col",
#                                summary.col.pos = "front",
#                                summary.row.name = "Summary",
#                                rate.supp = 5,
#                                count.supp = 5,
#                                nm.var1 = "new name",
#                                percentages.rel = "var1",
#                                row.variable.labels = list( v1 = "geovar" ),
#                                col.variable.labels = list( v2 = "var2",
#                                                            v3 = "var3" ) )
#   
#   
#   expect_true( nrow( short.var1$frame ) == 4 )
#   
#   expect_true( short.var1$frame$var1[ 3 ] == "Geo 3" )
#   
#   expect_true( short.var1$frame$var1[ 3 ] == "Geo 3" )
#   
#   expect_true( short.var1$frame %>%
#                  filter( var1 == "Geo 2" ) %>% 
#                  pull( `Char 2,count`) == table( d.example$v1, d.example$v2 )[ "Geo 2", "Char 2" ] )
#   
#   expect_true( short.var1$frame[4, 14 ]  == table( d.example$v1, d.example$v3 )[ "Geo 1", "Other Char 3" ] )
#   
# })    