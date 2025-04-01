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




