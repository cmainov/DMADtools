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


