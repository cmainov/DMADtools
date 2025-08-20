
test_that( "basic no errors call", {
  extrafont::loadfonts(device = "win")
  
  expect_no_error( dc_mapr(
    d = d.ward,
    var = "bin_other",
    id = "ward",
    geo = "ward 2022",
    bypass = FALSE,
    metric = "percent", pop.var = NULL, 
    per = 1000, colorbar.bins = 5, colorbar.round = 1,
    colorbar.high = "#7a1315"
  ) # title
  )
} )

test_that( "create a missing value for ward 8", {
  extrafont::loadfonts(device = "win")
  
  expect_no_error( dc_mapr(
    d = d.ward %>% 
      filter( ward != "Ward 8" ),
    var = "bin_other",
    id = "ward",
    geo = "ward 2022",
    bypass = FALSE,
    metric = "percent", pop.var = NULL, 
    per = 1000, colorbar.bins = 5, colorbar.round = 1,
    colorbar.high = "#7a1315"
  ) # title
  )
} )

test_that( "count map", {
  extrafont::loadfonts(device = "win")
  
  expect_no_error( dc_mapr(
    d = d.ward,
    var = "bin_other",
    id = "ward",
    metric = "count",
    geo = "ward 2022",
    colorbar.round = 1
  ) 
  )
  
  
} )


test_that( "suppression and missing values", {
  extrafont::loadfonts(device = "win")
  
  
d.new <- d.ward %>% 
  filter( ward != "Ward 8" ) %>% 
  mutate( bin_other = ifelse( ward == "Ward 5" & id != 1, 0,
                              ifelse( id == 1, 1, bin_other )))

expect_no_error( dc_mapr(
  d = d.new,
  var = "bin_other",
  id = "ward",
  metric = "count",
  geo = "ward 2022",
  colorbar.round = 1,
  count.supp = 5,
  font.family = "sans",
  colorbar.name = "Count",
) )

})






# figure out suppressed rates and counts situation
# test count situation
