
test_that( "basic no errors call", {
  expect_no_error( dc_mapr(
    d = d.ward,
    var = "bin_other",
    id = "ward",
    geo = "ward 2022",
    bypass = FALSE,
    metric = "percent", pop.var = NULL, 
    per = 1000, colorbar.bins = 5, colorbar.round = 1,
    colorbar.high = "#7a1315", 
    font.family = "Calibri Light"
  ) # title
  )
} )

test_that( "create a missing value for ward 8", {
  expect_no_error( dc_mapr(
    d = d.ward %>% 
      filter( ward != "Ward 8" ),
    var = "bin_other",
    id = "ward",
    geo = "ward 2022",
    bypass = FALSE,
    metric = "percent", pop.var = NULL, 
    per = 1000, colorbar.bins = 5, colorbar.round = 1,
    colorbar.high = "#7a1315", 
    font.family = "Calibri Light"
  ) # title
  )
} )