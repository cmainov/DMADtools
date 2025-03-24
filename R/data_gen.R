###---------------------------------------------------------------
###  Example Data Generation
###---------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we generate some fake example data for use in the examples
# of the different functions in this package.
# 
# --------------------------------------------------------------------------------------------------------------------------------------------------------

## (1.0) Generate Conditional Categorical Data ##
# --------------------------------------------------------------------------------------------------------------------------------------------------------

set.seed( 425 ) # set seed for reproducibility given stochastic nature of sampling

d.example <- data.frame()

for( i in 1:500 ){
  
  # variable 1
  v1 <- sample( x = c( 1:3 ), size = 1,
                replace = TRUE,
                prob = c( 0.33, 0.5, 0.17 ) )
  
  v1.label <- c( "Geo 1", "Geo 2", "Geo 3" )[ v1 ]
  
  # variable 2
  v2 <- sample( x = c( 1:4 ), size = 1,
                replace = TRUE,
                # assign conditional probabilities based on value of `v1`
                prob = { if( v1 == 1 ) c( 0.2, 0.6, 0.1, 0.1 )
                  else if( v1 == 2 ) c( 0.3, 0.4, 0.2, 0.1 ) 
                  else if( v1 == 3 ) c( 0.5, 0.2, 0.2, 0.1 ) } )
  
  v2.label <- c( "Char 1", "Char 2", "Char 3", "Char 3" )[ v2 ]
  
  # variable 3
  v3 <- sample( x = c( 1:2 ), size = 1,
                replace = TRUE,
                prob = c( 0.52, 0.48 ) )
  
  v3.label <- c( "Other Char 1", "Other Char 2", "Other Char 3", "Other Char 3" )[ v2 ]
  
  # assign to `data.frame` and bind
  d.example <- rbind( d.example,
                      data.frame( v1 = v1.label,
                                  v2 = v2.label,
                                  v3 = v3.label ) )
  
  
}

# --------------------------------------------------------------------------------------------------------------------------------------------------------


## (2.0)  "Population" Data ##
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# `summary_table` can accept an aggregate variable with the population count for rate calculations.


# assign population data for computing rates (aggregate variable)
set.seed( 5789 )

pop.1 <- sample( 50000:60000, size = 1 )
pop.2 <- sample( 350000:360000, size = 1 )
pop.3 <- sample( 70000:860000, size = 1 )

d.example$v_pop <- ifelse( d.example$v1 == "Geo 1", pop.1,
                           ifelse( d.example$v1 == "Geo 2", pop.2,
                                   ifelse( d.example$v1 == "Geo 3", pop.3,
                                        NA ) ) )

# --------------------------------------------------------------------------------------------------------------------------------------------------------


## (3.0) Missing Data ##
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# to test `NAs.footnote` in `summary_table`

# generate a separate dataset with missing data
d.example.na <- d.example

d.example.na[ sample( 1:nrow( d.example.na ), size = 9 ), "v2" ] <- NA # introduce 9 NAs randomly 

# --------------------------------------------------------------------------------------------------------------------------------------------------------


## (4.0) Save example data for export ##
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# usethis::use_data( d.example, d.example.na,
#                    overwrite = TRUE ) # this creates the `/data` folder and stores the individual datasets in that directory
# 
# --------------------------------------------------------------------------------------------------------------------------------------------------------
