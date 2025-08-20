
#######################
### Utils Functions ###
#######################


## Function for pretty numbers (comma at thousands place) ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

pretty.num <- function(x, round.to = NULL ){
  # dependencies: stringr
  
  if( !is.null( round.to ) ) x.rounded <- sprintf( "%0.1f", round( x, digits = round.to ) ) else x.rounded <- x
  
  p <- prettyNum( x.rounded, big.mark = ",", scientific = FALSE )
  
  p.out <- str_remove( p, "\\s+" ) # remove all lingering white space
  
  return( p.out )
  
}

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## Function for Adding New Lines to ggplot Axis Labels or just adding some line breaks ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

add_new_lines <- function( n, dat ){
  
  # This function adds new line operators ("\n") after every "n" words in a string
  
  # Inputs:
  # n = an integer. this is the number used to count the number of words after which we add "\n"
  # dat = a vector of strings to be manipulated
  
  # Output:
  # this function returns a vector of strings (same as `dat` in input) with the "\n" added after
  # each `n` words.
  
  max.no <- max( str_count( dat, "\\b(\\s|$)" ) )
  
  no.its <- ceiling( max.no / n )
  
  cuts.n <- vector()
  
  for( i in 1:no.its ){
    
    w.in <- n * i
    dat <- str_replace_all( dat, 
                            paste0( "(", 
                                    paste0( rep( "(\\w+|[:punct:])\\s", (w.in-1) ), collapse = "" ),
                                    "(\\w+|\\w+[:punct:]))\\s") , 
                            "\\1\n" ) 
    
  }
  
  return( dat )
  
}

# add_new_lines( dat = data_final_T36nogrp$cat, n = 3 )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------



## Function for Capitalizing first Letter in String ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# not mind: https://stackoverflow.com/a/18509816
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# ---------------------------------------------------------------------------------------------------------------------------------------------------------

##  Generate NA Color/Text for Legend ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

generate_NA_legend <- function( na.text, color, text.size = 10,
                                family = "Calibri Light" ){
  # na.text = a string that will show as the value for the NA's in the plot
  # color = a ggplot2 color (a string)
  # dependencies: ggpubr, dplyr, ggplot2, cowplot
  
  # generate a plot and extract the legend 
  legend.plot <- data.frame( main = c( rnorm(20), NA ) ) %>%
    mutate( main.two = ifelse( !is.na( main ), NA, na.text ),
            y = c( rnorm(20), NA ) ) %>%
    filter( is.na( main ) ) %>%
    
    ggplot(mapping = aes( x = main, y = y, color = main.two ) ) +
    geom_point( shape = 15, size = 5 ) +
    scale_color_manual( values = c( color ) ) +
    theme_classic() +
    theme(
      legend.text = element_text( family = family, size = text.size),
      legend.title = element_blank(),
      legend.key = element_rect( colour = "black",
                                 fill = color ),
      legend.background = element_rect( fill = "transparent",
                                        color = "transparent")
    ) 
  
  # extract the legend
  leg.p <- get_legend( legend.plot ) %>%
    ggpubr::as_ggplot(.) +
    theme( plot.background = element_rect( fill = "transparent",
                                           color = "transparent") )
  
  return( leg.p )
}

# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## Function For Determining Percentage and Euclidian Distance ##
##            Between Two Colors (Hex codes)                  ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

colorDist <- function( hex.1, hex.2 ){
  
  # hex.1 = A string. Hexadecimal code for the first color.
  # hex.2 = A string. Hexadecimal code for the second color.
  
  # Credit/Source: https://stackoverflow.com/a/9018153
  
  col.1 <- setNames( as.vector( grDevices::col2rgb( hex.1 ) ),
                     str_extract( rownames( grDevices::col2rgb( hex.1 ) ), "^\\w" ) )
  
  col.2 <- setNames( as.vector( grDevices::col2rgb( hex.2 ) ),
                     str_extract( rownames( grDevices::col2rgb( hex.2 ) ), "^\\w" ) )
  
  
  # Euclidian distance
  eq.d <- sqrt( ( col.2["r"] - col.1["r"] )^2 + ( col.2["g"] - col.1["g"] )^2 + ( col.2["b"]  - col.1["b"] )^2 )
  
  # percentage difference
  p.d <- eq.d / sqrt( (255)^2 + (255)^2 + (255)^2 )
  
  return( data.frame( eq.d = unname( eq.d ),
                      p.d = unname( p.d ) ) )
}

# example:
# if( colorDist( "black", "#7a1315")$p.d < 30 ) "grey" else "black"

# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## Function For Determining Percentage and Euclidian Distance ##
##            Between Two Colors (Hex codes)                  ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

colorDist <- function( hex.1, hex.2 ){
  
  # hex.1 = A string. Hexadecimal code for the first color.
  # hex.2 = A string. Hexadecimal code for the second color.
  
  # Credit/Source: https://stackoverflow.com/a/9018153
  
  col.1 <- setNames( as.vector( grDevices::col2rgb( hex.1 ) ),
                     str_extract( rownames( grDevices::col2rgb( hex.1 ) ), "^\\w" ) )
  
  col.2 <- setNames( as.vector( grDevices::col2rgb( hex.2 ) ),
                     str_extract( rownames( grDevices::col2rgb( hex.2 ) ), "^\\w" ) )
  
  
  # Euclidian distance
  eq.d <- sqrt( ( col.2["r"] - col.1["r"] )^2 + ( col.2["g"] - col.1["g"] )^2 + ( col.2["b"]  - col.1["b"] )^2 )
  
  # percentage difference
  p.d <- eq.d / sqrt( (255)^2 + (255)^2 + (255)^2 )
  
  return( data.frame( eq.d = unname( eq.d ),
                      p.d = unname( p.d ) ) )
}

# example:
# if( colorDist( "black", "#7a1315")$p.d < 30 ) "grey" else "black"

# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## Deparsing Function Arguments ##
deparse_arg <- function(x) parse_expr(x) %>% as.character()

