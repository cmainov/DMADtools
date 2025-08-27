# install.packages("hexSticker")

library( hexSticker )
library( dplyr )
library( stringr )
library( sf )
library( ggplot2 )
library( conflicted )

## Read-in Population Files for Public Shapefiles ##
local.path <- "C:/Users/mainoc/OneDrive - Government of The District of Columbia/Documents/GitHub/cppe-dmad"

# get DC shapefiles with ward
dc.poly.22 <- readRDS( file.path( local.path,
                                  "programs/dc-shapefiles/dc-polygons-2022.rds" ) )

# can also be obtained with the `tigris` package using:
dc.ward.2022 <- ggplot(data = dc.poly.22 %>%
                         arrange(  NAMELSAD ) %>%
                         st_transform( crs = 4269 ) ) + 
  geom_sf(  fill = "antiquewhite1" ) +
 
  theme_classic() +
  theme( axis.line = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         legend.title = element_blank(),
         title = element_text( face = "italic"),
         legend.background = element_rect( fill = 'transparent' ),
         panel.background = element_rect( fill = 'transparent' ),
         plot.background = element_rect( fill = 'transparent' )) +
  guides( fill = guide_colorbar( ticks.colour = NA,
                                 frame.colour =  "black",
                                 barwidth = 10,
                                 barheight = 0.6,
                                 nbin = 10,
                                 direction = "horizontal",
                                 label.position = "bottom" ) ) 


conflict_prefer("theme_transparent", "hexSticker")

sticker.out <- "man/figures"
sticker( dc.ward.2022+theme_transparent(), 
         package = "DMADtools",
         p_size = 20, s_x = 1.05, 
         s_y = .8, 
         s_width = 1.8,
         s_height=1.2,
         h_fill="#8e3e2f", 
         h_color="#002b3a", 
         white_around_sticker = FALSE,
         filename = file.path( sticker.out, "dmadtoolslogo.png" ) )
         