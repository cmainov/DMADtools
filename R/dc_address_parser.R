library( stringr )
library( dplyr )





dc_address_parser <- function( address ){
  
  ## Step 1: Address Parsing
  
  street_types <- c( "AVE","AVENUE","ST","STREET","RD","ROAD","LN","LANE",
                     "DR","DRIVE","CT","COURT","PKWY","PKY","PARKWAY","TERR",
                     "TERRACE","PL","PLACE","CIR","CIRCLE","BLVD","BOULEVARD",
                     "SQ","SQUARE","WY","WAY","ALY","ALLY","ALLEY" )
  
  quad_types <- c( "NW","NE","SE","SW",
                   "NORTHWEST","NORTH WEST","NORTHEAST","NORTH EAST",
                   "SOUTHWEST","SOUTH WEST","SOUTHEAST","SOUTH EAST" )
  
  address1 = toupper( address )  # upper case all for standardization
  address2 = ifelse( !str_detect( address1, "^\\d+" ) & 
                       str_detect( address1, "^[:alpha:]+\\s+\\d+" ) &
                       address != "", str_remove( address1, "^[:alpha:]+\\s+(?=\\d+)" ),
                     address1 )  # if there is non-numeric text at the start of the address string that is followed by numbers, remove the letters (usually for the name of a place that precedes the address number)
  
  address3 = str_replace_all( address2, "\\s+", " " )  # ensure no multiple empty spaces, replace with one space
  address4 = str_replace( address3, "(\\d+)+\\s+(TH|RD)", "\\1\\2" )  # any spaces between numbered streets and their suffix are removed
  address5 = ifelse( str_detect( address4, "^\\d+[:alpha:]"),
                     str_replace( address4, "(^\\d+)([:alpha:])", "\\1 \\2" ),
                     address4 ) # add a space between street no. and st name if one is not detected
  
  aptnum <- str_extract( address5, "(?<=\\b[NS][EW]\\b\\s).*" )
  
  zipcode <- str_trim( str_extract( aptnum, "(?<!\\sAPT|\\sUNIT|\\s\\#|\\sNO\\.|\\sNUMBER|SUITE)\\d{5}$" ) ) # assumption is that any any 5 digit value at the end of the string not preceded by an apartment or unit keyword is a ZIP code
  
  aptnum1 <- str_trim( str_remove( aptnum, zipcode ) )
  
  if( !is.na( zipcode ) ){
    if( any( !str_detect( zipcode, "^2" ) ) ) warning( "Non-DC ZIP code detected. Double check address input." ) # warning for non-DC ZIP code detection
  }
  
  
  address6 = str_remove( address5, "(?<=\\b[NS][EW]\\b\\s).*" ) # remove apt/unit, first pass
  
  aptnum2 = str_trim( ifelse( is.na(aptnum1), str_extract( address6, "\\b(?:\\sAPT|\\sUNIT|\\s\\#|\\sNO\\.|\\sNUMBER|SUITE)\\b\\s?.*"),
                              aptnum1 ) ) # second pass for detecting apt/uni
  
  address7 = str_remove( address6, "\\b(?:\\sAPT|\\sUNIT|\\s\\#|\\sNO\\.|\\sNUMBER|SUITE)\\b\\s?.*" ) # remove apt/unit, second pass
  address8 = str_remove( address7, "(?<=\\b[NS][EW]\\b\\s).*" )
  address9 = str_remove( address8, "^\\s+|\\s+$" ) # remove leading or trailing whitespaces
  address10 = str_remove_all( address9, "[[:punct:]&&[^/'']]" ) # remove unnecessary punctuation from address string but keep forward slashes
  address11 = str_replace_all( address10, "\\s+", " " ) # remove unnecessary white space
  address12 = str_replace( address11, "(N)\\s+(W)$|(N)\\s+(E)$|(S)\\s+(W)$|(S)\\s+(E)$", 
                           "\\1\\2\\3\\4\\5\\6\\7\\8" ) # rare situation where quadrant letters are spaced apart (e.g., N W)
  
  # Step 1a: Word Extraction
  new1 = word( address12, 1, sep = fixed( " " ) ) # extract address components
  new2 = word( address12, 2, sep = fixed( " " ) )
  new3 = word( address12, 3, sep = fixed( " " ) )
  new4 = word( address12, 4, sep = fixed( " " ) )
  new5 = word( address12, 5, sep = fixed( " " ) )
  new6 = word( address12, 6, sep = fixed( " " ) )
  new7 = word( address12, 7, sep = fixed( " " ) )
  # clean up address components
  new4 = case_when(
    new4 %in% c( "NORTHWEST", "NORTH WEST" ) ~ "NW",
    new4 %in% c( "NORTHEAST", "NORTH EAST" ) ~ "NE",
    new4 %in% c( "SOUTHWEST", "SOUTH WEST" ) ~ "SW",
    new4 %in% c( "SOUTHEAST", "SOUTH EAST" ) ~ "SE",
    new4 == "NORTH" & new5 == "WEST" ~ "NW",
    new4 == "WASHINGTON" ~ "",
    TRUE ~ new4
  )
  
  # Step 1b: Address Component Generation
  addrnum = new1
  stname = case_when( !is.na( new7 ) ~ paste( new2, new3, new4, new5 ), # 4-word street names
                      !is.na( new6 ) ~ paste( new2, new3, new4 ), # 3-word street names
                      !new4 %in% c( 'NW','NE','SE','SW' ) & !is.na( new4 ) ~
                        paste( new2, new3 ), # 2-word street names
                      TRUE ~ new2 ) # all other 1-word street names
  
  street_type = case_when( !is.na( new7 ) ~ case_when( !new6 %in% c( 'NW','NE','SE','SW' ) ~
                                                         new6 ),
                           !is.na( new6 ) ~ case_when( !new5 %in% c( 'NW','NE','SE','SW' ) ~
                                                         new5 ),
                           !new4 %in% c( 'NW','NE','SE','SW' ) & !is.na( new4 ) ~
                             new4,
                           TRUE ~ new3 )
  
  # first pass clean-up: common abbreviations
  street_type2 = case_when( street_type == "AVE" ~ "AVENUE",
                            street_type == "ST" ~ "STREET",
                            street_type == "RD" ~ "ROAD",
                            street_type == "LN" ~ "LANE",
                            street_type == "DR" ~ "DRIVE",
                            street_type == "CT" ~ "COURT",
                            street_type %in% c( "PKWY", "PKY" ) ~ "PARKWAY",
                            street_type == "TERR" ~ "TERRACE",
                            street_type == "PL" ~ "PLACE",
                            street_type == "CIR" ~ "CIRCLE",
                            street_type == "BLVD" ~ "BOULEVARD",
                            street_type == "SQ" ~ "SQUARE",
                            street_type == "WY" ~ "WAY",
                            street_type %in% c( "ALY", "ALLY" ) ~ "ALLEY",
                            TRUE ~ street_type )
  
  quadrant = case_when( !is.na( new7 ) ~ case_when( !new6 %in% c( 'NW','NE','SE','SW' ) ~
                                                      new7,
                                                    new6 %in% c( 'NW','NE','SE','SW' ) ~
                                                      new6 ),
                        !is.na( new6 ) ~ case_when( !new5 %in% c( 'NW','NE','SE','SW' ) ~
                                                      new6,
                                                    new5 %in% c( 'NW','NE','SE','SW' ) ~
                                                      new5 ),
                        !new4 %in% c( 'NW','NE','SE','SW' ) & !is.na( new4 ) ~
                          new5,
                        TRUE ~ new4 )
  
  # Step 1c: Edge Cases
  edge1 = ifelse( !quadrant %in% c( 'NW','NE','SE','SW' ) &
                    !street_type2 %in% c("AVENUE", "STREET", "ROAD", "LANE", "DRIVE",
                                         "COURT", "PARKWAY", "TERRACE", "PLACE",
                                         "CIRCLE", "BOULEVARD", "SQUARE", "WAY",
                                         "ALLEY" ), 1, 0 )
  stname = ifelse( edge1 == 1, paste( stname, street_type2 ), stname )
  street_type2 = ifelse( edge1 == 1, quadrant, street_type2 )
  quadrant = ifelse( edge1 == 1, "", quadrant )
  
  cleanNA <- setNames( lapply( list( addrnum, stname, street_type, street_type2, quadrant, aptnum2, zipcode ),
                               function(x) str_trim( str_remove( ifelse( is.na(x), "", x ), "\\sNA$" ) ) ), # if NA, set to "" and remove any trailing NAs
                       c( "addrnum", "stname", "street_type", "street_type2", "quadrant", "aptnum", "zipcode" ) )
  
  # commonly misspelled DC street names that won't be picked up by geocoder
  cleanNA[["stname"]] <- ifelse( cleanNA[["stname"]] == "CHANCELLORS", "CHANCELLOR'S", cleanNA[["stname"]] )
  
  # finally, compile the cleaned address for DC residents only
  # Step 1d: Generate Final, Clean Address Used for Geocoding
  address3 = paste( cleanNA[["addrnum"]] , cleanNA[["stname"]] , cleanNA[["street_type2"]], cleanNA[["quadrant"]] )
  # address remains the unmodified address string
  
  address4 = str_replace_all( address3, "^\\s+|\\s+$", "" )
  
  # final build/clean
  address_build = str_replace_all( address4, paste0( "(\\d+)\\s*", # street number followed by 0 or more ws
                                                     "(.+?)\\s*",  # street name: 1 or more characters followed by 0 or more ws
                                                     "((", paste0( street_types, collapse = "|"), "))\\b", # street type (one of any in `street_type`)
                                                     "((", paste0( quad_types, collapse = "|"), "))",        # quadrant type (one of any in `quad_type`)
                                                     "((APT|UNIT|\\#|NO\\.|NUMBER|SUITE)*)", # 0 or more of APT or UNIT labels
                                                     "(\\d*)",      # 0 or more of APT or UNIT number
                                                     "(200\\d+)*",    # ZIP code, if present
                                                     "(US|USA|U\\.S\\.|U\\.S\\A\\.)*" ), # USA, if present
                                   "\\1 \\2 \\3 \\5 \\7 \\9" ) # the numbering is dependent on nested parentheses in the regex as well, e.g., "(())" counts as two sets of parentheses/groups
  
  
  address_clean = str_replace_all( address_build, "\\s\\s+", " " ) # remove leading and trailing white space
  
  # return object below
  return( as.data.frame( list( INPUT = address, address_clean = address_clean, cleanNA ) ) %>% 
            rename_all( toupper ) )
}



dc_address_parser_v2 <- function( address ){
  
  ## Step 1: Address Parsing
  
  street_types <- c( "AVE","AVENUE","ST","STREET","RD","ROAD","LN","LANE",
                     "DR","DRIVE","CT","COURT","PKWY","PKY","PARKWAY","TERR",
                     "TERRACE","PL","PLACE","CIR","CIRCLE","BLVD","BOULEVARD",
                     "SQ","SQUARE","WY","WAY","ALY","ALLY","ALLEY" )
  
  quad_types <- c( "NW","NE","SE","SW",
                   "NORTHWEST","NORTH WEST","NORTHEAST","NORTH EAST",
                   "SOUTHWEST","SOUTH WEST","SOUTHEAST","SOUTH EAST" )
  
  address1 = toupper( address )  # upper case all for standardization
  address2 = ifelse( !str_detect( address1, "^\\d+" ) & 
                       str_detect( address1, "^[:alpha:]+\\s+\\d+" ) &
                       address != "", str_remove( address1, "^[:alpha:]+\\s+(?=\\d+)" ),
                     address1 )  # if there is non-numeric text at the start of the address string that is followed by numbers, remove the letters (usually for the name of a place that precedes the address number)
  
  address3 = str_replace_all( address2, "\\s+", " " )  # ensure no multiple empty spaces, replace with one space
  address4 = str_replace( address3, "(\\d+)+\\s+(TH|RD)", "\\1\\2" )  # any spaces between numbered streets and their suffix are removed
  address5 = ifelse( str_detect( address4, "^\\d+[:alpha:]"),
                     str_replace( address4, "(^\\d+)([:alpha:])", "\\1 \\2" ),
                     address4 ) # add a space between street no. and st name if one is not detected
  
  aptnum <- str_extract( address5, "(?<=\\b[NS][EW]\\b\\s).*" )
  
  zipcode <- str_trim( str_extract( aptnum, "(?<!\\sAPT|\\sUNIT|\\s\\#|\\sNO\\.|\\sNUMBER|SUITE)\\d{5}$" ) ) # assumption is that any any 5 digit value at the end of the string not preceded by an apartment or unit keyword is a ZIP code
  
  aptnum1 <- str_trim( str_remove( aptnum, zipcode ) )
  
  if( !is.na( zipcode ) ){
    if( any( !str_detect( zipcode, "^2" ) ) ) warning( "Non-DC ZIP code detected. Double check address input." ) # warning for non-DC ZIP code detection
  }
  
  
  address6 = gsub( x = address5, pattern = "(?<=\\b[NS][EW]\\b\\s).*", replacement = "", perl = TRUE ) # remove apt/unit, first pass
  
  aptnum2 = str_trim( ifelse( is.na(aptnum1), str_extract( address6, "\\b(?:\\sAPT|\\sUNIT|\\s\\#|\\sNO\\.|\\sNUMBER|SUITE)\\b\\s?.*"),
                              aptnum1 ) ) # second pass for detecting apt/uni
  
  address7 = gsub( x = address6, pattern = "\\b(?:\\sAPT|\\sUNIT|\\s\\#|\\sNO\\.|\\sNUMBER|SUITE)\\b\\s?.*", replacement = "", perl = TRUE  ) # remove apt/unit, second pass
  address8 = gsub( x = address7, pattern = "(?<=\\b[NS][EW]\\b\\s).*", replacement = "", perl = TRUE  )
  address9 = gsub( x = address8, pattern =  "^\\s+|\\s+$", replacement = "" ) # remove leading or trailing whitespaces
  address10 = gsub( x = address9, pattern = "[[:punct:]&&[^/'']]", replacement = "", perl = TRUE ) # remove unnecessary punctuation from address string but keep forward slashes
  address11 = str_replace_all( address10, "\\s+", " " ) # remove unnecessary white space
  address12 = str_replace( address11, "(N)\\s+(W)$|(N)\\s+(E)$|(S)\\s+(W)$|(S)\\s+(E)$", 
                           "\\1\\2\\3\\4\\5\\6\\7\\8" ) # rare situation where quadrant letters are spaced apart (e.g., N W)
  
  # Step 1a: Word Extraction
  new1 = word( address12, 1, sep = fixed( " " ) ) # extract address components
  new2 = word( address12, 2, sep = fixed( " " ) )
  new3 = word( address12, 3, sep = fixed( " " ) )
  new4 = word( address12, 4, sep = fixed( " " ) )
  new5 = word( address12, 5, sep = fixed( " " ) )
  new6 = word( address12, 6, sep = fixed( " " ) )
  new7 = word( address12, 7, sep = fixed( " " ) )
  # clean up address components
  new4 = case_when(
    new4 %in% c( "NORTHWEST", "NORTH WEST" ) ~ "NW",
    new4 %in% c( "NORTHEAST", "NORTH EAST" ) ~ "NE",
    new4 %in% c( "SOUTHWEST", "SOUTH WEST" ) ~ "SW",
    new4 %in% c( "SOUTHEAST", "SOUTH EAST" ) ~ "SE",
    new4 == "NORTH" & new5 == "WEST" ~ "NW",
    new4 == "WASHINGTON" ~ "",
    TRUE ~ new4
  )
  
  # Step 1b: Address Component Generation
  addrnum = new1
  stname = case_when( !is.na( new7 ) ~ paste( new2, new3, new4, new5 ), # 4-word street names
                      !is.na( new6 ) ~ paste( new2, new3, new4 ), # 3-word street names
                      !new4 %in% c( 'NW','NE','SE','SW' ) & !is.na( new4 ) ~
                        paste( new2, new3 ), # 2-word street names
                      TRUE ~ new2 ) # all other 1-word street names
  
  street_type = case_when( !is.na( new7 ) ~ case_when( !new6 %in% c( 'NW','NE','SE','SW' ) ~
                                                         new6 ),
                           !is.na( new6 ) ~ case_when( !new5 %in% c( 'NW','NE','SE','SW' ) ~
                                                         new5 ),
                           !new4 %in% c( 'NW','NE','SE','SW' ) & !is.na( new4 ) ~
                             new4,
                           TRUE ~ new3 )
  
  # first pass clean-up: common abbreviations
  street_type2 = case_when( street_type == "AVE" ~ "AVENUE",
                            street_type == "ST" ~ "STREET",
                            street_type == "RD" ~ "ROAD",
                            street_type == "LN" ~ "LANE",
                            street_type == "DR" ~ "DRIVE",
                            street_type == "CT" ~ "COURT",
                            street_type %in% c( "PKWY", "PKY" ) ~ "PARKWAY",
                            street_type == "TERR" ~ "TERRACE",
                            street_type == "PL" ~ "PLACE",
                            street_type == "CIR" ~ "CIRCLE",
                            street_type == "BLVD" ~ "BOULEVARD",
                            street_type == "SQ" ~ "SQUARE",
                            street_type == "WY" ~ "WAY",
                            street_type %in% c( "ALY", "ALLY" ) ~ "ALLEY",
                            TRUE ~ street_type )
  
  quadrant = case_when( !is.na( new7 ) ~ case_when( !new6 %in% c( 'NW','NE','SE','SW' ) ~
                                                      new7,
                                                    new6 %in% c( 'NW','NE','SE','SW' ) ~
                                                      new6 ),
                        !is.na( new6 ) ~ case_when( !new5 %in% c( 'NW','NE','SE','SW' ) ~
                                                      new6,
                                                    new5 %in% c( 'NW','NE','SE','SW' ) ~
                                                      new5 ),
                        !new4 %in% c( 'NW','NE','SE','SW' ) & !is.na( new4 ) ~
                          new5,
                        TRUE ~ new4 )
  
  # Step 1c: Edge Cases
  edge1 = ifelse( !quadrant %in% c( 'NW','NE','SE','SW' ) &
                    !street_type2 %in% c("AVENUE", "STREET", "ROAD", "LANE", "DRIVE",
                                         "COURT", "PARKWAY", "TERRACE", "PLACE",
                                         "CIRCLE", "BOULEVARD", "SQUARE", "WAY",
                                         "ALLEY" ), 1, 0 )
  stname = ifelse( edge1 == 1, paste( stname, street_type2 ), stname )
  street_type2 = ifelse( edge1 == 1, quadrant, street_type2 )
  quadrant = ifelse( edge1 == 1, "", quadrant )
  
  cleanNA <- setNames( lapply( list( addrnum, stname, street_type, street_type2, quadrant, aptnum2, zipcode ),
                               function(x) str_trim( str_remove( ifelse( is.na(x), "", x ), "\\sNA$" ) ) ), # if NA, set to "" and remove any trailing NAs
                       c( "addrnum", "stname", "street_type", "street_type2", "quadrant", "aptnum", "zipcode" ) )
  
  # commonly misspelled DC street names that won't be picked up by geocoder
  cleanNA[["stname"]] <- ifelse( cleanNA[["stname"]] == "CHANCELLORS", "CHANCELLOR'S", cleanNA[["stname"]] )
  
  # finally, compile the cleaned address for DC residents only
  # Step 1d: Generate Final, Clean Address Used for Geocoding
  address3 = paste( cleanNA[["addrnum"]] , cleanNA[["stname"]] , cleanNA[["street_type2"]], cleanNA[["quadrant"]] )
  # address remains the unmodified address string
  
  address4 = str_replace_all( address3, "^\\s+|\\s+$", "" )
  
  # final build/clean
  address_build = str_replace_all( address4, paste0( "(\\d+)\\s*", # street number followed by 0 or more ws
                                                     "(.+?)\\s*",  # street name: 1 or more characters followed by 0 or more ws
                                                     "((", paste0( street_types, collapse = "|"), "))\\b", # street type (one of any in `street_type`)
                                                     "((", paste0( quad_types, collapse = "|"), "))",        # quadrant type (one of any in `quad_type`)
                                                     "((APT|UNIT|\\#|NO\\.|NUMBER|SUITE)*)", # 0 or more of APT or UNIT labels
                                                     "(\\d*)",      # 0 or more of APT or UNIT number
                                                     "(200\\d+)*",    # ZIP code, if present
                                                     "(US|USA|U\\.S\\.|U\\.S\\A\\.)*" ), # USA, if present
                                   "\\1 \\2 \\3 \\5 \\7 \\9" ) # the numbering is dependent on nested parentheses in the regex as well, e.g., "(())" counts as two sets of parentheses/groups
  
  
  address_clean = str_replace_all( address_build, "\\s\\s+", " " ) # remove leading and trailing white space
  
  # return object below
  return( as.data.frame( list( INPUT = address, address_clean = address_clean, cleanNA ) ) %>% 
            rename_all( toupper ) )
}

a <- "2201 Shannon Pl SE"


library(microbenchmark)
microbenchmark( dc_address_parser( a ),
                dc_address_parser_v2( a ) )

