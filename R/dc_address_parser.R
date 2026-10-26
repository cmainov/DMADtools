library( stringr )
## Step 1: Address Parsing

street_types <- c( "AVE","AVENUE","ST","STREET","RD","ROAD","LN","LANE",
                   "DR","DRIVE","CT","COURT","PKWY","PKY","PARKWAY","TERR",
                   "TERRACE","PL","PLACE","CIR","CIRCLE","BLVD","BOULEVARD",
                   "SQ","SQUARE","WY","WAY","ALY","ALLY","ALLEY" )

quad_types <- c( "NW","NE","SE","SW",
                 "NORTHWEST","NORTH WEST","NORTHEAST","NORTH EAST",
                 "SOUTHWEST","SOUTH WEST","SOUTHEAST","SOUTH EAST" )

ADDRESS= c( "1600 Pennsylvania Ave NW", "2201 Shannon Pl SE" )


ADDRESS2 = toupper( ADDRESS )  # upper case all for standardization
ADDRESS2 = ifelse( !str_detect( ADDRESS, "^\\d+" ) & 
                     str_detect( ADDRESS, "^[:alpha:]+\\s+\\d+" ) &
                     ADDRESS != "", str_remove( ADDRESS2, "^[:alpha:]+\\s+(?=\\d+)" ),
                   ADDRESS2 )  # if there is non-numeric text at the start of the address string that is followed by numbers, remove the letters (usually for the name of a place that precedes the address number)

ADDRESS2 = str_replace_all( ADDRESS2, "\\s+", " " )  # ensure no multiple empty spaces, replace with one space
ADDRESS2 = str_replace( ADDRESS2, "(\\d+)+\\s+(TH|RD)", "\\1\\2" )  # any spaces between numbered streets and their suffix are removed
ADDRESS2 = ifelse( str_detect( ADDRESS2, "^\\d+[:alpha:]"),
                   str_replace( ADDRESS2, "(^\\d+)([:alpha:])", "\\1 \\2" ),
                   ADDRESS2 ) # add a space between street no. and st name if one is not detected

APTNUM = str_extract( ADDRESS2, "(?<=\\b[NS][EW]\\b\\s).*" )
ADDRESS2 = str_remove( ADDRESS2, "(?<=\\b[NS][EW]\\b\\s).*" ) # remove apt/unit, first pass


APTNUM = ifelse( is.na(APTNUM), str_extract( ADDRESS2, "\\b(?:APT|UNIT)\\b\\s?.*"),
                 APTNUM ) # second pass for detecting apt/uni

ADDRESS2 = str_remove( ADDRESS2, "\\b(?:APT|UNIT)\\b\\s?.*" ) # remove apt/unit, second pass
ADDRESS2 = str_remove( ADDRESS2, "(?<=\\b[NS][EW]\\b\\s).*" )
ADDRESS2 = str_remove( ADDRESS2, "^\\s+|\\s+$" ) # remove leading or trailing whitespaces
ADDRESS2 = str_remove_all( ADDRESS2, "[[:punct:]&&[^/'']]" ) # remove unnecessary punctuation from address string but keep forward slashes
ADDRESS2 = str_replace_all( ADDRESS2, "\\s+", " " ) # remove unnecessary white space
ADDRESS2 = str_replace( ADDRESS2, "(N)\\s+(W)$|(N)\\s+(E)$|(S)\\s+(W)$|(S)\\s+(E)$", 
                        "\\1\\2\\3\\4\\5\\6\\7\\8" ) # rare situation where quadrant letters are spaced apart (e.g., N W)

# Step 1a: Word Extraction
new1 = word( ADDRESS2, 1, sep = fixed( " " ) ) # extract address components
new2 = word( ADDRESS2, 2, sep = fixed( " " ) )
new3 = word( ADDRESS2, 3, sep = fixed( " " ) )
new4 = word( ADDRESS2, 4, sep = fixed( " " ) )
new5 = word( ADDRESS2, 5, sep = fixed( " " ) )
new6 = word( ADDRESS2, 6, sep = fixed( " " ) )
new7 = word( ADDRESS2, 7, sep = fixed( " " ) )
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
ADDRNUM = new1
STNAME = case_when( !is.na( new7 ) ~ paste( new2, new3, new4, new5 ), # 4-word street names
                    !is.na( new6 ) ~ paste( new2, new3, new4 ), # 3-word street names
                    !new4 %in% c( 'NW','NE','SE','SW' ) & !is.na( new4 ) ~
                      paste( new2, new3 ), # 2-word street names
                    TRUE ~ new2 ) # all other 1-word street names

STREET_TYPE = case_when( !is.na( new7 ) ~ case_when( !new6 %in% c( 'NW','NE','SE','SW' ) ~
                                                       new6 ),
                         !is.na( new6 ) ~ case_when( !new5 %in% c( 'NW','NE','SE','SW' ) ~
                                                       new5 ),
                         !new4 %in% c( 'NW','NE','SE','SW' ) & !is.na( new4 ) ~
                           new4,
                         TRUE ~ new3 )

# first pass clean-up: common abbreviations
STREET_TYPE2 = case_when( STREET_TYPE == "AVE" ~ "AVENUE",
                          STREET_TYPE == "ST" ~ "STREET",
                          STREET_TYPE == "RD" ~ "ROAD",
                          STREET_TYPE == "LN" ~ "LANE",
                          STREET_TYPE == "DR" ~ "DRIVE",
                          STREET_TYPE == "CT" ~ "COURT",
                          STREET_TYPE %in% c( "PKWY", "PKY" ) ~ "PARKWAY",
                          STREET_TYPE == "TERR" ~ "TERRACE",
                          STREET_TYPE == "PL" ~ "PLACE",
                          STREET_TYPE == "CIR" ~ "CIRCLE",
                          STREET_TYPE == "BLVD" ~ "BOULEVARD",
                          STREET_TYPE == "SQ" ~ "SQUARE",
                          STREET_TYPE == "WY" ~ "WAY",
                          STREET_TYPE %in% c( "ALY", "ALLY" ) ~ "ALLEY",
                          TRUE ~ STREET_TYPE )

QUADRANT = case_when( !is.na( new7 ) ~ case_when( !new6 %in% c( 'NW','NE','SE','SW' ) ~
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
EDGE1 = ifelse( !QUADRANT %in% c( 'NW','NE','SE','SW' ) &
                  !STREET_TYPE2 %in% c("AVENUE", "STREET", "ROAD", "LANE", "DRIVE",
                                       "COURT", "PARKWAY", "TERRACE", "PLACE",
                                       "CIRCLE", "BOULEVARD", "SQUARE", "WAY",
                                       "ALLEY" ), 1, 0 )
STNAME = ifelse( EDGE1 == 1, paste( STNAME, STREET_TYPE2 ), STNAME )
STREET_TYPE2 = ifelse( EDGE1 == 1, QUADRANT, STREET_TYPE2 )
QUADRANT = ifelse( EDGE1 == 1, "", QUADRANT )

cleanNA <- setNames( lapply( list( ADDRNUM, STNAME, STREET_TYPE, STREET_TYPE2, QUADRANT ),
                             function(x) str_remove( ifelse( is.na(x), "", x ), "\\sNA$" ) ), # if NA, set to "" and remove any trailing NAs
                     c( "ADDRNUM", "STNAME", "STREET_TYPE", "STREET_TYPE2", "QUADRANT" ) )

# commonly misspelled DC street names that won't be picked up by geocoder
cleanNA[["STNAME"]] <- ifelse( cleanNA[["STNAME"]] == "CHANCELLORS", "CHANCELLOR'S", cleanNA[["STNAME"]] )

# finally, compile the cleaned address for DC residents only
# Step 1d: Generate Final, Clean Address Used for Geocoding
ADDRESS3 = paste( cleanNA[["ADDRNUM"]] , cleanNA[["STNAME"]] , cleanNA[["STREET_TYPE2"]], cleanNA[["QUADRANT"]] )
# ADDRESS remains the unmodified address string

ADDRESS4 = str_replace_all( ADDRESS3, "^\\s+|\\s+$", "" )

# final build/clean
ADDRESS_BUILD = str_replace_all( ADDRESS4, paste0( "(\\d+)\\s*", # street number followed by 0 or more ws
                                                   "(.+?)\\s*",  # street name: 1 or more characters followed by 0 or more ws
                                                   "((", paste0(street_types, collapse = "|"), "))\\b", # street type (one of any in `street_type`)
                                                   "((", paste0( quad_types, collapse = "|"), "))",        # quadrant type (one of any in `quad_type`)
                                                   "((APT|UNIT)*)", # 0 or more of APT or UNIT labels
                                                   "(\\d*)",      # 0 or more of APT or UNIT number
                                                   "(200\\d+)*",    # ZIP code, if present
                                                   "(US|USA|U\\.S\\.|U\\.S\\A\\.)*" ), # USA, if present
                                 "\\1 \\2 \\3 \\5 \\7 \\9" ) # the numbering is dependent on nested parentheses in the regex as well, e.g., "(())" counts as two sets of parentheses/groups


ADDRESS_CLEAN = str_replace_all( ADDRESS_BUILD, "\\s\\s+", " " ) # remove leading and trailing white space

# return object below
as.data.frame( list( INPUT = ADDRESS, ADDRESS_CLEAN = ADDRESS_CLEAN, cleanNA ) )
