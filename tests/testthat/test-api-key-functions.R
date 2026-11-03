

# no errors for normal run
key_real <- Sys.getenv( "MAR2_KEY" )

key_fake <- "2789"

write_mar2_api_key( key = key_fake, overwrite = TRUE )

readRenviron( "~/.Renviron" )

test_that( "normal run of `write` and `read`", {

  # reboot the file in the session environment
  readRenviron( "~/.Renviron" )
  
  # check that the key was correctly written 
  # home directory
  r_path <- Sys.getenv( "HOME" )
  
  # .renviron path 
  renviron_path <- file.path( r_path, ".Renviron" )
  
  #.renviron exists logic, generate file if it doesn't exist
  if( !file.exists( renviron_path ) ) file.create( renviron_path )
  
  # read .renviron
  read_renviron <- readLines( renviron_path )
  
  which_mar2 <- which( grepl( pattern = "MAR2_KEY", x = read_renviron ) )
  
  # check `write` function correctly wrote it
  expect_true( read_renviron[ which_mar2 ] == paste0( "MAR2_KEY=", "\"", key_fake, "\"" ) )
  
  # check read function correctly read it
  expect_true( read_mar2_api_key() == key_fake ) 

} )


# expect warning when overwrite set to FALSE
test_that( "warning overwrite in `write`", {
  
  expect_warning( write_mar2_api_key( key = key_fake ) )
  
} )

# expect errors when key is NULL
test_that( "error for key in `write`", { 

  expect_error( write_mar2_api_key( key = c( "KEY1", "KEY2" ), overwrite = FALSE )  )
  
  expect_error( write_mar2_api_key( key = NULL, overwrite = FALSE )  )
  
  expect_error( write_mar2_api_key( key = "", overwrite = FALSE )  )
  
} )



test_that( "no error and functionality in  `read`", { 
  
  expect_no_error( write_mar2_api_key( key = key_fake, overwrite = TRUE ) )
  
  # check that the key was correctly written 
  # home directory
  r_path <- Sys.getenv( "HOME" )
  
  # .renviron path 
  renviron_path <- file.path( r_path, ".Renviron" )
  
  #.renviron exists logic, generate file if it doesn't exist
  if( !file.exists( renviron_path ) ) file.create( renviron_path )
  
  # read .renviron
  read_renviron <- readLines( renviron_path )
  
  which_mar2 <- which( grepl( pattern = "MAR2_KEY", x = read_renviron ) )
  expect_true( read_renviron[ which_mar2 ] == paste0( "MAR2_KEY=", "\"", key_fake, "\"" ) )
  expect_true( read_mar2_api_key() == key_fake )
  # write back original key if present
  if( key_real != "" ) expect_no_error( write_mar2_api_key( key = key_real, overwrite = TRUE ) )
  
} )


# write back original key if present
write_mar2_api_key( key = key_real, overwrite = TRUE ) 
readRenviron( "~/.Renviron" ) # reboot .renviron
