if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

ahs_cat <-
	get_catalog( "ahs" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( ahs_cat ) ) / ceiling( nrow( ahs_cat ) / 3 ) )

ahs_cat <- unique( rbind( ahs_cat[ record_categories == this_sample_break , ] , ahs_cat[ ahs_cat$year == 2015 , ] ) )

lodown( "ahs" , ahs_cat )
