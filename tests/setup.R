if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

ahs_cat <-
	get_catalog( "ahs" ,
		output_dir = file.path( getwd() ) )

# sample 75% of the records
which_records <- sample( seq( nrow( ahs_cat ) ) , round( nrow( ahs_cat ) * 0.75 ) )

# always sample year == 2015
ahs_cat <- unique( rbind( ahs_cat[ which_records , ] , subset( ahs_cat , year == 2015 ) ) )

lodown( "ahs" , ahs_cat )
