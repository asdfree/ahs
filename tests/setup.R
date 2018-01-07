if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
lodown( "ahs" , output_dir = file.path( getwd() ) )
this_sample_break <- Sys.getenv( "this_sample_break" )
ahs_cat <- get_catalog( "ahs" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( ahs_cat ) ) / ceiling( nrow( ahs_cat ) / 5 ) )
ahs_cat <- ahs_cat[ record_categories == this_sample_break , ]
lodown( "ahs" , ahs_cat )
if( any( ahs_cat$year == 2015 ) ){
library(lodown)
# examine all available AHS microdata files
ahs_cat <-
	get_catalog( "ahs" ,
		output_dir = file.path( getwd() ) )

# 2015 only
ahs_cat <- subset( ahs_cat , year == 2015 )
# download the microdata to your local computer
lodown( "ahs" , ahs_cat )

options( survey.replicates.mse = TRUE )

library(survey)

ahs_df <- 
	readRDS( 
		file.path( getwd() , 
			"2015/national_v1.3/household.rds" 
		) 
	)

ahs_design <- 
	svrepdesign(
		weights = ~weight,
		repweights = "repwgt[1-9]" ,
		type = "Fay" ,
		rho = ( 1 - 1 / sqrt( 4 ) ) ,
		data = ahs_df
	)
ahs_design <- 
	update( 
		ahs_design , 

		occupant = 
			ifelse( tenure == 1 , "owner" , 
			ifelse( tenure %in% 2:3 , "renter" , 
				"not occupied" ) ) ,
				
		lotsize =
			factor( lotsize , levels = 1:7 ,
				labels = c( "Less then 1/8 acre" , 
				"1/8 up to 1/4 acre" , "1/4 up to 1/2 acre" ,
				"1/2 up to 1 acre" , "1 up to 5 acres" , 
				"5 up to 10 acres" , "10 acres or more" ) ) ,
				
				
		below_poverty = as.numeric( perpovlvl < 100 )
				
	)
sum( weights( ahs_design , "sampling" ) != 0 )

svyby( ~ one , ~ occupant , ahs_design , unwtd.count )
svytotal( ~ one , ahs_design )

svyby( ~ one , ~ occupant , ahs_design , svytotal )
svymean( ~ totrooms , ahs_design )

svyby( ~ totrooms , ~ occupant , ahs_design , svymean )
svymean( ~ lotsize , ahs_design , na.rm = TRUE )

svyby( ~ lotsize , ~ occupant , ahs_design , svymean , na.rm = TRUE )
svytotal( ~ totrooms , ahs_design )

svyby( ~ totrooms , ~ occupant , ahs_design , svytotal )
svytotal( ~ lotsize , ahs_design , na.rm = TRUE )

svyby( ~ lotsize , ~ occupant , ahs_design , svytotal , na.rm = TRUE )
svyquantile( ~ totrooms , ahs_design , 0.5 )

svyby( 
	~ totrooms , 
	~ occupant , 
	ahs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ totrooms , 
	denominator = ~ rent , 
	ahs_design ,
	na.rm = TRUE
)
sub_ahs_design <- subset( ahs_design , garage == 1 )
svymean( ~ totrooms , sub_ahs_design )
this_result <- svymean( ~ totrooms , ahs_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ totrooms , 
		~ occupant , 
		ahs_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( ahs_design )
svyvar( ~ totrooms , ahs_design )
# SRS without replacement
svymean( ~ totrooms , ahs_design , deff = TRUE )

# SRS with replacement
svymean( ~ totrooms , ahs_design , deff = "replace" )
svyciprop( ~ below_poverty , ahs_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( totrooms ~ below_poverty , ahs_design )
svychisq( 
	~ below_poverty + lotsize , 
	ahs_design 
)
glm_result <- 
	svyglm( 
		totrooms ~ below_poverty + lotsize , 
		ahs_design 
	)

summary( glm_result )
library(srvyr)
ahs_srvyr_design <- as_survey( ahs_design )
ahs_srvyr_design %>%
	summarize( mean = survey_mean( totrooms ) )

ahs_srvyr_design %>%
	group_by( occupant ) %>%
	summarize( mean = survey_mean( totrooms ) )

}
