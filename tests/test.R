if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available AHS microdata files
ahs_cat <-
	get_catalog( "ahs" ,
		output_dir = file.path( getwd() ) )

# 2015 only
ahs_cat <- subset( ahs_cat , year == 2015 )
# download the microdata to your local computer
stopifnot( nrow( ahs_cat ) > 0 )

options( survey.replicates.mse = TRUE )

library(survey)

ahs_df <- 
	readRDS( 
		file.path( getwd() , 
			"2015/national_v1.2/household.rds" 
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
				
				
		condominium = as.numeric( condo == 1 )
				
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
svyciprop( ~ condominium , ahs_design ,
	method = "likelihood" )
svyttest( totrooms ~ condominium , ahs_design )
svychisq( 
	~ condominium + lotsize , 
	ahs_design 
)
glm_result <- 
	svyglm( 
		totrooms ~ condominium + lotsize , 
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

