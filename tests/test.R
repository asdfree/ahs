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

library(survey)

ahs_df <- readRDS( file.path( getwd() , "2015 main.rds" ) )

ahs_design <- 
	svydesign( 
		~ psu , 
		strata = ~ stratum , 
		data = ahs_df , 
		weights = ~ weight , 
		nest = TRUE 
	)
ahs_design <- 
	update( 
		ahs_design , 
		q2 = q2 ,
		never_rarely_wore_bike_helmet = as.numeric( qn8 == 1 ) ,
		ever_smoked_marijuana = as.numeric( qn47 == 1 ) ,
		ever_tried_to_quit_cigarettes = as.numeric( q36 > 2 ) ,
		smoked_cigarettes_past_year = as.numeric( q36 > 1 )
	)
sum( weights( ahs_design , "sampling" ) != 0 )

svyby( ~ one , ~ ever_smoked_marijuana , ahs_design , unwtd.count )
svytotal( ~ one , ahs_design )

svyby( ~ one , ~ ever_smoked_marijuana , ahs_design , svytotal )
svymean( ~ bmipct , ahs_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , ahs_design , svymean , na.rm = TRUE )
svymean( ~ q2 , ahs_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , ahs_design , svymean , na.rm = TRUE )
svytotal( ~ bmipct , ahs_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , ahs_design , svytotal , na.rm = TRUE )
svytotal( ~ q2 , ahs_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , ahs_design , svytotal , na.rm = TRUE )
svyquantile( ~ bmipct , ahs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ ever_smoked_marijuana , 
	ahs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ever_tried_to_quit_cigarettes , 
	denominator = ~ smoked_cigarettes_past_year , 
	ahs_design ,
	na.rm = TRUE
)
sub_ahs_design <- subset( ahs_design , qn41 == 1 )
svymean( ~ bmipct , sub_ahs_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , ahs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ ever_smoked_marijuana , 
		ahs_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( ahs_design )
svyvar( ~ bmipct , ahs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , ahs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , ahs_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ never_rarely_wore_bike_helmet , ahs_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_bike_helmet , ahs_design )
svychisq( 
	~ never_rarely_wore_bike_helmet + q2 , 
	ahs_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_bike_helmet + q2 , 
		ahs_design 
	)

summary( glm_result )
library(srvyr)
ahs_srvyr_design <- as_survey( ahs_design )
ahs_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

ahs_srvyr_design %>%
	group_by( ever_smoked_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

unwtd.count( ~ never_rarely_wore_bike_helmet , yrbss_design )

svytotal( ~ one , subset( yrbss_design , !is.na( never_rarely_wore_bike_helmet ) ) )
 
svymean( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE )

svyciprop( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE , method = "beta" )

