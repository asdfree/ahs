#
#
#
library(haven)
library(httr)

tf <- tempfile()

this_url <-
	paste0(
		"https://www2.census.gov/programs-surveys/ahs/" ,
		"2021/AHS%202021%20National%20PUF%20v1.0%20Flat%20SAS.zip"
	)

GET( this_url , write_disk( tf ) , progress() )

ahs_tbl <- read_sas( tf )

ahs_df <- data.frame( ahs_tbl )

names( ahs_df ) <- tolower( names( ahs_df ) )
# ahs_fn <- file.path( path.expand( "~" ) , "AHS" , "this_file.rds" )
# saveRDS( ahs_df , file = ahs_fn , compress = FALSE )
# ahs_df <- readRDS( ahs_fn )
library(survey)

ahs_design <- 
	svrepdesign(
		weights = ~ weight ,
		repweights = "repweight[1-9]" ,
		type = "Fay" ,
		rho = ( 1 - 1 / sqrt( 4 ) ) ,
		mse = TRUE ,
		data = ahs_df
	)
ahs_design <- 
	update( 
		ahs_design , 

		one = 1 ,

		tenure = 
			factor( 
				ifelse( tenure %in% c( -6 , 'N' ) , 4 , tenure ) , 
				levels = 1:4 , 
				labels = 
					c( 'Owned or being bought' ,
					'Rented for cash rent' ,
					'Occupied without payment of cash rent' ,
					'Not occupied' )
			) ,
			
			
		lotsize =
			factor( 
				lotsize , 
				levels = 1:7 ,
				labels = c( "Less then 1/8 acre" , 
				"1/8 up to 1/4 acre" , "1/4 up to 1/2 acre" ,
				"1/2 up to 1 acre" , "1 up to 5 acres" , 
				"5 up to 10 acres" , "10 acres or more" ) ) ,
				
				
		below_poverty = as.numeric( perpovlvl < 100 )
				
	)
sum( weights( ahs_design , "sampling" ) != 0 )

svyby( ~ one , ~ tenure , ahs_design , unwtd.count )
svytotal( ~ one , ahs_design )

svyby( ~ one , ~ tenure , ahs_design , svytotal )
svymean( ~ totrooms , ahs_design , na.rm = TRUE )

svyby( ~ totrooms , ~ tenure , ahs_design , svymean , na.rm = TRUE )
svymean( ~ lotsize , ahs_design , na.rm = TRUE )

svyby( ~ lotsize , ~ tenure , ahs_design , svymean , na.rm = TRUE )
svytotal( ~ totrooms , ahs_design , na.rm = TRUE )

svyby( ~ totrooms , ~ tenure , ahs_design , svytotal , na.rm = TRUE )
svytotal( ~ lotsize , ahs_design , na.rm = TRUE )

svyby( ~ lotsize , ~ tenure , ahs_design , svytotal , na.rm = TRUE )
svyquantile( ~ totrooms , ahs_design , 0.5 , na.rm = TRUE )

svyby( 
	~ totrooms , 
	~ tenure , 
	ahs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ totrooms , 
	denominator = ~ rent , 
	ahs_design ,
	na.rm = TRUE
)
sub_ahs_design <- subset( ahs_design , garage == 1 )
svymean( ~ totrooms , sub_ahs_design , na.rm = TRUE )
this_result <- svymean( ~ totrooms , ahs_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ totrooms , 
		~ tenure , 
		ahs_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( ahs_design )
svyvar( ~ totrooms , ahs_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ totrooms , ahs_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ totrooms , ahs_design , na.rm = TRUE , deff = "replace" )
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
result <- svytotal( ~ as.numeric( intstatus == 1 ) , ahs_design )

stopifnot( round( coef( result ) / 1000 , 0 ) == 128504 )

ci_results <- confint( result , level = 0.9 )

stopifnot( round( ( ci_results[ 2 ] - coef( result ) ) / 1000 , 0 ) == 388 )
library(srvyr)
ahs_srvyr_design <- as_survey( ahs_design )
ahs_srvyr_design %>%
	summarize( mean = survey_mean( totrooms , na.rm = TRUE ) )

ahs_srvyr_design %>%
	group_by( tenure ) %>%
	summarize( mean = survey_mean( totrooms , na.rm = TRUE ) )
