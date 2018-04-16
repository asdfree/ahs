if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
ahs_cat <- get_catalog( "ahs" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( ahs_cat ) ) / ceiling( nrow( ahs_cat ) / 6 ) )
ahs_cat <- ahs_cat[ record_categories == this_sample_break , ]
ahs_cat <- lodown( "ahs" , ahs_cat )
if( any( ahs_cat$year == 2013 ) ){











library(survey)

ahs_df <- 
	readRDS( 
		file.path( getwd() , 
			"2013/national_v1.2/newhouse_repwgt.rds" 
		) 
	)

ahs_design <- 
	svrepdesign(
		weights = ~wgt90geo,
		repweights = "repwgt[1-9]" ,
		type = "Fay" ,
		rho = ( 1 - 1 / sqrt( 4 ) ) ,
		mse = TRUE ,
		data = ahs_df
	)
ahs_design <- 
	update( 
		ahs_design , 

		tenure = 
			factor( 
				ifelse( is.na( tenure ) , 4 , tenure ) , 
				levels = 1:4 , 
				labels = 
					c( 'Owned or being bought' ,
					'Rented for cash rent' ,
					'Occupied without payment of cash rent' ,
					'Not occupied' )
			) ,
			
			
		lotsize =
			factor( 
				1 + findInterval( lot ,
					c( 5500 , 11000 , 22000 , 
					44000 , 220000 , 440000 ) ) , 
				levels = 1:7 ,
				labels = c( "Less then 1/8 acre" , 
				"1/8 up to 1/4 acre" , "1/4 up to 1/2 acre" ,
				"1/2 up to 1 acre" , "1 up to 5 acres" , 
				"5 up to 10 acres" , "10 acres or more" ) ) ,
				
				
		below_poverty = as.numeric( poor < 100 )
				
	)
sum( weights( ahs_design , "sampling" ) != 0 )

svyby( ~ one , ~ tenure , ahs_design , unwtd.count )
svytotal( ~ one , ahs_design )

svyby( ~ one , ~ tenure , ahs_design , svytotal )
svymean( ~ rooms , ahs_design )

svyby( ~ rooms , ~ tenure , ahs_design , svymean )
svymean( ~ lotsize , ahs_design , na.rm = TRUE )

svyby( ~ lotsize , ~ tenure , ahs_design , svymean , na.rm = TRUE )
svytotal( ~ rooms , ahs_design )

svyby( ~ rooms , ~ tenure , ahs_design , svytotal )
svytotal( ~ lotsize , ahs_design , na.rm = TRUE )

svyby( ~ lotsize , ~ tenure , ahs_design , svytotal , na.rm = TRUE )
svyquantile( ~ rooms , ahs_design , 0.5 )

svyby( 
	~ rooms , 
	~ tenure , 
	ahs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ rooms , 
	denominator = ~ rent , 
	ahs_design ,
	na.rm = TRUE
)
sub_ahs_design <- subset( ahs_design , garage == 1 )
svymean( ~ rooms , sub_ahs_design )
this_result <- svymean( ~ rooms , ahs_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ rooms , 
		~ tenure , 
		ahs_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( ahs_design )
svyvar( ~ rooms , ahs_design )
# SRS without replacement
svymean( ~ rooms , ahs_design , deff = TRUE )

# SRS with replacement
svymean( ~ rooms , ahs_design , deff = "replace" )
svyciprop( ~ below_poverty , ahs_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( rooms ~ below_poverty , ahs_design )
svychisq( 
	~ below_poverty + lotsize , 
	ahs_design 
)
glm_result <- 
	svyglm( 
		rooms ~ below_poverty + lotsize , 
		ahs_design 
	)

summary( glm_result )
library(srvyr)
ahs_srvyr_design <- as_survey( ahs_design )
ahs_srvyr_design %>%
	summarize( mean = survey_mean( rooms ) )

ahs_srvyr_design %>%
	group_by( tenure ) %>%
	summarize( mean = survey_mean( rooms ) )
means <- c( 1241.8890 , 972.6051 , 170.0121 )
std_err <- c( 7.3613 , 5.6956 , 6.1586 )
ci_lb <- c( 1227.3511 , 961.3569 , 157.8495 )
ci_ub <- c( 1256.4270 , 983.8532 , 182.1747 )

results <- 
	svyby( 
		~ zsmhc , 
		~ tenure , 
		ahs_design , 
		svymean , 
		na.rm = TRUE , 
		na.rm.all = TRUE 
	)

ci_res <- 
	confint( results , df = degf( ahs_design ) + 1 )

stopifnot( all( round( coef( results ) , 4 ) == means ) )

stopifnot( all( round( SE( results ) , 4 ) == std_err ) )

stopifnot( all( round( ci_res[ , 1 ] , 4 ) == ci_lb ) )

stopifnot( all( round( ci_res[ , 2 ] , 4 ) == ci_ub ) )
}
