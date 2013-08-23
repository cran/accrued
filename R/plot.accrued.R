##########################################################################################

plot.accrued =  function(x, ...) { 

	## Throw an error if the argument is not of the correct class.
	if( class(x) != "accrued" )  stop("ERROR: argument is not an object of the 'data.accrued' class.")

	accrued_data = x		


	## Get largest lag from data (lag is counted from zero, so it will be maximum column number minus 1).
	MAX_LAGS = ncol( accrued_data[["data"]] ) - 1

	## Throw an error if there are not enough columns of data.
	if( MAX_LAGS == 0 ) stop("The function 'plot.accrued' is useful only if there are at least two lag columns.")

	NROW = nrow( accrued_data[["data"]] )

	## The "stackedUploadData" takes the "data" component of a data.accrued object and
	## stacks it, adding "lag" as a column variable. It also calculates the number of counts added
	## on a particular encounter date from one upload date to the next.
	STACKED = stackedUploadData( accrued_data )
 	
 	FIRST_NON_NA_ENCOUNTER_DATE = min( STACKED[ !is.na(STACKED[,"NumberAdded"]) , "EncounterDate" ] )
 	FINAL_NON_NA_ENCOUNTER_DATE = max( STACKED[ !is.na(STACKED[,"NumberAdded"]) , "EncounterDate" ] )
 	
 	MAX_HEIGHT_BY_LAG = MIN_HEIGHT_BY_LAG = CUMULATIVE_HEIGHTS = rep( NA, times=MAX_LAGS )
	
 	for( lag in 1:MAX_LAGS ) {
		MAX_HEIGHT_BY_LAG[lag] = max(0, max( STACKED[ (STACKED[,"Lag"] == lag ), "NumberAdded"], na.rm=T ) )
		MIN_HEIGHT_BY_LAG[lag] = min(0, min( STACKED[ (STACKED[,"Lag"] == lag ), "NumberAdded"], na.rm=T ) )
 	}	
	
	ABS_MIN_HEIGHT_BY_LAG = abs(MIN_HEIGHT_BY_LAG)
	
	CUMULATIVE_HEIGHTS[1] = MAX_HEIGHT_BY_LAG[1] + MIN_HEIGHT_BY_LAG[1]
 	for( lag in 2:MAX_LAGS ) {
		CUMULATIVE_HEIGHTS[lag] = ( abs(MAX_HEIGHT_BY_LAG[lag]) + abs(MIN_HEIGHT_BY_LAG[lag])  + CUMULATIVE_HEIGHTS[lag-1])
	}

	PADDING =  max(c(abs(MAX_HEIGHT_BY_LAG), abs(MIN_HEIGHT_BY_LAG)))

	ZERO_HEIGHTS = rep(NA, times=MAX_LAGS)
	ZERO_HEIGHTS[1] =   ABS_MIN_HEIGHT_BY_LAG[1]

	UPPER_BUFFER_VECTOR = rep(NA, times=MAX_LAGS)
	LOWER_BUFFER_VECTOR = rep(NA, times=MAX_LAGS)
	LOWER_BUFFER_VECTOR[1] = ABS_MIN_HEIGHT_BY_LAG[1]
	UPPER_BUFFER_VECTOR[1] = ABS_MIN_HEIGHT_BY_LAG[1] + MAX_HEIGHT_BY_LAG[1]

	for( zero in 2:MAX_LAGS ) {
		ZERO_HEIGHTS[zero]        = ZERO_HEIGHTS[zero-1] + MAX_HEIGHT_BY_LAG[zero-1] + PADDING + ABS_MIN_HEIGHT_BY_LAG[zero]
		LOWER_BUFFER_VECTOR[zero] = ZERO_HEIGHTS[zero] - ABS_MIN_HEIGHT_BY_LAG[zero]
		UPPER_BUFFER_VECTOR[zero] = ZERO_HEIGHTS[zero] + MAX_HEIGHT_BY_LAG[zero]
	}

	y_MAX =  max(ZERO_HEIGHTS) + MAX_HEIGHT_BY_LAG[MAX_LAGS]

	x_VALUES = FIRST_NON_NA_ENCOUNTER_DATE: FINAL_NON_NA_ENCOUNTER_DATE
	y_VALUES_01 = STACKED[ (STACKED[,"Lag"] == 1) , "NumberAdded" ]	 + ZERO_HEIGHTS[1]

	plot( x_VALUES, y_VALUES_01, 
		  xlim=c(min(x_VALUES, na.rm=T)-1, max(x_VALUES, na.rm=T)), 
		  ylim=c(0, y_MAX), 
		  xlab="", ylab="", 
		  type='n', axes=F, xaxs="i", yaxs="i", ... )
	abline( h=ZERO_HEIGHTS[1], col="gray")
	points( x_VALUES, y_VALUES_01, pch=".", cex=0.5, col="blue" )
	lines( x_VALUES, y_VALUES_01, col="blue" )

	abline(h=UPPER_BUFFER_VECTOR,col="orange", lwd=0.1)
	abline(h=LOWER_BUFFER_VECTOR,col="orange", lwd=0.1)

	for( lag in 2:(MAX_LAGS) ) {
		COLOR="blue"
		if( lag %% 2 == 0 ) COLOR = "forestgreen"
		abline( h=ZERO_HEIGHTS[lag], col="gray" )
		points( x_VALUES, STACKED[ (STACKED[,"Lag"] == lag) , "NumberAdded" ] + ZERO_HEIGHTS[lag], pch=".", cex=0.5, col=COLOR )
		lines(  x_VALUES, STACKED[ (STACKED[,"Lag"] == lag) , "NumberAdded" ] + ZERO_HEIGHTS[lag], col=COLOR)
	}


 	### y-axis labels.
	y_AXIS_INDEX  		= seq( 1, MAX_LAGS, by=min(4, floor(MAX_LAGS/2)) )
	y_LOWER_ENDPOINTS 	= 0:(MAX_LAGS-1)
	y_UPPER_ENDPOINTS	= 1:MAX_LAGS
	y_LABEL_TEXT   		= paste(y_LOWER_ENDPOINTS[y_AXIS_INDEX], "-", y_UPPER_ENDPOINTS[y_AXIS_INDEX], sep="")
	y_LABEL_HEIGHTS  	= ZERO_HEIGHTS[y_AXIS_INDEX] 
	axis( 2, at=y_LABEL_HEIGHTS, labels=y_LABEL_TEXT, cex.axis=0.8, las=2, font.axis=1 )

 	### x-axis labels.
 	NUMBER_OF_LABELS = 10
	JUMP = round( (NROW/NUMBER_OF_LABELS)/5, 0 ) * 10
	if( JUMP == 0 ) JUMP = 2
	x_TICK_PLACES = ( 0:NUMBER_OF_LABELS ) * JUMP
	x_LABELS = x_TICK_PLACES
	if( accrued_data[["start"]][[1]] ){
		START_DATE 	= accrued_data[["start"]][[2]]
		x_LABELS 	= as.Date(START_DATE + x_TICK_PLACES, origin=as.Date("1970-01-01")) 
		axis(1, at=x_TICK_PLACES, labels=x_LABELS, cex.axis=0.8, las=2, font.axis=1) 
	} else {
		axis(1, at=x_TICK_PLACES, labels=x_LABELS, cex.axis=0.8, las=1, font.axis=1) 
	}
	
	        
} 



