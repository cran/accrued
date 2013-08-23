barcode = function(x, ...) { 

	## Throw an error if the argument is not of the correct class.
	if( class(x) != "accrued" )  stop("ERROR: argument is not an object of the 'data.accrued' class.")

	accrued_data = x		

	data = accrued_data[["data"]]
	STACKED = stackedUploadData( accrued_data )	

	UPLOAD_DATE_vs_NUM_ADDED = as.matrix( cbind( STACKED[ , "UploadDate"]  , STACKED[ , "NumberAdded"] ) )

	## X is the vector of upload dates.
	X = sort(  unique( STACKED[, "UploadDate"] ) )

	## Y is the vector of upload indicators (1 if upload with positive counts, 0 otherwise).
	Y = rep( 0, times = length(X) )

	## Populate the values of Y.
	for( R in 1:dim(UPLOAD_DATE_vs_NUM_ADDED)[1] )  
		if( !is.na(UPLOAD_DATE_vs_NUM_ADDED[R,2]) )   
			if( UPLOAD_DATE_vs_NUM_ADDED[R,2] != 0 )  Y[ X == UPLOAD_DATE_vs_NUM_ADDED[R,1] ] = 1 		

	BOTTOM_MARGIN = 0.5
	TOP_MARGIN = LEFT_MARGIN = RIGHT_MARGIN = 0.25
	 
	X_MIN = min(X); X_MAX = max(X); Y_MIN = min(Y);  Y_MAX = max(Y);

 	par( mai=c( BOTTOM_MARGIN, LEFT_MARGIN, TOP_MARGIN, RIGHT_MARGIN ) )

	plot( X, Y, 
		  xlim=c(X_MIN,X_MAX), ylim=c(Y_MIN,Y_MAX), 
	      xlab="", ylab="", 
	      main="", type='n',  
	      axes=FALSE, xaxs="i", ...)

	polygon(  c( X_MIN-1, X_MAX+1, X_MAX+1, X_MIN-1 ), 	
			  c( Y_MIN-1, Y_MIN-1, Y_MAX+1, Y_MAX+1 ), 
			  col=rgb(1, .75, .5), border=FALSE ) 
	
	abline( v=(X_MIN-1):(X_MAX+1)+0.5, cex=0.15, col="white" )
	abline( v=X[Y==1], cex=0.50, col="darkblue" )
	

	## x-axis labels.
	NUMBER_OF_LABELS = 10
	JUMP = round( (length(X)/NUMBER_OF_LABELS)/5, 0 ) * 10
	if( JUMP == 0 ) JUMP = 4
	TICK_PLACES = ( 0:NUMBER_OF_LABELS ) * JUMP
	DATE_LABELS = TICK_PLACES
	LABEL_ORIENTATION = 1
	
	if( accrued_data[["start"]][[1]] ) {
		# x-axis labels need to be dates because the argument is TRUE.
		START_DATE = accrued_data[["start"]][[2]]
		DATE_LABELS = as.Date(START_DATE + TICK_PLACES, origin=as.Date("1970-01-01")) 
		LABEL_ORIENTATION = 2
	}
		
	axis(1, at = TICK_PLACES, labels=DATE_LABELS, cex.axis=0.8, las=LABEL_ORIENTATION, font.axis=1) 
	

} 

