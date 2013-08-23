
accruedErrors = function( x,  y=NULL,  func=NULL ) {

	if( class(x) != "accrued" )  stop("ERROR: first argument is not an object of the 'data.accrued' class.")

	NUMBER_OF_DATA_SETS = 0
	if( !is.null(x) ) {
		NUMBER_OF_DATA_SETS	= 1
		if( !is.null(y) ) {
				NUMBER_OF_DATA_SETS	= 2
				if( class(y) != "accrued")  stop("ERROR: second argument is not an object of the 'data.accrued' class.")
			} 
	}

	if( NUMBER_OF_DATA_SETS == 0 ) stop("ERROR: first argument is null.")

	accrued_data = x
	accrued_data2 = y

	##########################################################################################
	## Load the subroutines.
	##########################################################################################

	## Create the count ratio function.	
	createRatio = function( dat1, dat2, numDat ) {

		accrued_ratio = NULL

		if( numDat == 2 ) {

			## First set the final ratios.
			denominator = dat1[["final"]]
			zero_indices = which( denominator == 0)
			length(zero_indices)
			if( length(zero_indices) > 0 ) {
				NA_vec = rep(NA, times=length(zero_indices))
				denominator[zero_indices] = NA_vec
			}
			final_ratios = dat2[["final"]]/denominator

			## Then set the lagged ratios.
			denom = as.matrix(dat1[["data"]])
			numer = as.matrix(dat2[["data"]])
			nr = dim(denom)[[1]]
			nc = dim(denom)[[2]]
			zero_indices = which(denom == 0 )
			NA_vec = rep(NA, times=length(zero_indices))
			temp_denom = as.vector(denom)
			temp_denom[zero_indices] = NA_vec
			denom_new = matrix(temp_denom, nrow=nr, ncol=nc)
			ratios = numer/denom_new

			accrued_ratio = data.accrued(data=ratios, start=NULL, final=final_ratios)
		}
 
	 	class(accrued_ratio) = "accrued"
		accrued_ratio

	} # END  'createRatio' definition


	## Function to create the errors.
	errorCreation = function( dat1, func=func, numDat ) {

		dat = dat1[["data"]]
		final = dat1[["final"]]
		NCOL = ncol(dat)
		NROW = nrow(dat)
		ROWNAMES = dimnames(dat)[[1]]
		COLNAMES = dimnames(dat)[[2]]
		ERRORS = matrix(data=NA, nrow=NROW, ncol=NCOL, dimnames=list(ROWNAMES, COLNAMES) )

		errorFunction = func
		if( is.null(func) ) {
			errorFunction = function(x,y) { return(x-y) }
			if( numDat == 2 ) {
				errorFunction = function(x,y) {
					if( length(x) != length(y) ) stop("ERROR: in errorFunction--arguments must be of the same length.")
					errors=rep(NA, times=length(x))
					product = y*x
					indices = which(product > 0.000001 )
					errors[indices] = log( y[indices]/x[indices])	
					errors
				} # END 'errorFunction' definition
			} # END else if( numDat == 2 )
		} # END if( is.null(func) )

		for( L in 1:NCOL ) ERRORS[,L] = errorFunction( final , dat[,L] )

		ERRORS

	} # END  'errorCreation' definition
	
	
	## Function to stack the errors.
	errorStacking = function( ERRORS ) {

		MAX_LAGS = ncol(ERRORS) - 1
		COLNAMES = c("EncounterDate","Lag","Error")
		NCOL = length(COLNAMES)
		NROW = nrow(ERRORS)* ncol(ERRORS)
		ROWNAMES = 1:NROW
		STACKED = matrix( NA, nrow=NROW, ncol=NCOL, dimnames=list(ROWNAMES, COLNAMES) )

		## Populate encounter dates.
		STACKED[,"EncounterDate"] = rep( 1:nrow(ERRORS), times=ncol(ERRORS) )
	
		## Populate lags.
		TEMP_LAG = NULL
		for( L in 0:MAX_LAGS ) TEMP_LAG = c( TEMP_LAG, rep(L, times=nrow(ERRORS) ) ) 
		STACKED[,"Lag"] = TEMP_LAG

		## Populate the "Error" column.
		STACKED[,"Error"] = as.vector(ERRORS)	
		
		return(STACKED)
		
	} # END 'errorStacking' function
	  
	
	##########################################################################################
	## Now generate the errors. ##
	##########################################################################################
	
	## Carry out the calculations.
	
	ERRORS = NULL

	temp = accrued_data
	if( NUMBER_OF_DATA_SETS == 2 ) temp = createRatio(dat1=accrued_data, dat2=accrued_data2, numDat=NUMBER_OF_DATA_SETS)
	ERRORS =  errorCreation( dat1=temp, func=func, numDat=NUMBER_OF_DATA_SETS ) 
	STACKED = errorStacking( ERRORS )

	class(STACKED) = "accruedErrors"
	STACKED

}
	
	
	
