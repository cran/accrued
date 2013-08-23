
## This creates the accrued summary.
summary.accrued = function(object, ...) {

	## Throw an error if the argument is not of the correct class.
	if( class(object) != "accrued" )  stop("ERROR: argument is not an object of the 'data.accrued' class.")
		
	x = object
	lags = ncol(x$data)

	# An anonymous function is created in the 3rd argument of the "apply" function call below.
	# "y" is a generic object (whatever "apply" passes to the function) and returns the proportion
	# of non-missing data. (!is.na(y) returns T (which is then coerced to 1) if data are present and F otherwise)

	## "upload.prop" is created as follows. For each lag, an encounter date is assigned a 1 if data for that encounter date,
	## are uploaded, and a 0 otherwise. 
	## The percentage of such encounter dates is returned for each lag.
	upload.prop = apply( cbind(x$data, x$final), 2, function(y) mean(!is.na(y)) )


	## "mean.prop" is created as follows. For each lag, the proportion of counts for reach upload date, 
	## relative to the final counts, is computed. Then, the mean over encounter dates is taken, 
	## for each lag, and returned.
	mean.prop   = apply( cbind(x$data, x$final), 2, function(y) mean(y/x$final, na.rm=TRUE) )

	## "mean.tot" is created as follows. The mean number of counts for any encounter date is computed, for
	## each lag.
	mean.tot = apply( cbind(x$data, x$final), 2, mean, na.rm = TRUE )
	
	result = data.frame( upload.prop=upload.prop, mean.prop=mean.prop, mean.total=mean.tot)
	row.names(result) = c(0:(lags-1), "final")
	class(result) = 'summary.accrued'
	result
}



## This prints the accrued summary.
print.summary.accrued = function(x, ...) {

	if( class(x) != "summary.accrued" )  stop("ERROR: argument is not an object of the 'summary.accrued' class.")

	upload.prop = x$upload.prop
	mean.prop = x$mean.prop
	mean.tot = x$mean.tot
	
	lags = length(upload.prop)-1
	out = cbind( format(round(upload.prop,2)), format(round(mean.prop, 2)), format(round(mean.tot,1)) )
	out = rbind( c("","",""), out )
	row.names(out) = c(" Lag", paste(rep('', lags), as.character(0:(lags - 1))), " final")
	dimnames(out)[[2]] = c( "Upload Percent", "Proportion", "Mean Count")
	
	cat( paste("\nSummary of accrued data object with", nrow(x$data), "time points.\n\n") )
	print.default( out, print.gap=2, quote=FALSE )
}





# This plots the summary.
plot.summary.accrued = function(x, ...) {
	if( class(x) != "summary.accrued" )  stop("ERROR: argument is not an object of the 'summary.accrued' class.")
	lags = length(x$mean.prop)
	plot( 0:(lags-2), x$mean.prop[-lags], ylim=c(0,1), type='l', xlab='lag', ylab='proportion of counts received', ...)
}




