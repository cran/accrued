\name{asOf}

\alias{asOf}

\title{Function to recover the current data.}
\description{
	Function to create a vector of current data as of a specified date.
}

\usage{ asOf(x, currentDate) }

\arguments{
  \item{x}{Object of the \code{accrued} class.}
  \item{currentDate}{The current date of interest.}
}

\details{A column matrix of data current as of the specified date is returned. 
The row names are the dates. 
For dates prior to the currentDateNum minus the maximal accrual lag, the final value is used.}

\value{None.}

\author{Julie Eaton and Ian Painter}



\examples{
	data(accruedDataExample)	# simulated accrued data
	dat <- data.accrued(accruedDataExample)	
	currentData <- asOf(dat)
	plot(currentData[,1])
	currentData <- asOf(dat, currentDate=20) # If dates are indexed from 1.
	plot(currentData[,1])

	dat <- data.accrued(accruedDataExample, start="2010-04-20")	
	currentData <- asOf(dat, currentDate="2010-07-12") 
	L = dim(currentData)[1] 
	yMax = max(currentData[,1], na.rm=TRUE) + 100
	num_labels = round(L/4,0)
	y_tick_places = seq(0, yMax, by=round(yMax/num_labels,0))
	x_indices = 1:L
	x_tick_places = x_indices[(0:num_labels)*3]
	x_labels = dimnames(currentData)[[1]][(0:num_labels)*3]
	plot(currentData[,1], axes=FALSE, xlab="", ylab="Current counts", ylim=c(0,yMax))
	abline(h=0)
	abline(v=0)
	axis(1, at=x_tick_places, labels=x_labels, las=2 )
	axis(2, at=y_tick_places, labels=y_tick_places, las=2 )
	
	\dontrun{
	library(animation)
	# Requires a separate application called "ImageMagik"

		NROW = nrow(dat[["data"]])
		saveGIF({
			for( R in 1:NROW ) {
				plot( 	c(0, NROW+1), 
						c(0, max(dat[["data"]], na.rm=TRUE)), 
						type='n', xlab='day', ylab='count' )
				currentValues = asOf(dat, currentDate=R)	
				points(1:R, currentValues)
				lines(1:R, currentValues, col='lightblue')
			}
		}, cmd.fun=system, interval=0.1)
	}

	
	
}


