\name{accruedErrors}

\alias{accruedErrors}

\title{Function to calculate error between partially accrued data and final data}

\description{Returns the error between final data values and partially accruing values, by accrual lag.}

\usage{accruedErrors(x, y=NULL, func=NULL)}

\arguments{
  \item{x}{Object of the \code{accrued} class.}
  \item{y}{Optional second object of the \code{accrued} class.}
  \item{func}{Error function to calcuate errors. Default behavior depends on whether one or two objects are included in the call. The first argument is the final value of the indicator and the second argument is the lagged value of the indicator.}
}

\details{
If  only \code{x} is specified, calculates the error between each lag value and the final value for each encounter date. 
If both \code{x} and \code{y} are specified, then the error is calculated for the ratio \code{y}/\code{x} (except in that \eqn{X/0} is set to \code{NA} for all \eqn{X}). The deault error function in only \code{x} is specified is the difference between the lag value and the corresponding final value. The default error function if both are specified is the difference in logs of the two values.
}

\value{An object of the \code{accruedErrors} class which consists of a matrix with columns "EncounterDate", "Lag" and "Error".}
\author{Julie Eaton and Ian Painter}

\seealso{
		\code{\link{data.accrued}}, 
		\code{\link{plot.accruedErrors}}, 
		\code{\link{summary.accruedErrors}
	}
}

\examples{
	data(accruedDataExample)	# simulated accrued data 
	testData <- data.accrued(accruedDataExample)	
	errors <- accruedErrors(testData)
	plot(errors)
	errors <- accruedErrors(testData, func = function(a,b) {sqrt(a) - sqrt(b)} )
	plot(errors)
	
	data(accruedDataILIExample)	# simulated accrued data - ILI counts
	testData2 = data.accrued(accruedDataILIExample)
	errors <- accruedErrors(testData, testData2)
	plot(errors)
}
