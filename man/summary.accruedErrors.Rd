\name{summary.accruedErrors}

\alias{summary.accruedErrors}

\title{Summarizes accrual errors}

\description{Default function to summarize errors by accrual lag using quantiles.}

\usage{
	\S3method{summary}{accruedErrors}(object, ...)
}

\arguments{
  \item{object}{Object of the \code{accruedErrors} class.}
  \item{\dots}{Parameters to pass to \code{summary}.}
}

\value{A matrix of error quantiles by lag.}

\author{Julie Eaton and Ian Painter}

\seealso{
	\code{\link{data.accrued}}, 
	\code{\link{accruedErrors}}, 
	\code{\link{plot.accruedErrors}},
	The following function allows the user to specify a vector of quantiles:
	\code{\link{errorQuantileSummary}}
}

\examples{
	data(accruedDataExample)			# simulated accrued data
	testData <- data.accrued(accruedDataExample)	
	errors <- accruedErrors(testData)
	summary(errors)
}


