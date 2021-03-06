\name{errorQuantileSummary}

\alias{errorQuantileSummary}

\title{Summarizes accrual errors}

\description{Summarizes accrual errors by accrual lag using quantiles where the user can specify quantiles.}

\usage{
	errorQuantileSummary(x, quantiles = c(0.1, 0.5, 0.9), \dots)
}

\arguments{
  \item{x}{Object of the \code{accruedErrors} class.}
  \item{quantiles}{A vector of quantiles to use as a summary.}
  \item{\dots}{Parameters to pass to \code{summary}.}
}

\value{A matrix of error quantiles by lag.}

\author{Julie Eaton and Ian Painter}

\seealso{
	\code{\link{data.accrued}}, 
	\code{\link{accruedErrors}}, 
	\code{\link{plot.accruedErrors}}
}

\examples{
	data(accruedDataExample)			# simulated accrued data
	dat <- data.accrued(accruedDataExample)	
	errors <- accruedErrors(dat)
	errorQuantileSummary(errors, quantiles=seq(0.1,0.9,by=0.1) )
}


