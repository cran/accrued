\name{laggedTSarray}

\alias{laggedTSarray}

\title{Function to create an array of time series across accrual lags using the running median}

\description{Creates an array of time series graphs--one for each accrual lag--and displays a running median and MAD-based bounds on each graph.}

\usage{laggedTSarray(x, daysOfHistory, lags, ...)}

\arguments{
	\item{x}{Object of the \code{accrued} class containing data to be plotted.}
  	\item{daysOfHistory}{An integer greater or equal to 1. If \code{NULL}, then defaults to 30. 
	    The number of days of previous data used to calculate the running median.}
	\item{lags}{A vector of nonnegative integers specifying which lagged time series will be plotted. If NULL, all lags are used.}
	\item{\dots}{Parameters to pass to \code{plot}.}
}

\details{ An array of graphs is produced, with the \eqn{i}th graph showing for each encounter date the cumulative counts received \eqn{i-1} days after the encounter date, with the final graph showing the final counts. A running median and error bars are disiplayed by default. Error bars are calculated as \eqn{\pm (2)(MAD)} (median absolute deviations) from the running median, calculated using the same running window.}

\value{None.}

\author{Julie Eaton and Ian Painter}

\seealso{
	\code{\link{data.accrued}},
	\code{\link{stackedUploadData}},
 	\code{\link{uploadPattern}}, 
 	\code{\link{plot.accrued}}, 
	\code{\link{plot.summary.accrued}}, 
	\code{\link{lagHistogram}},
	\code{\link{asOf}}
}



\examples{
	data(accruedDataExample)			# simulated accrued data
	dat <- data.accrued(accruedDataExample)	# Convert to data.accrued object
	laggedTSarray(dat)
}
