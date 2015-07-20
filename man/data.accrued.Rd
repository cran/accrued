\name{data.accrued}

\alias{data.accrued}

\title{Data structure for partially accruing data}

\description{Creates an object of the \code{accrued} class for use in the plot, summary, and error functions in the accrued package.}
\usage{data.accrued(data, start = NULL, final = NULL)}

\arguments{
  \item{data}{A matrix of time series data, with rows as consecutive encounter dates and columns as accrual lag.}
  \item{start}{The start date. If none is specified it defaults to 1.}
  \item{final}{Vector of final counts for each encounter date. If omitted, the final column of \code{data} is used for the final counts.}
}

\details{
	The function converts a matrix of patrically accrued counts into an \code{accrued} object. 
	The value at the \eqn{i}th row and \eqn{j}th column of \code{data} represent the count for encounter
	date \eqn{i} as known \eqn{j-1} days after the encounter date. If \code{final} is not specified, 
	the last column of \code{data} is used as the final count. 

	Summary and plot methods exist for objects of this class. Several additional graph types are available, including \code{barcode}, \code{uploadPattern},
	\code{laggedTSarray} and \code{lagHistogram}.
}

\value{
	\code{data.accrued} returns an object of the \code{accrued} class.

An object of the \code{accrued} class is a list containing the following components:

  \item{final }{Vector of final counts}
  \item{data  }{Matrix of counts. The value at the \eqn{i}th row and \eqn{j}th column represent the count for encounter date 
  \eqn{i} as known \eqn{j-1} days after the encounter date.}
  \item{start }{List of boolean start variable and start date}

  Print, summary and plot methods exist for objects of the \code{accrued} class.
}

\author{Julie Eaton and Ian Painter}

\seealso{
	\code{\link{print.accrued}}, 
	\code{\link{plot.accrued}},
	\code{\link{summary.accrued}}, 
	\code{\link{plot.summary.accrued}}
}

\examples{
	
	# simulated accrued data
	data(accruedDataExample)				

	# Convert to data.accrued object
	dat <- data.accrued(accruedDataExample)	

	dat$start
	# This is 1 because no start date was specified.

	dat					# uses print.accrued
	summary(dat)		# uses summary.accrued

	plot(summary(dat)) 	# produces a plot of the summary object
	plot(dat)			# plots time series of differences between lags


	# Convert to data.accrued object, start date now specified.
	dat <- data.accrued(accruedDataExample, start="2012-09-15")	

}



