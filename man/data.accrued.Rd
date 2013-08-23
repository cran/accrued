\name{data.accrued}

\alias{data.accrued}

\title{Data structure for partially accrued data}

\description{This function creates an object of the \code{accrued} class for use in the plot, summary, and error functions in the accrued package.}
\usage{data.accrued(data, start = NULL, final = NULL)}

\arguments{
  \item{data}{A matrix of time series data, with rows as consecutive encounter dates and columns as lag.}
  \item{start}{A list consisting of two elements. The first component is boolean according to whether a start date is specified (defaults to \code{FALSE}). The second component is the start date. If none is specified it defaults to 1.}
  \item{final}{Vector of final counts for each encounter date. If omitted, the final column of \code{data} is used for the final counts.}
}

\details{
	The function converts a matrix of patrically accrued counts into a \code{data.accrued} object. 
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

	dat$start[[1]]		
	# This is FALSE because no start date was specified.

	dat$start[[2]]		
	# This is 1 because no start date was specified.

	dat					# uses print.accrued
	summary(dat)		# uses summary.accrued

	plot(summary(dat)) 	# produces a plot of the summary object
	plot(dat)			# plots time series of differences between lags


	# Convert to data.accrued object, start date now specified.
	dat <- data.accrued(accruedDataExample, start="2012-09-15")	
	dat$start[[1]]		# this is TRUE because a start date was specified
	dat$start[[2]]		# this is the start date

}



