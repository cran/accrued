\name{lagHistogram}

\alias{lagHistogram}

\title{Function to create a a summary histogram of completeness for each lag.}

\description{This function creates a column of histograms, the \eqn{i}th histogram showing the distribution of 
completeness of data \eqn{i-1} days after the encounter date.}

\usage{lagHistogram(x, ...)}

\arguments{
  \item{x}{Object of the \code{accrued} class containing data to be graphed.}
  \item{\dots}{Parameters to pass to \code{plot}.}
}

\details{This function shows a complete summary of the completeness of the data by lag. The \eqn{i}th row of the column shows a histogram of the completion percent at \eqn{i-1} days after the encounter date (so that the \eqn{x}-axis represents the completeness proportion in blocks of 0.1). Note that this function should only be used on count data.}

\value{None.}

\author{Julie Eaton and Ian Painter}

\seealso{
	\code{\link{plot.summary.accrued}} displays the mean completion rate for each lag. 
}

\examples{	
	library(grid) 	
	data(accruedDataExample)			
	dat <- data.accrued(accruedDataExample)	
	lagHistogram(dat)
}
