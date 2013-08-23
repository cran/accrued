\name{plot.summary.accrued}

\alias{plot.summary.accrued}

\title{Plot function for objects of the \code{summary.accrued} class.}

\description{Function to plot summarized partially accrued data objects of the \code{summary.accrued} class.}

\usage{
	\S3method{plot}{summary.accrued}(x, \dots)
}

\arguments{
  \item{x}{Object of class \code{summary.accrued}.}
  \item{\dots}{Parameters to pass to \code{plot}.}
}

\details{
	\code{plot.summary.accrued} generates a plot of the an object of the \code{summary.accrued} class.
}

\value{None.}

\author{Ian Painter and Julie Eaton}

\examples{
	data(accruedDataExample)
	dat <- data.accrued(accruedDataExample)
	summ_dat = summary(dat)		# summary.accrued object
	plot(summ_dat) 				# plot.summary.accrued

}

