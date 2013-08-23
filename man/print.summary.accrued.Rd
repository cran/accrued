\name{print.summary.accrued}

\alias{print.summary.accrued}

\title{Print function for objects of the \code{summary.accrued} class.}

\description{Functions to display as text summarized partially accrued data objects of the \code{summary.accrued} class.}

\usage{
	\S3method{print}{summary.accrued}(x, \dots)
}

\arguments{
  \item{x}{Object of class \code{summary.data.accrued}.}
  \item{\dots}{Parameters to pass to \code{print}.}
}

\details{
	\code{print.summary.accrued} prints to the screen the contents of a \code{summary.accrued} object, namely, the counts consisting of the percentage of days on 
	  which an upload was recevied for each lag, the mean proportion of counts received by each lag, 
 	  and the mean counts received by each lag.
}

\value{None.}

\author{Ian Painter and Julie Eaton}

\examples{
	data(accruedDataExample)
	dat <- data.accrued(accruedDataExample)

	summ_dat = summary(dat)		# summary.accrued object
	summ_dat			# prints the object
}

