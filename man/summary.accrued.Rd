\name{summary.accrued}

\alias{summary.accrued}

\title{Print and summary functions for objects of the \code{accrued} class.}

\description{Functions to display and summarize (as text or as a plot) partially accrued data objects of the \code{accrued} class.}

\usage{
	\S3method{summary}{accrued}(object, \dots)
}

\arguments{
  \item{object}{Object of the \code{accrued} class.}
  \item{\dots}{Parameters to pass to \code{print.default} or \code{plot}.}
}

\details{
	\code{summary.accrued} creates a summary object (of class \code{summary.accrued}) of the counts consisting of the percentage of days on 
	  which an upload was recevied for each lag, the mean proportion of counts received by each lag, 
 	  and the mean counts received by each lag.
}

\value{
	\code{summary.accrued} invisibly returns an object of the \code{summary.accrued} class, consisting of a list with the following components:
	\item{upload_prop}{Vector containing the percentage of days on which an upload was recevied for each lag}
	\item{mean_prop}{Vector containing the mean proportion of counts received by each lag}
	\item{mean_total}{Vector containing the mean counts received by each lag}
}

\author{Ian Painter and Julie Eaton}

\examples{
	data(accruedDataExample)
	dat <- data.accrued(accruedDataExample)
	summ_dat = summary(dat)		# summary.accrued object
}

