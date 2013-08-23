\name{print.accrued}

\alias{print.accrued}

\title{Print function for objects of the \code{accrued} class.}

\description{Functions to print partially accrued data objects of the \code{accrued} class.}

\usage{
	\S3method{print}{accrued}(x, ...)
}

\arguments{
  \item{x}{Object of the \code{accrued} class.}
  \item{\dots}{Parameters to pass to \code{print.default} or \code{plot}.}
}

\details{
	\code{print.accrued} displays a matrix of counts with columns representing the lag and rows the encounter date.
}

\value{None.}

\author{Ian Painter and Julie Eaton}

\examples{
	data(accruedDataExample)
	dat <- data.accrued(accruedDataExample)

	dat					# print.accrued
}

