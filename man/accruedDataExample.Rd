\name{accruedDataExample}

\alias{accruedDataExample}

\title{Data set for illustrating accrued data methods}

\docType{data}

\description{Simulated dataset of 12 months of counts accrued over 20 days. This data set is a \code{data.frame}. To use the dataset with any of the functions in the package, the user must first convert it to an \code{accrued} object.}

\usage{data(accruedDataExample)}

\format{A data frame of count data showing the cumulative count data received, with each row representing an encounter date and columns representing the number of days after the encounter. The "final" column represents the final count for each encounter date.}

\details{A simulated dataset designed to represent typical observed behavior.}

\examples{
	data(accruedDataExample)
	plot(data.accrued(accruedDataExample))
	summary(data.accrued(accruedDataExample))
	uploadPattern( data.accrued(accruedDataExample), horizontal=TRUE )
}

\keyword{datasets}
