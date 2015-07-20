\name{barcode}

\alias{barcode}

\title{Function to create a barcode plot of partially accruing data}

\description{
	Creates a sparkline graph or "barcode" for partially accruing data 
	showing dates on which data are received.
	If data are received containing 0 or negative counts, the bars are gray. Otherwise they are dark blue.
}

\usage{ barcode(x, ...) }

\arguments{
  \item{x}{Object of the \code{accrued} class containing data to be plotted.}
  \item{\dots}{Parameters to pass to \code{plot}.}
}

\details{A plot is produced showing days on which data were received. 
Note that this function calls the \code{stackedUploadData} which should only be used on count data.}

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
	data(accruedDataExample)	# simulated accrued data
	testData <- data.accrued(accruedDataExample)	
	barcode(testData)		
}


