\name{barcode}

\alias{barcode}

\title{Function to create a barcode plot of partially accrued data}

\description{
	Function to create a simple barcode sparkline graph for partially accrued data 
	showing dates on which counts are received.
	If data were received containing 0 or negative counts, bars are colored in gray. Otherwise they are dark blue.
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
	\code{\link{lagHistogram}}
}

\examples{
	data(accruedDataExample)	# simulated accrued data
	dat <- data.accrued(accruedDataExample)	
	barcode(dat)		
}


