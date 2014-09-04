\name{stackedUploadData}

\alias{stackedUploadData}

\title{Subfunction used by the upload pattern and barcode plot functions}

\description{This subfunction organizes the accrued data into a matrix that includes accrual lag as a variable.}

\usage{stackedUploadData(x)}

\arguments{
	\item{x}{Object of the \code{accrued} class.}
}

\details{This should only be used on accrued data that are counts. No error will be thrown in the case of non-count data, but the results will not make sense.}

\value{A data frame with the columns consisting of event date (encounter date), record date (upload date), accrual lag (lag), counts (time series value), number added.} 

\author{Julie Eaton and Ian Painter}

\seealso{
	\code{\link{data.accrued}}, 
	\code{\link{uploadPattern}}, 
	\code{\link{barcode}}
}

