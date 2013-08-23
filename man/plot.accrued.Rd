\name{plot.accrued}

\alias{plot.accrued}

\title{Plot function for accrued data.}

\description{Creates a stacklag difference plots of accrued data.}

\usage{ \S3method{plot}{accrued}(x, \dots)}

\arguments{  
	\item{x}{Object of the \code{accrued} class containing data to be plotted.}
	\item{\dots}{Parameters to pass to \code{plot}.}
}

\details{This is only appropriate for count data. This plots differences in counts between successive upload dates. It is the default plot for data of the \code{accrued} class.}

\value{None.}

\author{Ian Painter and Julie Eaton}

\seealso{
	\code{\link{data.accrued}}, 
	\code{\link{summary.accrued}}, 
	\code{\link{plot.summary.accrued}},
	Many other plot methods are available for accrued data: 
	\code{\link{laggedTSarray}},  
	\code{\link{lagHistogram}}
}

\examples{
	data(accruedDataExample)
	plot(data.accrued(accruedDataExample))
}
