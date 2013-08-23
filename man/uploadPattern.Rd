\name{uploadPattern}

\alias{uploadPattern}

\title{Function to create an upload pattern plot}

\description{This function creates an upload pattern plot for partially accruing data showing dates at which new data was received for each encounter date.}

\usage{uploadPattern(x, horizontal = TRUE, ...)}

\arguments{
	\item{x}{An object of the \code{accrued} class.}
	\item{horizontal}{logical value; if TRUE a compact horizontal plot is generated, 
		if FALSE a diagonal upload plot is generated.}
	\item{\dots}{Parameters to pass to \code{plot}.}
}

\details{A diagonal upload pattern plot displays for each encounter date (\eqn{x}-axis) whether or not new data was received at each upload date (\eqn{y}-axis). A horizontal upload pattern plot displays for each encounter date (\eqn{x}-axis) whether or not new data was received \eqn{y} days after the encounter date. Note that this function calls the "stackedUploadData" which should only be used on count data.}
 
\value{None.}

\author{Julie Eaton and Ian Painter}

\seealso{
	\code{\link{stackedUploadData}},
 	\code{\link{barcode}}, 
 	\code{\link{plot.accrued}}, 
	\code{\link{plot.summary.accrued}}, 
	\code{\link{lagHistogram}}
}

\examples{
	data(accruedDataExample)			
	dat <- data.accrued(accruedDataExample)	
	uploadPattern(dat)				# Diagonal plot (default)
	dev.new(height = 1.5, width = 10)		# Horizontal plot - best short and wide
	uploadPattern(dat, horizontal = TRUE)				
}


