\name{plot.accruedErrors}

\alias{plot.accruedErrors}

\title{Function to plot errors in accrued data}

\description{Plots errors in accrued data, optionally with quantiles of those errors superimposed.}

\usage{
\S3method{plot}{accruedErrors}(x, withSmoothing = FALSE, quantiles = c(0.1, 0.5, 0.9), 
  quantileColors = switch(1+is.null(quantiles), rainbow(length(quantiles)), NULL), \dots)
}

\arguments{  
	\item{x}{Object of the \code{accruedErrors} class.}
	\item{withSmoothing}{Logical. If \code{TRUE} the the quantiles are smoothed prior to plotting.}
	\item{quantiles}{A vector of quantiles to be plotted.}
	\item{quantileColors}{Colors to use for quantiles.}
	\item{\dots}{Parameters to pass to \code{plot}.}
}

\details{
	Produces a scatter plot of errors for each lag value, with the \eqn{x}-axis representing lag and the \eqn{y}-axis the error values. 
	The \eqn{x}-value are jittered prior to plotting.
	If quantiles is specified, the corresponding quantiles of the errors will be superimposed on the plot.	
}

\value{None.}

\author{Julie Eaton and Ian Painter}

\seealso{
	\code{\link{accruedErrors}}, 
	\code{\link{data.accrued}}, 
	\code{\link{summary.accruedErrors}}
}

\examples{
	data(accruedDataExample)			# simulated accrued data
	dat <- data.accrued(accruedDataExample)	
	errors <- accruedErrors(dat)
	plot(errors)
}
