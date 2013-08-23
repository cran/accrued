\name{accrued-package}

\alias{accrued-package}

\alias{accrued}

\docType{package}

\title{accrued package for data quality visualization}

\description{Package for visualizing data quality of partially accruing time series.}

\details{
  \tabular{ll}{
    Package: \tab accrued\cr
    Type: \tab Package\cr
    Version: \tab 1.0\cr
    Date: \tab 2013-07-03\cr
    License: GPL-3\cr
  }
}

\author{Julie Eaton (jreaton@uw.edu) and Ian Painter }

\references{
[1] Painter, I., Eaton, J., Olson, D., Revere, D., & Lober, W. (2011). How good is your data. In 2011 ISDS Conference Abstract.  Emerging Health Threats Journal, 4. (http://www.eht-journal.net/index.php/ehtj/article/view/11907)

[2] Painter, I., Eaton, J., Olson, D., Lober, W., & Revere, D. (2011). Visualizing data quality: tools and views. In 2011 ISDS Conference Abstract.  Emerging Health Threats Journal, 4. (http://www.eht-journal.net/index.php/ehtj/article/view/11907)

Others are forthcoming.
}

\keyword{ package }

\examples{
data(accruedDataExample)
testData = data.accrued(accruedDataExample)
plot(testData)
summary(testData)
plot(summary(testData))
uploadPattern(testData)
laggedTSarray(testData, lags=c(1,3,5,7,14) )
lagHistogram(testData)
accruedErrors(testData)
summary(accruedErrors(testData))
plot(accruedErrors(testData))
}
