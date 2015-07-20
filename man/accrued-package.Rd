\name{accrued-package}

\alias{accrued-package}

\alias{accrued}

\docType{package}

\title{Data Quality Visualization Tools for Partially Accruing Data}

\description{Package for visualizing data quality of partially accruing data.}

\details{
  \tabular{ll}{
    Package: \tab accrued\cr
    Type: \tab Package\cr
    Version: \tab 1.3.5\cr
    Date: \tab 2015-07-20\cr
    License: GPL-3\cr
  }
}

\author{Julie Eaton (jreaton@uw.edu) and Ian Painter}

\references{
[1] Painter, I., Eaton, J., Olson, D., Revere, D., & Lober, W. (2011). How good is your data. In 2011 ISDS Conference Abstract.  Emerging Health Threats Journal, 4. (http://www.eht-journal.net/index.php/ehtj/article/view/11907)

[2] Painter, I., Eaton, J., Olson, D., Lober, W., & Revere, D. (2011). Visualizing data quality: tools and views. In 2011 ISDS Conference Abstract.  Emerging Health Threats Journal, 4. (http://www.eht-journal.net/index.php/ehtj/article/view/11907)

[3] Lober W., Reeder B., Painter I., Revere D., Bugni P. McReynolds J., Goldov K., Webster E., & Olson D. (2014). Technical Description of the Distribute Project: A Community-based Syndromic Surveillance System Implementation. Online Journal of Public Health Informatics, 5.
}

\keyword{ package }

\examples{
data(accruedDataExample)
testData <- data.accrued(accruedDataExample)
plot(testData)
summary(testData)
plot(summary(testData))
uploadPattern(testData)
laggedTSarray(testData, lags=c(1,3,5,7) )
lagHistogram(testData)
summary(accruedErrors(testData))
plot(accruedErrors(testData))
currentValues = asOf(testData, currentDate=20)
# plot(currentValues)

data(accruedDataILIExample)
testData2 <- data.accrued(accruedDataILIExample)
plot(accruedErrors(testData, testData2))
}
