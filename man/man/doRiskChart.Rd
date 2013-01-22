\name{doRiskChart}
\alias{doRiskChart}
\title{
  A convinent wrapper for plotRisk
}
\description{
  
  Plots a Rattle Risk Chart wrapping up evaluateRisk and plotRisk.
  
}
\usage{
doRiskChart(pr, data, test, target, risk, main)

}
\arguments{
  
  \item{pr}{a vector of predicted values corresponding to the rows of
    data.}

  \item{data}{the data frame on which the predictions were made.}

  \item{test}{the rows of the data frame on which the predictions were made.}

  \item{target}{the name of the target column in the data frame.}

  \item{risk}{the name of the risk column in the data frame.}

  \item{main}{title for the plot.}
  
}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\seealso{\code{\link{plotRisk}}, \code{\link{evaluateRisk}}.}
\keyword{hplot}
