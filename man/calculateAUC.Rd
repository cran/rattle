\name{calculateAUC}
\alias{calculateAUC}
\title{
  Determine area under a curve (e.g. a risk or recall curve) of a risk chart
}
\description{
  
  Given the evaluation returned by evaluateRisk, for example, calculate
  the area under the risk or recall curves, to use as a metric to
  compare the performance of a model.
  
}
\usage{
calculateAUC(x, y)
}
\arguments{
  
  \item{x}{a vector of values for the x points.}
  \item{y}{a vector of values for the y points.}

}
\details{

  The area is returned.

}
\references{Package home page: \url{https://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\seealso{\code{\link{evaluateRisk}}.}
\examples{
## this is usually used in the context of the evaluateRisk function
\dontrun{ev <- evaluateRisk(predicted, actual, risk)}

## imitate this output here
ev <- data.frame(Caseload=c(1.0, 0.8, 0.6, 0.4, 0.2, 0),
                 Precision=c(0.15, 0.18, 0.21, 0.25, 0.28, 0.30),
                 Recall=c(1.0, 0.95, 0.80, 0.75, 0.5, 0.0),
                 Risk=c(1.0, 0.98, 0.90, 0.77, 0.30, 0.0))

## Calculate the areas unde the Risk and the Recall curves.
calculateAUC(ev$Caseload, ev$Risk)
calculateAUC(ev$Caseload, ev$Recall)

}
\keyword{hplot}
