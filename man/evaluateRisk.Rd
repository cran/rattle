\name{evaluateRisk}
\alias{evaluateRisk}
\title{
  Summarise the performance of a data mining model
}
\description{
  
  By taking predicted values, actual values, and measures of the risk
  associated with each case, generate a summary that groups the distinct
  predicted values, calculating the accumulative percentage Caseload,
  Recall, Risk, Precision, and Measure.

}
\usage{
evaluateRisk(predicted, actual, risks)
}
\arguments{

  \item{predicted}{a numeric vector of probabilities (between 0 and 1)
  representing the probability of each entity being a 1.}

  \item{actual}{a numeric vector of classes (0 or 1).}

  \item{risks}{a numeric vector of risk (e.g., dollar amounts)
  associated with each entity that has a acutal of 1.}

}
\references{Package home page: \url{https://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\seealso{\code{\link{plotRisk}}.}
\examples{

## simulate the data that is typical in data mining

## we often have only a small number of positive known case
cases <- 1000
actual <- as.integer(rnorm(cases) > 1)
adjusted <- sum(actual)
nfa <- cases - adjusted

## risks might be dollar values associated adjusted cases
risks <- rep(0, cases)
risks[actual==1] <- round(abs(rnorm(adjusted, 10000, 5000)), 2)

## our models will generated a probability of a case being a 1
predicted <- rep(0.1, cases) 
predicted[actual==1] <- predicted[actual==1] + rnorm(adjusted, 0.3, 0.1)
predicted[actual==0] <- predicted[actual==0] + rnorm(nfa, 0.1, 0.08)
predicted <- signif(predicted)

## call upon evaluateRisk to generate performance summary
ev <- evaluateRisk(predicted, actual, risks)

## have a look at the first few and last few
head(ev)
tail(ev)

## the performance is usually presented as a Risk Chart
## under the CRAN MS/Windows this causes a problem, so don't run for now
\dontrun{plotRisk(ev$Caseload, ev$Precision, ev$Recall, ev$Risk)}
}
\keyword{dplot}
