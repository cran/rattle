\name{plotBenfordsLaw}
\alias{plotBenfordsLaw}
\title{
  Plot a chart comparing Benford's Law with a supplied numeric vertor
}
\description{
  
  Plots a barchart of Benford's Law and the distribution of the
  frequencies of the first digit of the numbers supplied as the
  argument.

}
\usage{
plotBenfordsLaw(l)
}
\arguments{
  
  \item{l}{a vector of numbers to compare to Benford's Law.}

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
# A simple example using the audit data from Rattle.
data(audit)
plotBenfordsLaw(audit$Income)
}
\keyword{hplot}
