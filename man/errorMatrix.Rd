\name{errorMatrix}
\alias{errorMatrix}
\title{
  Generate an error matrix from actua and predicted data.
}
\description{
  An error matrix reports the true/false potisitve/negative rates.
}
\usage{
errorMatrix(actual,
                        predicted,
                        percentage=TRUE,
                        digits=ifelse(percentage,1,3),
                        count=FALSE)
}
\arguments{
  
  \item{actual}{a vector of true values.}
  \item{predicted}{a vector of predicted values.}
  \item{percentage}{return percentages.}
  \item{digits}{the number of digits to round results.}
  \item{count}{return counts.}

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}

\examples{
  \dontrun{errorMatrix(model)}
}
