\name{calcInitialDigitDistr}
\alias{calcInitialDigitDistr}
\title{
  Generate a frequency count of the initial digits
}
\description{
  
  In the context of Benford's Law calculate the distribution of the
  frequencies of the first digit of the numbers supplied as the
  argument.

}
\usage{
calcInitialDigitDistr(l, digit=1, len=1,
 sp=c("none", "positive", "negative"))
}
\arguments{
  
  \item{l}{a vector of numbers.}
  \item{digit}{the digit to generate frequencies for.}
  \item{len}{The number of digits.}
  \item{sp}{whether and how to split the digits.}

}
\references{Package home page: \url{https://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\keyword{hplot}
