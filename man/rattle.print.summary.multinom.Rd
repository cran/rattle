\name{rattle.print.summary.multinom}
\alias{rattle.print.summary.multinom}
\title{
  Print information about a multinomial model
}
\description{
  
  Displays a textual reveiw of the performance of a multinom model.
  
}
\usage{
rattle.print.summary.multinom(x, digits = x$digits, ...)
}
\arguments{
  
  \item{x}{An rpart object.}

  \item{digits}{Number of digist to print for numbers.}

  \item{...}{Other arguments.}
  
}
\details{

  Print a summary of a multinom model. This is sipmly a modification of
  the print.summary.multinom function to add the number of entities!

}
\references{Package home page: \url{https://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}

