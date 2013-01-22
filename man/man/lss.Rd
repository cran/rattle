\name{lss}
\alias{lss}
\title{
  List sizes of objects.
}
\description{
  
  List the sizes of objects.
  
}
\usage{
lss(n=10, pos=1, pattern, order.by="Size", decreasing=TRUE, head=TRUE)
}
\arguments{
  
  \item{n}{show the top n largest objects.}
  \item{pos}{which environment.}
  \item{pattern}{objects to match.}
  \item{order.by}{how to order.}
  \item{decreasing}{show in decreasing order.}
  \item{head}{only show the top n.}
}
\value{A data frame.}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{Based on posts by Dirk Eddelbuettel, Petr Pikal and David Hinds.}
