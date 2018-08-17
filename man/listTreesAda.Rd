\name{listTreesAda}
\alias{listTreesAda}
\title{
  List trees from an Ada model
}
\description{
  
  Display the textual representation of a selection of Ada trees.
  
}
\usage{
listTreesAda(model, trees=0)
}
\arguments{
  
  \item{model}{an ada model.}

  \item{trees}{The list of trees to list. Use 0 to list all trees.}

}
\details{

  Using rpart's print method display each of the specified trees from an
  Ada model.

}
\references{Package home page: \url{https://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
\dontrun{listTreesAda(ds.ada)}
}
\keyword{hplot}
