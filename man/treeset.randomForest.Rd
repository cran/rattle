\name{treeset.randomForest}
\alias{treeset.randomForest}
\title{
  Generate a representation of a tree in a Random Forest
}
\description{
  Often we want to view the actual trees built by a random
  forest. Although reviewing all 500 trees might be a bit much, this
  function allows us to at least list them.
}
\usage{
treeset.randomForest(model, n=1, root=1, format="R")
}
\arguments{
  
  \item{model}{a randomForest model.}

  \item{n}{a specific tree to list.}
  
  \item{root}{where to start the stree from, primarily for internal use.}
  
  \item{format}{one of "R", "VB".}
  
}
\references{Package home page: \url{https://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
## Display a treeset for a specific model amongst the 500.
\dontrun{treeset.randomForests(rfmodel, 5)}
}
\keyword{hplot}
