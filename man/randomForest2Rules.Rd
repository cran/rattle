\name{randomForest2Rules}
\alias{randomForest2Rules}
\title{
  Generate accessible data structure of a randomForest model
}
\description{
  
  A randomForest model, by default, consists of 500 decision trees. This
  function walks through each tree and generates a set of rules. This
  takes a considerable amount of time and is provided for users to
  access the actual model, but it is not yet used within the Rattle
  GUI. It may be used to display the output of the RF (but it takes
  longer to generate than the model itself!). Or it might only be used
  on export to PMML or SQL.

}
\usage{
randomForest2Rules(model, models=NULL)
}
\arguments{
  
  \item{model}{a randomForest model.}

  \item{models}{a list of integers limiting the models in MODEL that are
  converted.}
  
}
\references{Package home page: \url{https://togaware.com/projects/rattle/}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
## Generate a ruleset for a specific model amongst the 500.
\dontrun{randomForest2Rules(rfmodel, 5)}

## Generate a ruleset for specific models amongst the 500.
\dontrun{randomForest2Rules(rfmodel, c(5,10,15))}

## Generate a ruleset for each of the 500 models.
\dontrun{randomForest2Rules(rfmodel)}
}
\keyword{hplot}
