\name{printRandomForests}
\alias{printRandomForests}
\title{
  Print a representation of the Random Forest models to the console
}
\description{
  
  A randomForest model, by default, consists of 500 decision trees. This
  function walks through each tree and generates a set of rules which
  are printed to the console. This takes a considerable amount of time
  and is provided for users to access the actual model, but it is not
  yet used within the Rattle GUI. It may be used to display the output
  of the RF (but it takes longer to generate than the model itself!). Or
  it might only be used on export to PMML or SQL.

}
\usage{
printRandomForests(model, models=NULL, include.class=NULL, format="")
}
\arguments{
  
  \item{model}{a randomForest model.}

  \item{models}{a list of integers limiting the models in MODEL that are
  displayed.}
  
  \item{include.class}{limit the output to the specific class.}

  \item{format}{possible values are "VB".}

}
\references{Package home page: \url{https://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
## Display a ruleset for a specific model amongst the 500.
\dontrun{printRandomForests(rfmodel, 5)}

## Display a ruleset for specific models amongst the 500.
\dontrun{printRandomForests(rfmodel, c(5,10,15))}

## Display a ruleset for each of the 500 models.
\dontrun{printRandomForests(rfmodel)}
}
\keyword{hplot}
