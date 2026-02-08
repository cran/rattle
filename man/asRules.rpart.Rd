\name{asRules.rpart}
\alias{asRules.rpart}
\title{
  List the rules corresponding to the rpart decision tree
}
\description{
  
  Display a list of rules for an rpart decision tree.
  
}
\usage{
\method{asRules}{rpart}(model, compact=FALSE, classes=NULL, \dots)
}
\arguments{
  
  \item{model}{an rpart model.}

  \item{compact}{whether to list cateogricals compactly (default FALSE).}

  \item{classes}{which target classes should be listed (default all).}

  \item{\dots}{further arguments passed to or from other methods.}
  
}
\details{

  Traverse a decision tree to generate the equivalent set of rules, one
  rule for each path from the root node to a leaf node.

}
\references{Package home page: \url{https://togaware.com/projects/rattle/}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
\dontrun{asRules.rpart(my.rpart)}
}
\keyword{tree}
