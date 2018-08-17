\name{drawTreesAda}
\alias{drawTreesAda}
\title{
  Draw trees from an Ada model
}
\description{
  
  Using the Rattle drawTreeNodes, draw a selection of Ada trees.
  
}
\usage{
drawTreesAda(model, trees=0, title="")
}
\arguments{
  
  \item{model}{an ada model.}

  \item{trees}{The list of trees to draw. Use 0 to draw all trees.}

  \item{title}{An option title to add.}

}
\details{

  Using Rattle's drawTreeNodes underneath, a plot for each of the
  specified trees from an Ada model will be displayed.

}
\references{Package home page: \url{https://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
\dontrun{drawTreesAda(ds.ada)}
}
\keyword{hplot}
