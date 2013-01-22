\name{drawTreeNodes}
\alias{drawTreeNodes}
\title{
  Draw nodes of a decision tree
}
\description{
  
  Draw the nodes of a decision tree
  
}
\usage{
drawTreeNodes(tree, cex = par("cex"), pch = par("pch"),
                           size = 4 * cex, col = NULL, nodeinfo = FALSE,
                           units = "", cases = "obs", 
                           digits = getOption("digits"),
                           decimals = 2,
                           print.levels = TRUE, new = TRUE) 
}
\arguments{
  
  \item{tree}{an rpart decision tree.}

  \item{cex}{.}

  \item{pch}{.}

  \item{size}{.}

  \item{col}{.}

  \item{nodeinfo}{.}

  \item{units}{.}

  \item{cases}{.}

  \item{digits}{.}

  \item{decimals}{the number of decimal digits to include in numeric
    split nodes.}

  \item{print.levels}{.}

  \item{new}{.}

}
\details{

  A variation of draw.tree from the maptree package.

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
## this is usually used in the context of the plotRisk function
\dontrun{drawTreeNodes(ds.rp)}
}
\keyword{hplot}
