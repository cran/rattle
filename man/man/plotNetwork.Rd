\name{plotNetwork}
\alias{plotNetwork}
\title{
  Plot a circular map of network links between entities
}
\description{
  
  Plots a circular map of entities and their relationships. The entities
  are around the edge of the circle with lines linking the entities
  depending on their relationships as represented in the supplied FLOW
  argument. Line widths represent relative magnitude of flows, as do
  line colours, and the font size of a label for an entity represents
  the size of the total flow into that entity. Useful for displaying,
  for example, cash flows between entities.

}
\usage{
plotNetwork(flow)
}
\arguments{
  
  \item{flow}{a square matrix of directional flows.}

}
\details{

  The FLOW is a square matrix that records the directional flow and
  magnitude of the flow from one entity to another.  The flow may
  represent a flow of cash from one company to another company. The
  dimensions of the square matrix are the number of entities represented.

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\examples{
# Create a small sample matrix.
flow <- matrix(c(0, 10000, 0, 0, 20000,
                 1000, 0, 10000, 0, 0,
                 5000, 0, 0, 0, 3000,
                 0, 1000000, 600000, 0, 0,
                 0, 50000, 0, 500000, 0),
               nrow=5, byrow=TRUE)
rownames(flow) <- colnames(flow) <- c("A", "B", "C", "D", "E")
plotNetwork(flow)

# Use data from the network package.
data(flo)
plotNetwork(flo)
}
\keyword{hplot}
