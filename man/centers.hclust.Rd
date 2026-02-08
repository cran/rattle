\name{centers.hclust}
\alias{centers.hclust}
\title{
  List Cluster Centers for a Hierarchical Cluster
}
\description{
  
  Generate a matrix of centers from a hierarchical cluster.
  
}
\usage{
centers.hclust(x, object, nclust=10, use.median=FALSE)
}
\arguments{
  
  \item{x}{The data used to build the cluster.}

  \item{object}{A hclust object.}

  \item{nclust}{Number of clusters.}

  \item{use.median}{Use meadion instead of mean.}

}
\details{

  For the specified number of clusters, cut the hierarchical cluster
  appropriately to that number of clusters, and return the mean (or
  median) of each resulting cluster.

}
\references{Package home page: \url{https://togaware.com/projects/rattle/}}
\author{Daniele Medri and \email{Graham.Williams@togaware.com}}
\keyword{cluster}
