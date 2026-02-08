\name{rescale.by.group}
\alias{rescale.by.group}
\title{

  Transform a numeric vector by grouping it according to the values of
  the supplied factor and then rescaling within the groups.

}
\description{

  The numeric vector is remapped to integers from 0 to max-1, with any
  missing values mapped to the midpoint. Original idea from Tony
  Nolan. This will eventually be generalised to do the remapping using
  any of the rescaling functions.

}
\usage{
rescale.by.group(x, by=NULL, type = "irank", itop = 100)
}
\arguments{
  
  \item{x}{The numeric vector to rescale.}

  \item{by}{A factor of the same length as x used to define the groups.}

  \item{type}{The type of rescaling to perform.}

  \item{itop}{For an integer remapping this is the number of groups, so
    that the numeric values are maped to the integers from 0 to (max-1).}

}
\details{

  This Rattle support function, which is also useful by itself, provides
  a simple mechanism to rescale a numeric variable. Several rescalings
  are possible. The rescaling is done by first grouping the observations
  according to the by argument.
  
}

\references{Package home page: \url{https://togaware.com/projects/rattle/}}

\author{\email{Graham.Williams@togaware.com}}

\seealso{

  \code{\link{rattle}}.

}
