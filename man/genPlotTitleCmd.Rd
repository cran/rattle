\name{genPlotTitleCmd}
\alias{genPlotTitleCmd}
\title{
  Generate a string to add a title to a plot
}
\description{
  
  Generate a string that is intended to be \code{\link{eval}}'d that
  will add a title and sub-title to a plot. The string is a call to
  \code{\link{title}}, supplying the given arguments,
  \code{\link{paste}}d together, as the main title, and generating a
  sub-title that begins with `Rattle' and continues with the current date
  and time, and finishes with the current user's username. This is used
  internally in Rattle to adorn a plot with relevant information, but
  may be useful outside of Rattle.

}
\usage{
genPlotTitleCmd(..., vector=FALSE)
}
\arguments{
  \item{...}{one or more strings that will be pasted together to form
    the main title.}
  \item{vector}{whether to return a vector as the result.}
}
\references{Package home page: \url{https://togaware.com/projects/rattle/}}
\author{\email{Graham.Williams@togaware.com}}
\seealso{

  \code{\link{eval}}, \code{\link{title}}, \code{\link{plotRisk}}.
}
\examples{
# generate some random plot
plot(rnorm(100))

# generate the string representing the command to add titles
tl <- genPlotTitleCmd("Sample Plot of", "No Particular Importance")

# cause the string to be executed as an R command
eval(parse(text=tl))
}
\keyword{aplot}
