\name{rattle}
\alias{rattle}
\alias{crs}
\alias{crv}
\title{Display the Rattle User Interface}
\description{

  DEPRECATED: The Rattle user interface used the RGtk2 package to
  present an intuitive point and click interface for data mining,
  extensively building on the excellent collection of R packages by very
  many authors for data manipulation, exploration, analysis, and
  evaluation. If you have RGtk2 installed then this function will still
  be useful. Otherwise visithttps://togaware.com/projects/rattle// for a modern
  re-implementation of the rattle UI utilising much of the same
  underlying R code for the data analyses.

}
\usage{
rattle(csvname=NULL, dataset=NULL, useGtkBuilder=TRUE)
}
\arguments{

  \item{csvname}{the optional name of a CSV file to load into Rattle on
    startup.}

  \item{dataset}{The optional name as a character string of a dataset to
    load into Rattle on startup.}

  \item{useGtkBuilder}{if not supplied then automatically determine whether to
    use the new GtkBuilder rather than the deprecated libglade. A user
    can override the heuristic choice with TRUE or FALSE.}
}
\details{

  Refer to the Rattle home page in the URL below for a growing reference
  manual for using Rattle.

  Whilst the underlying functionality of Rattle is built upon a vast
  collection of other R packages, Rattle itself provides a collection of
  utility functions used within Rattle. These are made available through
  loading the rattle package into your R library. The See Also section
  lists these utility functions that may be useful outside of Rattle.

  Rattle can initialise some options using a .Rattle file if the folder
  in which Rattle is started. The currently supported options are
  .RATTLE.DATA, .RATTLE.SCORE.IN, and .RATTLE.SCORE.OUT.

  If the environment variable RATTLE_DATA is defined then that is set
  as the default CSV file name to load. Otherwise, if .RATTLE.DATA is
  defined then that will be used as the CSV file to load. Otherwise, if
  csvname is provided then that will be used.

  Two environments are exported by Rattle, capturing the current rattle
  state (crs) and the current rattle variables (crv).

}

\references{Package home page: \url{https://togaware.com/projects/rattle/}}

\author{\email{Graham.Williams@togaware.com}}

\seealso{

  \code{\link{evaluateRisk}}, \code{\link{genPlotTitleCmd}},
  \code{\link{plotRisk}}.

}
\examples{
# You can start rattle with a path to a csv file to pre-specify the
# dataset. You then need to click Execute to load the data.

\dontrun{rattle(system.file("csv", "weather.csv", package = "rattle"))}

}
\keyword{environment}
