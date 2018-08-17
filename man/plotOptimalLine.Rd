\name{plotOptimalLine}
\alias{plotOptimalLine}
\title{
  Plot three lines on a risk chart, one vertical and two horizontal
}
\description{
  
  Plots a a vertical line at x up to max of y1 and y2, then horizontal
  from this line at y1 and y2. Intended for plotting on a plotRisk.
  
}
\usage{
plotOptimalLine(x, y1, y2, pr = NULL, colour = "plum", label = NULL)
}
\arguments{
  
  \item{x}{location of vertical line.}

  \item{y1}{location of one horizontal line.}

  \item{y2}{location of other horizontal line.}

  \item{pr}{Aprint a percentage at this point.}

  \item{colour}{of the line.}

  \item{label}{at bottom of line.}

}
\details{

  Intended to plot an optimal line on a Risk Chart as plotted by
  plotRisk.

}
\references{Package home page: \url{https://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\seealso{\code{\link{plotRisk}}.}
\examples{
## this is usually used in the context of the plotRisk function
\dontrun{ev <- evaluateRisk(predicted, actual, risk)}

## imitate this output here
ev <- NULL
ev$Caseload  <- c(1.0, 0.8, 0.6, 0.4, 0.2, 0)
ev$Precision <- c(0.15, 0.18, 0.21, 0.25, 0.28, 0.30)
ev$Recall    <- c(1.0, 0.95, 0.80, 0.75, 0.5, 0.0)
ev$Risk      <- c(1.0, 0.98, 0.90, 0.77, 0.30, 0.0)

## plot the Risk Chart
plotRisk(ev$Caseload, ev$Precision, ev$Recall, ev$Risk,
         chosen=60, chosen.label="Pr=0.45")

## plot the optimal point
plotOptimalLine(40, 77, 75, colour="maroon")

}
\keyword{hplot}
