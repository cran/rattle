\name{riskchart}
\alias{riskchart}
\title{
  Plot a risk chart
}
\description{
  
  Plots a Rattle Risk Chart using ggplot2. Such a chart has been
  developed in a practical context to present the performance of data
  mining models to clients, plotting a caseload against performance,
  allowing a client to see the tradeoff between coverage and
  performance.
  
}
\usage{
riskchart(data,
          title=NULL,
    show.legend = TRUE,
    optimal = NULL, optimal.label = "", chosen = NULL, chosen.label = "",
    include.baseline = TRUE, dev = "", filename = "", show.knots = NULL,
    show.lift=TRUE, show.precision=TRUE,
    risk.name = "Risk", recall.name = "Recall",
    precision.name = "Precision", thresholds=NULL)
}
\arguments{
  
  \item{data}{The caseloads, precision, recall, risk values for each
    probability cutoff.}
  
  \item{title}{the main title to place at the top of the plot.}
  
  \item{show.legend}{whether to display the legend in the plot.}
  
  \item{optimal}{a caseload (percentage or fraction) that represents an
  optimal performance point which is also plotted. If instead the value
  is \code{TRUE} then the optimal point is identified internally
  (maximum valud for \code{(recall-casload)+(risk-caseload)}) and
  plotted.}

\item{optimal.label}{a string which is added to label the line drawn
  as the optimal point.}

\item{chosen}{a caseload (percentage or fraction) that represents a
  user chosen optimal performance point which is also plotted.}

\item{chosen.label}{a string which is added to label the line drawn as
  the chosen point.}

\item{include.baseline}{if TRUE (the default) then display the
  diagonal baseline.}

\item{dev}{a string which, if supplied, identifies a device type as
  the target for the plot. This might be one of \code{wmf} (for
  generating a Windows Metafile, but only available on MS/Windows),
  \code{pdf}, or \code{png}.}

\item{filename}{a string naming a file. If \code{dev} is not given
  then the filename extension is used to identify the image format as
  one of those recognised by the \code{dev} argument.}

\item{show.knots}{a vector of caseload values at which a vertical line
  should be drawn. These might correspond, for example, to individual
  paths through a decision tree, illustrating the impact of each path on
  the caseload and performance.}

\item{show.lift}{whether to label the right axis with lift.}

\item{show.precision}{whether to show the precision plot.}

\item{risk.name}{a string used within the plot's legend that gives a
  name to the risk. Often the risk is a dollar amount at risk from a
  fraud or from a bank loan point of view, so the default is
  \code{Revenue}.}

\item{recall.name}{a string used within the plot's legend that gives a
  name to the recall. The recall is often the percentage of cases that
  are positive hits, and in practise these might correspond to known
  cases of fraud or reviews where some adjustment to perhaps a incom tax
  return or application for credit had to be made on reviewing the case,
  and so the default is \code{Adjustments}.}

\item{precision.name}{a string used within the plot's legend that gives
  a name to the precision. A common name for precision is \code{Strike
  Rate}, which is the default here.}
  
\item{thresholds}{whether to display scores along the top axis.}
}
\details{

  Caseload is the percentage of the entities in the dataset covered by
  the model at a particular probability cutoff, so that with a cutoff of
  0, all (100\%) of the entities are covered by the model. With a cutoff
  of 1 (0\%) no entities are covered by the model. A diagonal line is
  drawn to represent a baseline random performance. Then the percentage
  of positive cases (the recall) covered for a particular caseload is
  plotted, and optionally a measure of the percentage of the total risk
  that is also covered for a particular caseload may be plotted. Such a
  chart allows a user to select an appropriate tradeoff between caseload
  and performance. The charts are similar to ROC curves. The precision
  (i.e., strike rate) is also plotted.

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\seealso{\code{\link{evaluateRisk}}, \code{\link{genPlotTitleCmd}}.}
\examples{
## this is usually used in the context of the evaluateRisk function
\dontrun{ev <- evaluateRisk(predicted, actual, risk)}

## imitate this output here
ev <- data.frame(Caseload=c(1.0, 0.8, 0.6, 0.4, 0.2, 0),
                 Precision=c(0.15, 0.18, 0.21, 0.25, 0.28, 0.30),
                 Recall=c(1.0, 0.95, 0.80, 0.75, 0.5, 0.0),
                 Risk=c(1.0, 0.98, 0.90, 0.77, 0.30, 0.0))

## plot the Risk Chart
riskchart(ev, chosen=60, chosen.label="Pr=0.45")

## Add a title
eval(parse(text=genPlotTitleCmd("Sample Risk Chart")))
}
\keyword{hplot}
