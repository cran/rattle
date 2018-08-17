\name{riskchart}
\alias{riskchart}
\title{
  Plot a risk chart
}
\description{
  
  Plots a Rattle Risk Chart for binary classification models using
  ggplot2. Such a chart has been developed in a practical context to
  present the performance of data mining models to clients, plotting a
  caseload against performance, allowing a client to see the tradeoff
  between coverage and performance.
  
}
\usage{
riskchart(pr,
          ac,
          ri               = NULL,
          title            = "Risk Chart",
          title.size       = 10,
          subtitle         = NULL,
          caption          = TRUE,
          show.legend      = TRUE,
          optimal          = NULL,
          optimal.label    = "",
          chosen           = NULL,
          chosen.label     = "",
          include.baseline = TRUE,
          dev              = "",
          filename         = "",
          show.knots       = NULL,
          show.lift        = TRUE,
          show.precision   = TRUE,
          show.maximal     = TRUE,
          risk.name        = "Risk",
          recall.name      = "Recall",
          precision.name   = "Precision",
          thresholds       = NULL,
          legend.horiz     = TRUE)
}
\arguments{
  
  \item{pr}{The predicted class for each observation.}

  \item{ac}{The actual class for each observation.}

  \item{ri}{The risk class for each observation.}

  \item{title}{the main title to place at the top of the plot.}
  
  \item{title.size}{font size for the main title.}

  \item{subtitle}{subtitle under the main title.}

  \item{caption}{caption for the bottom right of plot.}
  
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

\item{show.maximal}{whether to show the maximal performance line.}

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

\item{legend.horiz}{whether to display a horizontal legend.}
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
\references{Package home page: \url{https://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
\seealso{\code{\link{evaluateRisk}}, \code{\link{genPlotTitleCmd}}.}
\examples{
\dontrun{

## Use rpart to build a decision tree.

library(rpart)

## Set up the data for modelling.

set.seed(42)
ds     <- weather
target <- "RainTomorrow"
risk   <- "RISK_MM"
ignore <- c("Date", "Location", risk)
vars   <- setdiff(names(ds), ignore)
nobs   <- nrow(ds)
form   <- formula(paste(target, "~ ."))
train  <- sample(nobs, 0.7*nobs)
test   <- setdiff(seq_len(nobs), train)
actual <- ds[test, target]
risks  <- ds[test, risk]

# Build the model.

model <- rpart(form, data=ds[train, vars])

## Obtain predictions.

predicted <- predict(model, ds[test, vars], type="prob")[,2]

## Plot the Risk Chart.

riskchart(predicted, actual, risks)
}
}
\keyword{hplot}
