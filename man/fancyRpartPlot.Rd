\name{fancyRpartPlot}
\alias{fancyRpartPlot}
\title{
  A wrapper for plotting rpart trees using prp
}
\description{
  
  Plots a fancy RPart decision tree using the pretty rpart plotter.
  
}
\usage{
fancyRpartPlot(model, main="", sub, caption, palettes, type=2, ...)
}
\arguments{
  
  \item{model}{an rpart object.}

  \item{main}{title for the plot.}

  \item{sub}{sub title for the plot. The default is a Rattle string with
  date, time and username.}

  \item{caption}{caption for bottom right of plot.}

  \item{palettes}{a list of sequential palettes names. As supported by
  RColorBrewer::brewer.pal the available names are Blues BuGn BuPu GnBu
  Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn
  YlGnBu YlOrBr YlOrRd.}

  \item{type}{the type of plot to generate (2).}

  \item{...}{additional arguments passed on to prp.}

}
\references{Package home page: \url{https://togaware.com/projects/rattle/}}
\author{\email{Graham.Williams@togaware.com}}

\examples{
## Use rpart to build a decision tree.

\dontrun{library(rpart)

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

# Fit the model.

fit <- rpart(form, data=ds[train, vars])

## Plot the model.

fancyRpartPlot(fit)

## Choose different colours.

fancyRpartPlot(fit, palettes=c("Greys", "Oranges"))

## Add a main title to the plot.

fancyRpartPlot(fit, main=target) 

}}
\keyword{hplot}
