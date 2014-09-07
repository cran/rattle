\name{fancyRpartPlot}
\alias{fancyRpartPlot}
\title{
  A wrapper for plotting rpart trees using prp
}
\description{
  
  Plots a fancy RPart decision tree using the pretty rpart plotter.
  
}
\usage{
fancyRpartPlot(model, main="", sub, ...)
}
\arguments{
  
  \item{model}{an rpart object.}

  \item{main}{title for the plot.}

  \item{sub}{sub title for the plot. The default is a Rattle string with
  date, time and username.}

  \item{...}{additional arguments passed on to prp.}

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}

\examples{
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

## Plot the model.

fancyRpartPlot(model)
}
\keyword{hplot}
