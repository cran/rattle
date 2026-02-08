\name{wine}
\docType{data}
\alias{wine}
\title{The wine dataset from the UCI Machine Learning Repository.}
\description{

  The \code{wine} dataset contains the results of a chemical analysis of
  wines grown in a specific area of Italy. Three types of wine are
  represented in the 178 samples, with the results of 13 chemical
  analyses recorded for each sample.  The \code{Type} variable has been
  transformed into a categoric variable.

  The data contains no missing values and consits of only numeric data,
  with a three class target variable (\code{Type}) for classification.

}
\usage{wine}
\format{

  A data frame containing 178 observations of 13 variables.

  \describe{

    \item{\code{Type}}{ The type of wine, into one of three classes, 1
    (59 obs), 2(71 obs), and 3 (48 obs).}

    \item{\code{Alcohol}}{Alcohol}

    \item{\code{Malic}}{Malic acid}

    \item{\code{Ash}}{Ash}

    \item{\code{Alcalinity}}{Alcalinity of ash}

    \item{\code{Magnesium}}{Magnesium}

    \item{\code{Phenols}}{Total phenols}

    \item{\code{Flavanoids}}{Flavanoids}

    \item{\code{Nonflavanoids}}{Nonflavanoid phenols}

    \item{\code{Proanthocyanins}}{Proanthocyanins}

    \item{\code{Color}}{Color intensity.}

    \item{\code{Hue}}{Hue}

    \item{\code{Dilution}}{D280/OD315 of diluted wines.}

    \item{\code{Proline}}{Proline}

  }
}
\source{

  The data was downloaded from the UCI Machine Learning Repository.

  It was read as a CSV file with no header using
  \code{\link{read.csv}}. The columns were then given the appropriate
  names using \code{\link{colnames}} and the Type was transformed into a
  factor using \code{\link{as.factor}}. The compressed R data file was
  saved using \code{\link{save}}:

  \preformatted{
  UCI <- "https://archive.ics.uci.edu/ml"
  REPOS <- "machine-learning-databases"
  wine.url <- sprintf("%s/%s/wine/wine.data", UCI, REPOS)
  wine <- read.csv(wine.url, header=FALSE)
  colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash',
                      'Alcalinity', 'Magnesium', 'Phenols',
                      'Flavanoids', 'Nonflavanoids',
                      'Proanthocyanins', 'Color', 'Hue',
                      'Dilution', 'Proline')
  wine$Type <- as.factor(wine$Type)
  save(wine, file="wine.Rdata", compress=TRUE)
  }
}

\references{

  Asuncion, A. & Newman, D.J. (2007). \emph{UCI Machine Learning
  Repository},
  Irvine, CA:
  University of California, School of Information and Computer Science.

}

\keyword{datasets}
