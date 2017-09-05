\name{setupDataset}
\alias{setupDataset}
\title{
  Given specific contents of env add other dataset related variables.
}
\description{
  
  This rattle support function is used for encapsulating data mining
  objects. The supplied environment is augmented with other data derived
  from the supplied data, such as a sample trianing dataset, list of
  numeric variables, and a formula for modelling.
  
}
\usage{
setupDataset(env, seed=NULL)

}
\arguments{
  
  \item{env}{the environment to modify.}

  \item{seed}{optionally set the seed for repeatability.}
  
}
\details{
  
  The supplied object (an environment) is assumed to also contain the
  variables data (a data frame), target (a character string naming the
  target variable), risk (a character string naming the risk variable),
  and inputs (a character vector naming all the input variables). This
  function then adds in the variables vars (the variables used for
  modelling), numerics (the numeric vars within inputs), nobs (the
  number of observations), form (the formula for building models), train
  (a 70\% training dataset).

}
\references{Package home page: \url{http://rattle.togaware.com}}
\author{\email{Graham.Williams@togaware.com}}
