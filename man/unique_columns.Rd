\name{unique_columns}
\alias{unique_columns}
\title{
  List columns with unique values for every row.
}
\description{

   Identify columns with unique values for each row in the supplied dataset.

}
\usage{
unique_columns(tbl)
}
\arguments{

  \item{tbl}{A data frame or tibble.}
}
\details{

  A function to return a list of the names of those columns which have a
  unique value for each row in the dataset, excluding real valued
  columns. This is used by Rattle V6 to identify potential identifier
  variables.

}
\keyword{metadata}
