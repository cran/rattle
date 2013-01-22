\name{cranSearch}
\alias{cranSearch}
\title{
  Search keywords in the CRAN packages table.
}
\description{
  
  Search keywords in the CRAN packages table for packages matching a
  given regular expression.
  
}
\usage{
cranSearch(lookFor="", ignoreCase=TRUE, abbreviate=50,
          CRANPackageTable = "http://http://cran.r-project.org/web/packages") 
}
\arguments{
  
  \item{lookFor}{the keywords to search for.}
  
  \item{ignoreCase}{case sensitivity is by default turned off, but it
    can be turned on.}
  
  \item{abbreviate}{the package description is by default abbreviated to
    the first 50 characters, but this can be adjusted or turned off.}

  \item{CRANPackageTable}{the CRAN mirror is set to the nternational one
    in Austria, but should be changed to your nearest.}

}
\value{A data frame.}
\author{Based on post by Bill Venables to R-DownUnder - not yet with permission.}
