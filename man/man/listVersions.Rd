\name{listVersions}
\alias{listVersions}
\title{
  
  Versions of Installed Packages
  
}
\description{

  Generate a list of packages installed and their version number.

}
\usage{

listVersions(file="", ...)

}
\arguments{

  \item{file}{a character string naming a file or a connection open for
    writing. '""' indicates output to the console.}

  \item{...}{arguments to \code{\link{write.csv}}.}

}
\details{

  This function is useful in reporting problems or bugs, to ensure there
  is a clear match of R package versions between the system exhibiting
  the issue and the test system replicating the issue.

  By default the information is written to the console in a comma
  separated form, that is ideally designed to be written to a CSV file
  for emailing.
  
}
\seealso{\code{\link{write.csv}}}
\author{\email{Graham.Williams@togaware.com}}
