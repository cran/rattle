\name{acquireAuditData}

\alias{acquireAuditData}

\title{Generate the audit dataset.}

\description{

  Rattle uses an artificial dataset for demonstration purposes. This
  function retrieves the source data
  \url{https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data}
  and then transforms the data in a variety of ways.
  
}

\usage{
acquireAuditData(write.to.file=FALSE)
}

\arguments{

  \item{write.to.file}{Whether to generate a colleciton of files based
    on the data. The files generated include: audit.csv, audit.Rdata,
    audit.arf, and audit\_missing.csv}

}

\details{

  See the function definition for details of the processing done on the
  data downloaded from the UCI repository.

}

\value{

  By default the function returns a data frame containing the audit
  dataset. If write.to.file is TRUE then the data frame is returned
  invisibly.

}

\references{Package home page: \url{https://rattle.togaware.com}}

\author{\email{Graham.Williams@togaware.com}}

\seealso{

  \code{\link{audit}}, \code{\link{rattle}}.

}
