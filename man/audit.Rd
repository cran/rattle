\name{audit}
\docType{data}
\alias{audit}
\title{Sample dataset to illustrate Rattle functionality.}
\description{
  
  The audit dataset is an artificially constructed dataset that has some
  of the characteristics of a true financial audit dataset for modelling
  productive and non-productive audits of a person's financial
  statement. A productive audit is one which identifies errors or
  inaccuracies in the information provided by a client. A non-productive
  audit is usually an audit which found all supplied information to be
  in order.

  The audit dataset is used to illustrate binary classification.  The
  target variable is identified as \code{TARGET\_Adjusted}.

  The dataset is quite small, consisting of just 2000 entities. Its
  primary purpose is to illustrate modelling in Rattle, so a minimally
  sized dataset is suitable.

  The dataset itself is derived from publicly available data (which has
  nothing to do with audits).

}

\format{

  A data frame. In line with data mining terminology we refer to the
  rows of the data frame (or the observations) as entities. The columns
  are refered to as variables.  The entities represent people in this
  case. We describe the variables here:

  \describe{
    
    \item{\code{ID}}{This is a unique identifier for each person.}

    \item{\code{Age}}{The age.}
    
    \item{\code{Employment}}{The type of employment.}

    \item{\code{Education}}{The highest level of education.}
    
    \item{\code{Marital}}{Current marital status.}
    
    \item{\code{Occupation}}{The type of occupation.}

    \item{\code{Income}}{The amount of income declared.}

    \item{\code{Gender}}{The persons gender.}

    \item{\code{Deductions}}{Total amount of expenses that a person
      claims in their financial statement.}

    \item{\code{Hours}}{The average hours worked on a weekly basis.}

    \item{\code{IGNORE_Accounts}}{The main country in which the person
      has most of their money banked. Note that the variable name is
      prefixed with IGNORE. This is recognised by Rattle as the default
      role for this variable.}

    \item{\code{RISK_Adjustment}}{This variable records the monetary
      amount of any adjustment to the person's financial claims as a
      result of a productive audit. This variable, which should not be
      treated as an input variable, is thus a measure of the size of the
      risk associated with the person.}

    \item{\code{TARGET_Adjusted}}{The target variable for modelling
      (generally for classification modelling). This is a numeric field
      of class integer, but limited to 0 and 1, indicating
      non-productive and productive audits, respectively. Productive
      audits are those that result in an adjustment being made to a
      client's financial statement.}

  }

}
\keyword{datasets}
