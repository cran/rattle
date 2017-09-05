#' @title Generate an error (confusion) matrix.
#'
#' @param actual a vector of true values.
#' @param predicted a vector of predicted values.
#' @param percentage return percentages.
#' @param digits the number of digits to round results.
#' @param count return counts.
#'
#' @value An error matrix (also known as a confusion matrix) is
#'   generated based on the comparison of the actual and predicted
#'   values. One of three forms is returned: percentages (pc), counts,
#'   or proportions (if both percentage and counts are FALSE).

errorMatrix <- function(actual,
                        predicted,
                        percentage=TRUE,
                        digits=ifelse(percentage,1,3),
                        count=FALSE)
{
  # Preconditions.
  
  if (!missing(percentage) & percentage & count)
    stop("percentages not possible as counts were specified")

  # Data quality checks.
  #
  # If both actual and predicted are factors they must
  # have the same levels in the same order else the table will have
  # rearranged column or row orders - the table is expeted to have the
  # labels in the same order column and row wise.
  #
  # If either is a factor and the other a character then convert the
  # character to a factor with the levels of the factor used.
  #
  # If both are character or numeric leave it to table() to sort out.
  
  if (is.factor(actual) & is.factor(predicted))
  {
    if (! all(levels(actual) == levels(predicted)))
      stop("The supplied actual and predicted must have the same levels.")
  } else if (is.factor(actual))
  {
    predicted <- factor(predicted, levels=levels(actual))
  } else if (is.factor(predicted))
  {
    actual <- factor(actual, levels=levels(predicted))
  }
  
  # Initial table.
  
  x   <- table(actual, predicted)

  # Number of classes.
  
  nc  <- nrow(x)

  # Number of values.
  
  nv  <- length(actual) - sum(is.na(actual) | is.na(predicted))

  # Calculate proportions.
  
  if (!count) x <- x/nv

  # Calculate class error. For row r this is the sum of all values in
  # the row minus the r'th value, divided by the sum of all values in
  # the row. If count then the error is returned as a percentage rather
  # than a proportion.
  
  tbl <- cbind(x,
               Error=sapply(1:nc,
                            function(r)
                            {
                              y <- sum(x[r,-r])/sum(x[r,])
                              if (count) y <- round(100*y, digits)
                              return(y)
                            }))

  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")

  # Round the resulting percentages or proportions unless we are
  # returning count.
  
  if (!count) tbl <- if (percentage) round(100*tbl, digits) else round(tbl, digits)

  return(tbl)
}
