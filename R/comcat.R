# 20170129 Convenience combinine format with comma and cat("\n") to
# return a printed string rather than print().

comcat <- function(x, ...)
{
  cat(format(x, ..., big.mark=",", scientific=FALSE, trim=TRUE), "\n")
}

