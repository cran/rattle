rocChart <- function(pr, target)
{
  # Calculate the true positive and the false 
  # positive rates.
  
  rates <- pr %>%
    ROCR::prediction(target) %>%
    ROCR::performance("tpr", "fpr")

  # Calulcate the AUC.

  auc <- pr %>%
    ROCR::prediction(target) %>%
    ROCR::performance("auc") %>%
    attr("y.values") %>%
    magrittr::extract2(1)
  
  # Construct the plot.
  
  pl <- data.frame(tpr=attr(rates, "y.values")[[1]], 
                   fpr=attr(rates, "x.values")[[1]]) %>%
    ggplot2::ggplot(ggplot2::aes(fpr, tpr)) +
    ggplot2::geom_line() +
    ggplot2::annotate("text", x=0.875, y=0.125, vjust=0,
                      label=paste("AUC =", round(100*auc, 2)), 
                      family="xkcd") +
    ggplot2::xlab("False Positive Rate (1-Specificity)") +
    ggplot2::ylab("True Positive Rate (Sensitivity)")
  
  # Return the plot object.

  attr(pl, "auc") <- auc
  return(pl)
}
