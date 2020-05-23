ggVarImp <- function(model, ...) UseMethod("ggVarImp")

ggVarImpPlot <- function(ds,
                         n=NULL,
                         title="Variable Importance",
                         label="Relative Importance",
                         caption=genPlotTitleCmd(vector=TRUE),
                         log=FALSE)
{
  # Expect ds to contain at least the columns Variable and Importance.
  
  if (length(n) == 1L) ds <- head(ds, n)

  ds %>%
    dplyr::arrange(desc(Importance)) %>%
    dplyr::mutate(Variable=factor(Variable, levels=rev(unique(Variable)))) %>%
    ggplot2::ggplot(ggplot2::aes(x    = Variable,
                                 y    = Importance,
                                 fill = Variable)) +
    ggplot2::geom_bar(stat     = "identity",
                      position = "identity",
                      width    = 0.1) +
    ggplot2::labs(title   = title,
                  y       = label,
                  x       = "",
                  caption = caption) +
    ggplot2::coord_flip() +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x  = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   legend.position = "none") ->
  p

  if (log)
    p <- p + ggplot2::scale_y_continuous(trans="log10")
  else
    p <- p + ggplot2::scale_y_continuous(labels=scales::comma)

  return(p)
}

ggVarImp.randomForest <- function(model, 
                                  title="Random Forest Variable Importance",
                                  ...)
{
  # By default randomForest() only returns the MeanDecreaseGini. With
  # importance=TRUE at model build time we also get
  # MeanDecreaseAccuracy and importance relative to the target levels.
  
  randomForest::importance(model) %>%
    data.frame() %>%
    dplyr::mutate(Variable=row.names(.)) %>%
    tidyr::gather(Measure, Importance, -Variable) %>%
    dplyr::group_by(Measure) %>%
    dplyr::mutate(Importance=(max(Importance)-Importance)/(max(Importance)-min(Importance))) %>%
    ggVarImpPlot(title, ...) +
    ggplot2::facet_wrap(~ Measure)
}

ggVarImp.rpart <- function(model,
                           title="Decision Tree Variable Importance",
                           ...)
{
  model$variable.importance %>%
    data.frame() %>%
    magrittr::set_names("Importance") %>%
    dplyr::mutate(Variable=row.names(.)) %>%
#    dplyr::arrange(desc(Importance)) %>%
#    dplyr::mutate(Variable=factor(Variable, levels=rev(unique(Variable)))) %>%
    ggVarImpPlot(title, ...)
}

ggVarImp.rxDForest <- function(model,
                               title="Big Data Random Forest Variable Importance",
                               ...)
{
  model$importance %>%
    data.frame() %>%
    dplyr::mutate(Variable=row.names(.)) %>%
#    dplyr::arrange(desc(IncNodePurity)) %>%
#    dplyr::mutate(Variable=factor(Variable, levels=rev(unique(Variable)))) %>%
    dplyr::rename(Importance=IncNodePurity) %>%
    ggVarImpPlot(title, ...)
}

ggVarImp.xgb.Booster <- function(model, 
                                 feature_names=NULL,
                                 title="Extreme Gradient Boost Variable Importance",
                                  ...)
{
  # The model does not include the feature/colnames, so we need to
  # have an option to pass it in.
  
  xgboost::xgb.importance(feature_names=feature_names, model=model) %>%
    dplyr::rename(Variable=Feature, Importance=Gain) %>%
    dplyr::select(Variable, Importance) %>%
    ggVarImpPlot(title, ...)
}

ggVarImp.xgb.formula <- function(model, 
                                 feature_names=NULL,
                                 title="Extreme Gradient Boost Variable Importance",
                                  ...)
{
  class(model) %<>% setdiff("xgb.formula")
  ggVarImp(model, feature_names=model$dimnames)
}
