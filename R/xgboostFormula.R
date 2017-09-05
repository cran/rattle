###########################################################################
## Title: Define functions to enable the formula form of xgboost model
## Author: Zhou Fang, Data Scientist, Microsoft
## Date: 11-05-2017
## Rework the implementation: Graham Williams
## Date: 20170710
## Function names:
## xgboost.formula
## xgb.importance.formula
## predict.xgboost.formula
###########################################################################

xgboost <- function(...) UseMethod("xgboost")

xgboost.formula <- function(form, data, nrounds=100, na.action=na.omit, ...)
{
  # FOR NOW ASSUME BINARY CLASSIFICATION TASK ONLY FIXME

  # Perform the NA action and note the missing observations.

  nads <- data %>% na.action()
  miss <- nads %>% attr("na.action") %>% as.vector() # Assume na.omit() FIXME

  # Create a sparse matrix from the supplied dataset. This will turn
  # categoricals into indictor variables.

  sds <- Matrix::sparse.model.matrix(form, data=nads)
 
  # Create the target vector.

  form %>%
    all.vars() %>%
    magrittr::extract(1) ->
  target

  # Make sure the target is a factor then convert to 0/1.
  
  data[[target]] %>%
    as.factor() %>%
    as.integer() %>%
    magrittr::subtract(1) ->
  label

  if (! is.null(miss)) label <- label[-miss]
  
  # Train xgboost model.  Note the use of print_every_n. I tried
  # verbose=0 but then there is no cb.evaluation.log produced and so
  # don't get the extra information we need. So use a big value for n
  # to aim for first and last iterations.
  
  model <- xgboost::xgboost(data          = sds,
                            label         = label,
                            nrounds       = nrounds,
                            print_every_n = 1000,
                            ...)

  # Record the actual formula and the final list of features for later
  # usage.
  
  model$formula  <- form
  model$dimnames <- sds@Dimnames[[2]]

  # Add extra class for the formula based model.

  class(model) <- c("xgb.formula", class(model))
  
  return(model)
}

importance <- function(...) UseMethod("importance")

importance.xgb.formula <- function(model, data, ...)
{
  # Remove the local class so xgboost is not confused.

  class(model) %<>% setdiff("xgb.formula")
  
  # Calculate the feature importance.

  imp <- xgboost::xgb.importance(feature_names=model$dimnames, model=model, ...) 
  
  return(imp)
}

predict.xgb.formula <- function(object, newdata, ...)
{
  # Transform to model matrix of just the variables required based on
  # the formula.
  
  mf <- model.frame(object$formula, data=newdata)
  vars <- attr(attr(mf, "terms"), "term.labels")
  x  <- model.matrix(attr(mf, "terms"), data=mf) 
  na <- attr(mf, "na.action") %>% as.vector()
  
  # Convert the data into a sparse matrix as required for
  # predict.xgb.Booster().
  
  x <- Matrix::Matrix(x, sparse=TRUE)
  
  # Remove our local xgb.formula class so that predict will use the
  # appropriate xgboost:: method. Otherwise xgboost includes a test
  # for == class() rather than %in% class() and fails.
  
  class(object) %<>% setdiff("xgb.formula")
  
  # Predict on the new data.

  pr <- predict(object, newdata=x, ...)

  # Splice the missing observations as NA predicitons into the
  # result. Is there a splice function? Note the boundary conditions.

  for (i in na)
    if (i > length(pr))
      pr <- c(pr, NA)
    else
      pr <- c(pr[1:i-1], NA, pr[i:length(pr)])
  
  return(pr)
}

print.xgb.formula <- function(model, ...)
{
  # Remove the local class so xgboost is not confused.

  class(model) %<>% setdiff("xgb.formula")

  print(model, ...)
}
