#' Build a decision tree based from xdf dataset.
#' 
#' Time-stamp: <2017-08-18 12:14:38 Graham Williams>
#'
executeModelRxDTree <- function()
{
  # Identify the model specific constants.
  
  TV   <- "rpart_textview"
  NAME <- commonName(crv$RXDTREE)
  PKG  <- "RevoScaleR"
  FUNC <- "rxDTree"
  VAR  <- "crs$rpart"
  TYPE <- Rtxt("Classification")
  DESC <- Rtxt("build an xdf based decision tree model")
  
  # Check package prerequisites.
  
  if (! packageIsAvailable(PKG, DESC)) return(FALSE)

  # 160901 XDF TODO This is an approximation based on the preview xdf
  # data frame. No doubt will come back to bite one day! Sorry about
  # that....
  
  num.classes <- crs$dataset[[crs$target]] %>% as.factor() %>% levels() %>% length()
  ## control     <- NULL
  parms       <- NULL

  ## # 100222 Use information as the default split method, as per a
  ## # machine learning view of the approach.

  ## parms <- ',\n    parms=list(split="information")'

  ## # Obtain the value of the tuning controls

  ## tune.controls <- theWidget("rpart_tune_entry")$getText()

  ## # Retrieve the Priors, and check there is the right number and that
  ## # they add up to 1.

  ## priors <- theWidget("model_tree_priors_entry")$getText()
  ## if (nchar(priors) > 0)
  ## {
  ##   pr <- as.numeric(unlist(strsplit(priors, ",")))
  ##   if (length(pr) != num.classes)
  ##     {
  ##       errorDialog(sprintf(Rtxt("The supplied priors (%s)",
  ##                                "need to correspond to the number of classes",
  ##                                "found in the target variable '%s'.",
  ##                                "Please supply exactly %d priors."),
  ##                           priors,crs$target, num.classes))
  ##       return(FALSE)
  ##     }
  ##   if (sum(pr) != 1)
  ##     {
  ##       errorDialog(sprintf(Rtxt("The supplied priors (%s)",
  ##                                "add up to %0.2f whereas",
  ##                                "they need to add up 1.00"),
  ##                           priors, sum(pr)))
  ##       return(FALSE)
  ##     }
  ##   if (is.null(parms))
  ##     parms <- sprintf(",\n      parms=list(prior=c(%s))", priors)
  ##   else
  ##     parms <- gsub(")$", sprintf(",\n      prior=c(%s))", priors), parms)
  ## }

  minsplit <- theWidget("rpart_minsplit_spinbutton")$getValue()
  if (is.null(parms))
    parms <- sprintf(",\n  minSplit = %d", minsplit)
  else
    parms <- sprintf("%s,\n  minSplit = %d", parms, minsplit)

  ## # Retrieve the Min Bucket and check if it is different from the
  ## # default, and if so then use it.

  ## minbucket <- theWidget("rpart_minbucket_spinbutton")$getValue()
  ## if (minbucket != crv$rpart.minbucket.default)
  ## {
  ##   if (is.null(control))
  ##     control <- sprintf(",\n      control=rpart.control(minbucket=%d)", minbucket)
  ##   else
  ##     control <- gsub(")$", sprintf(",\n           minbucket=%d)", minbucket), control)
  ## }

  # Retrieve the Max Depth and use it.

  maxdepth <- theWidget("rpart_maxdepth_spinbutton")$getValue()
  if (is.null(parms))
    parms <- sprintf(",\n  maxdepth = %d", maxdepth)
  else
    parms <- sprintf("%s,\n  maxdepth = %d", parms, maxdepth)

  ## # Retrieve the Complexity and check if it is different from the
  ## # default, and if so then use it.

  ## cp <- theWidget("model_tree_cp_spinbutton")$getValue()

  ## if (abs(cp-crv$rpart.cp.default) > 0.00001) ## Diff when same is 2.2352e-10!!!
  ## {
  ##   if (is.null(control))
  ##     control <- sprintf(",\n      control=rpart.control(cp=%f)", cp)
  ##   else
  ##     control <- gsub(")$", sprintf(",\n           cp=%f)", cp), control)
  ## }

  ## # Retrieve the Include Missing checkbutton status and if not set
  ## # then change default beahviour in usesurrogate.

  ## usesurrogate <- theWidget("model_tree_include_missing_checkbutton")$
  ##                 getActive()
  ## if (! usesurrogate)
  ## {
  ##   if (is.null(control))
  ##     control <- paste(",\n    control=rpart.control(usesurrogate=0,",
  ##                      "\n        maxsurrogate=0)")
  ##   else
  ##     control <- gsub(")$", paste(",\n        usesurrogate=0,",
  ##                                 "\n        maxsurrogate=0)"), control)
  ## }

  ## # Retrieve the Cross Validation value and if different from
  ## # default, use it. No longer. Common wisdom is that 10 is right, so
  ## # in Rattle just go with that.

  ## # xval <- theWidget("rpart_xval_spinbutton")$getValue()
  ## # if (xval != crv$rpart.xval.default)
  ## # {
  ## #  if (is.null(control))
  ## #    control <- sprintf(", control=rpart.control(xval=%d)", xval)
  ## #  else
  ## #    control <- gsub(")$", sprintf(", xval=%d)", xval), control)
  ## # }

  ## # Retrieve the loss matrix and ensure it matches the shape of the
  ## # data.

  ## loss <- theWidget("model_tree_loss_entry")$getText()
  ## if (nchar(loss) > 0)
  ## {
  ##   lo <- as.numeric(unlist(strsplit(loss, ",")))
  ##   if (length(lo) != num.classes * num.classes)
  ##   {
  ##     errorDialog(sprintf(Rtxt("The supplied loss matrix (%s)",
  ##                               "needs to have %d values.",
  ##                               "Please enter that many values, comma separated."),
  ##                         loss, num.classes*num.classes))
  ##     return(FALSE)
  ##   }

  ##   # TODO: Perform other checks on the matrix here.  The loss matrix
  ##   # must have zeros on the diagonal and positive numbers
  ##   # elsewhere. It must be the same dimensions as the number of
  ##   # classes.

  ##   lo <- sprintf("matrix(c(%s), byrow=TRUE, nrow=%d)", loss, num.classes)

  ##   if (is.null(parms))
  ##     parms <- sprintf(",\n    parms=list(loss=%s)", lo)
  ##   else
  ##     parms <- gsub(")$", sprintf(",\n    loss=%s)", lo), parms)
  ## }

  # Construct the formula for the model build.

  crs$target %>%
    paste("~", paste(crs$input, collapse=" + ")) %>%
    strwrap(crv$log_width, 0, 4) %>%
    paste(collapse="\n") ->
  frml

  # Variables to be included --- a string of indicies.

  # included <- getIncludedVariables()
  included <- "c(crs$input, crs$target)" # 20110102

  # Some convenience booleans

  sampling  <- not.null(crs$train)
  including <- not.null(included)
  subsetting <- sampling || including

  # Commands.

  ## if (action %in%  c("tune", "best"))
  ## {
  ##   lib.cmd <- paste(lib.cmd, "library(e1071, quietly=TRUE)", sep="\n")
  ##   if (! packageIsAvailable("e1071", Rtxt("tune decision trees"))) return(FALSE)
  ## }

  ## # For now, don't use any of the other parameter settings if tune or
  ## # best. Eventually I want to use the other parameter setting sand
  ## # override them with the tune options.

  ## if (action == "build")
  ## {
  ##   ds.string <- paste("crs$dataset",
  ##                      if (subsetting) "[",
  ##                      if (sampling) "crs$train",
  ##                      if (subsetting) ", ",
  ##                      if (including) included,
  ##                      if (subsetting) "]", sep="")

  ##   rpart.cmd <- paste("crs$rpart <- rpart(", frml, ",\n    data=", ds.string,
  ##                      ifelse(is.null(crs$weights), "",
  ##                             sprintf(",\n    weights=(%s)%s",
  ##                                     crs$weights,
  ##                                     ifelse(sampling, "[crs$train]", ""))),
  ##                      ',\n    method=',
  ##                      ifelse(categoricTarget(),
  ##                             '"class"', '"anova"'),
  ##                      ifelse(is.null(parms), "", parms),
  ##                      ifelse(is.null(control), "", control),
  ##                      ")", sep="")

  ##   # 090126 Add error matrix. 100321 Don't add the error matricies -
  ##   # they are more a standard evaluation and belong in the Evaluate
  ##   # tab.

  ## ##   if (categoricTarget())
  ## ##   {
  ## ##     pds.string <- paste("crs$dataset",
  ## ##                         if (subsetting) "[",
  ## ##                         if (sampling) "-crs$sample",
  ## ##                         if (subsetting) ", ",
  ## ##                         if (including) included,
  ## ##                         if (subsetting) "]", sep="")
  ## ##     print.cmd <- paste(print.cmd, "\n",
  ## ##                        'cat("\\n',
  ## ##                        ifelse(sampling, "Validation ", "Training "),
  ## ##                        'dataset error matrix - counts\\n\\n")\n',
  ## ##                        "print(table(predict(crs$rpart, ",
  ## ##                        pds.string, ', type="class"),',
  ## ##                        pds.string, '$', crs$target,
  ## ##                        ', dnn=c("Predicted", "Actual")))\n',
  ## ##                        'cat("\\n")\n',
  ## ##                        sep="")
  ## ##     print.cmd <- paste(print.cmd, "\n",
  ## ##                        'cat("\\n',
  ## ##                        ifelse(sampling, "Validation ", "Training "),
  ## ##                        'dataset error matrix - percentages\\n\\n")\n',
  ## ##                        "print(round(100*table(predict(crs$rpart, ",
  ## ##                        pds.string, ', type="class"),',
  ## ##                        pds.string, '$', crs$target,
  ## ##                        ', dnn=c("Predicted", "Actual"))/nrow(',
  ## ##                        pds.string, ")))\n",
  ## ##                        'cat("\\n")\n',
  ## ##                        sep="")
  ## ##   }
  ## }
  ## else if (action == "tune")
  ## {
  ##   rpart.cmd <- paste("crs$tune.rpart <- tune.rpart(", frml, ", data=crs$dataset",
  ##                      if (subsetting) "[",
  ##                      if (sampling) "crs$train",
  ##                      if (subsetting) ",",
  ##                      if (including) included,
  ##                      if (subsetting) "]",
  ##                      sprintf(", %s", tune.controls),
  ##                      ")", sep="")

  ##   print.cmd <- paste("print(crs$tune.rpart)", "plot(crs$tune.rpart)", sep="\n")
  ##   }
  ## else if (action == "best")
  ## {
  ##   # This won't work - best.rpart usese the tune.control() structure
  ##   rpart.cmd <- paste("crs$rpart <- best.rpart(", frml, ", data=crs$dataset",
  ##                    if (subsetting) "[",
  ##                    if (sampling) "crs$train",
  ##                    if (subsetting) ",",
  ##                    if (including) included,
  ##                    if (subsetting) "]",
  ##                    sprintf(", %s", tune.controls),
  ##                    ")", sep="")

  ##   print.cmd <- paste("print(crs$rpart)", "printcp(crs$rpart)", sep="\n")
  ## }

  build.cmd <- paste0(VAR, " <- ", FUNC, "(\n\n  ", frml, ",\n\n",
                      "  data     = crs$xdf.split[[1]]",
                      ifelse(is.null(parms), "", parms),
                      ")\n\n",
                      "# Ensure the resulting object also looks like an rpart tree.",
                      "\n\n",
                      VAR, " <- ", "rxAddInheritance(", VAR, ")")

  print.cmd <- paste0("print(", VAR, ")")

  startLog(NAME)

  ## # Set the seed so that xerror and xstd are consistent each time

  ## seed.cmd <- 'set.seed(crv$seed)'
  ## appendLog(Rtxt("Reset the random number seed to obtain the same results each time."),
  ##           seed.cmd)
  ## eval(parse(text=seed.cmd))

  # Build the model.

  appendLog(sprintf(Rtxt("Build the %s model."), NAME), build.cmd)
  start.time <- Sys.time()
  result <- try(eval(parse(text=build.cmd)), silent=TRUE)
  time.taken <- Sys.time() - start.time
  
  ## if (inherits(result, "try-error"))
  ## {
  ##   if (any(grep("syntax error.*weights", result)))
  ##     errorDialog(sprintf(Rtxt("The call to 'rpart' has a syntax error in the",
  ##                              "weights formula. The error message was:\n\n%s"),
  ##                         result))
  ##   else
  ##     errorDialog(errorMessageFun("rpart", result))
  ##   return(FALSE)
  ## }

  ## # Summary: Show the resulting model.

  ## appendLog(sprintf(Rtxt("Generate a textual view of the %s model."),
  ##                   commonName(crv$RPART)), print.cmd)

  resetTextview(TV)
  setTextview(TV,
              sprintf(Rtxt("Summary of the %s model for %s (built using '%s'):"),
                      NAME, TYPE, FUNC),
              "\n\n",
              collectOutput(print.cmd))

  ## if (sampling) crs$smodel <- union(crs$smodel, crv$RPART)

  # Now that we have a model, make sure the rules and plot buttons are
  # visible.

  showModelRPartExists()

  # Finish up.

  reportTimeTaken(TV, time.taken, NAME)

  return(TRUE)
}
