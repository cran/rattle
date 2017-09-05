#' Build a random forest model: traditional or conditional.
#' 
#' Time-stamp: <2016-07-22 22:06:58 Graham Williams>
#'
executeModelRF <- function(traditional=TRUE, conditional=!traditional)
{
  # 160722
  #
  # We might consider adding wsrf and rxDecisionForest and
  # mxFastForest.

  ## SETUP
  
  # Decide which random forest to build: randomForest or cforest
  
  FUN <- ifelse(traditional, "randomForest::randomForest", "party::cforest")

  # Make sure the appropriate package is available.
  
  if ((traditional&&
         ! packageIsAvailable("randomForest", Rtxt("build a random forest model")))
      ||
      conditional &&
        ! packageIsAvailable("party", Rtxt("build a random forest model")))
    return(FALSE)

  # Identify the appropriate textview widget to display results.
  
  TV <- "rf_textview"

  ## BUILD THE FUNCTION CALL
  
  num.classes <-
    crs$target %>%
    magrittr::extract2(crs$dataset, .) %>%
    as.factor() %>%
    levels() %>%
    length()

  parms <- ""

  # For the traditional random forest make sure there are no included
  # variables which have more than 32 levels, which can not be handled
  # by randomForest().

  if (traditional)
  {
    categoricals <-
      crs$input %>%
      lapply(function(x) is.factor(crs$dataset[,x])) %>%
      unlist() %>%
      magrittr::extract(crs$input, .)

    for (i in categoricals)
      if (length(levels(crs$dataset[,i])) > 32)
      {
        Rtxt("This implementation of random forests does not",
             "handle categorical variables with more than",
             "32 levels.",
             "Having a large number of levels tends to introduce",
             "bias into the tree algorithms. The variable %s has",
             "%d levels\n\nPlease choose to ignore it in the",
             "Data tab if you wish to build a",
             "random forest model.") %>%
          sprintf(i, length(levels(crs$dataset[,i]))) %>%
          errorDialog()
        return(FALSE)
      }
  }

  ## RETRIEVE OPTIONS AND SET UP FUNCTION ARGUMENTS

  # We use the supplied values even if they have not been changed from
  # the defaults (crv$rf.ntree.default, crv$rf.mtry.default). The
  # underlying randomForest() default itself might change and thus
  # results could be confusing.

  ntree <- theWidget("rf_ntree_spinbutton")$getValue()
  parms <- sprintf("\n  ntree=%d", ntree)

  mtry  <- theWidget("rf_mtry_spinbutton")$getValue()
  parms <- sprintf("%s,\n  mtry=%d", parms, mtry)

  if (traditional)
  {
    sampsize <- theWidget("model_rf_sample_entry")$getText()

    if (nchar(sampsize) > 0)
    {
      ss <- sampsize %>% strsplit(",") %>% unlist() %>% as.numeric()

      if (! (length(ss) == 1 || length(ss) == num.classes))
      {
        Rtxt("The supplied sample sizes (%s) need to be either",
             "a single number or else correspond to the number",
             "of classes found in the target variable '%s'.",
             "Please supply exactly 1 or %d sample sizes.") %>%
          sprintf(sampsize, crs$target, num.classes) %>%
          errorDialog()
        
        return(FALSE)
      }
      #TODO Check if sample sizes are larger than the classes!
      parms <- sprintf("%s,\n  sampsize=c(%s)", parms, sampsize)
    }
    
    # By default the MeanDecreaseGini is available for plotting. With
    # importance MeanDecreaseAccuracy is also available, and it seems
    # to also print the relative importance with regard class. So by
    # default, generate them both.
  
    parms <- sprintf("%s,\n  importance=TRUE", parms)
  }
  
  # Build the formula for the model. TODO We assume we will always do
  # classification rather than regression, at least for now.

  frml <- ifelse(is.factor(crs$dataset[[crs$target]]),
                 crs$target,
                 sprintf(ifelse(numericTarget(), "%s", "as.factor(%s)"),
                         crs$target)) %>% paste("~ .")

  # List, as a string of indicies, the variables to be included. 

  # included <- getIncludedVariables()
  included <- "c(crs$input, crs$target)" # 20110102
  
  # Some convenience booleans.

  sampling   <- not.null(crs$sample)
  including  <- not.null(included)
  subsetting <- sampling || including

  # Ensure we have some data - i.e., not all records will be removed
  # because they have missing values.

  dataset <- paste("crs$dataset",
                   if (subsetting) "[",
                   if (sampling) "crs$sample",
                   if (subsetting) ", ",
                   if (including) included,
                   if (subsetting) "]",
                   sep="")

  # 130322 Don't na.omit for cforest - not needed?
  #if (! traditional)
  #  dataset <- sprintf("na.omit(%s)", dataset)

  # Replicate rows according to the integer weights variable for
  # randomForest.
  
  if(! is.null(crs$weights))
    if (traditional)
      dataset <- paste(dataset,
                       "[rep(row.names(",
                       dataset,
                       "),\n                                        ",
                       # Use eval since crs$weights could be a formula
                       'as.integer(eval(parse(text = "', crs$weights,
                       '"))',
                       if (sampling) '[crs$sample]',
                       ')),]',
                       sep="")
    else
      dataset <- sprintf("%s,\n  weights=%s%s", dataset,
                       crs$weights,
                       ifelse(sampling, "[crs$sample]", ""))


  # 100107 Deal with missing values. I've not tested whether cforest
  # has issues with missing values.
  
  naimpute <- theWidget("model_rf_impute_checkbutton")$getActive()
  if (traditional)
  {
    missing.cmd <- sprintf('length(attr((na.omit(%s)), "na.action"))', dataset)
    result <- try(missing <- eval(parse(text=missing.cmd)), silent=TRUE)
    if (inherits(result, "try-error")) missing <- 0
    dsrow.cmd <- sprintf("nrow(%s)", dataset)
    result <- try(dsrow <- eval(parse(text=dsrow.cmd)), silent=TRUE)
    if (inherits(result, "try-error")) dsrow <- 0
    if (missing == dsrow && ! naimpute)
    {
      Rtxt("All observations in the dataset have one or more",
           "missing values for the selected variables.",
           "The random forest algorithm ignores any observation",
           "with missing values. No observations remain from which",
           "to build a model.",
           "To fix this problem, you might, for example, Ignore any",
           "variable with many or any missing values.",
           "Or else enable the Impute check button to impute",
           "the medium (numeric) or most frequent (categoric) value",
           "using randomForests' na.roughfix().",
           "You could also use imputation to fill in default or modelled",
           "values for the missing values manually.") %>%
        errorDialog()
      return()
    }
  }
    
  # START THE LOG
  
  startLog()

  # BUILD THE MODEL

  rf.cmd <-
    paste("set.seed(crv$seed)\n\n",
          "crs$rf <- ", FUN, "(", frml,
          ",\n  data=",
          dataset, ", ",
          #                  ifelse(traditional,
          #                         ",\n      data=crs$dataset",
          #                         ",\n      data=na.omit(crs$dataset"),
          #                  if (subsetting) "[",
          #                  if (sampling) "crs$sample",
          #                  if (subsetting) ",",
          #                  if (including) included,
          #                  ifelse(subsetting,
          #                         ifelse(traditional, "], ", "]), "),
          #                         ifelse(traditional, "", ")")),
          ifelse(traditional, parms,
                 sprintf(paste0("\n  controls=party::",
                                "cforest_unbiased(%s)"),
                         stringr::str_replace_all(parms, "\\n ", "\\\n   "))),
          ifelse(traditional,
                 sprintf(",\n  na.action=%s",
                         ifelse(naimpute, "randomForest::na.roughfix", "na.omit")),
                 ""),
          # 100521 Turn subsampling with replacement off since
          # it is more likely to produce biased imprtance
          # measures, as explained in
          # http://www.ncbi.nlm.nih.gov/pmc/articles/PMC1796903/
          # Note that for cforest, cforest_unbiased uses
          # replace=FALSE also.
          if (traditional) ",\n  replace=FALSE",
          ")", sep="")
  
  comment <-
    Rtxt("Build a %s model using %s.") %>%
    sprintf(commonName(crv$RF),
            ifelse(traditional,
                   "the traditional approach",
                   "conditional inference trees"))
  appendLog(comment, rf.cmd)

  start.time <- Sys.time()
  result <- try(eval(parse(text=rf.cmd)), silent=TRUE)

  if (inherits(result, "try-error"))
  {
    if (any(grep("cannot allocate vector", result)))
    {
      Rtxt("The call to '%s' appears to have failed.",
           "This is often due, as in this case,",
           "to running out of memory.",
           "A quick solution is to sample the dataset",
           "through the Transform tab.",
           "On 32 bit machines you may be limited to",
           "less than 2000 observations.") %>%
        sprintf(FUN) %>%
        errorDialog()
      setTextview(TV)
    }
    else if (any(grep("NA/NaN/Inf", result)))
    {
      # TODO 080520 This error arose when a log transform is done on
      # Deductions where there are many 0's (hence -Inf). To be more
      # helpful, find the variable with the -Inf and suggest ignoring
      # it. We can test this is the error if the following returns
      # non-zero:
      #
      # sum(crs$dataset[crs$sample,c(2:10, 13:14)]==-Inf, na.rm=TRUE)
      
      Rtxt("The call to '%s' failed.",
           "The problem may be with the data",
           "containing Infinite values.",
           "A quick solution may be to remove variables",
           "with any Inf or -Inf values.") %>%
        sprintf(FUN) %>%
        errorDialog()
                          
      setTextview(TV)
    }
    else if (any(grep('inherits', result)))
    {
      # 080825 This is a known error in randomForest 4.5-25 reported
      # to Andy Liau a few months ago, and acknowledge, but has not
      # been fixed. Of course i could try to figure it out myself, but
      # it would probably take some effort!
      
      Rtxt("The call to randomForest failed.",
           "You probably have version 4.5-25.",
           "This is a known problem and is fixed in 4.5-26.",
           "Please install a newer version of randomForest.\n",
           "\ninstall.packages('randomForest',\n",
           "    repos='http://rattle.togaware.com')") %>%
        errorDialog()
      
      setTextview(TV)
    }
    else 
      errorMessageFun(FUN, result) %>% errorDialog()
    return(FALSE)
  }

  resetTextview(TV)
  
  # Display the resulting model.

  summary.cmd <- "crs$rf"
  appendLog(sprintf(Rtxt("Generate textual output of the '%s' model."),
                    commonName(crv$RF)),
            summary.cmd)

  addTextview(TV, sprintf(Rtxt("Summary of the %s Model"), commonName(crv$RF)),
              "\n==================================",
              "\n\n",
              "Number of observations used to build the model: ",
              ifelse(traditional, length(crs$rf$y), crs$rf@responses@nobs),
              "\n",
              ifelse(naimpute, "Missing value imputation is active.\n", ""),
              sub(") \\n", ")\n\n",
                  sub(", ntree", ",\n              ntree",
                      sub(", data", ",\n              data",
                          gsub(", *", ", ", collectOutput(summary.cmd, TRUE))))))

  # 6 Mar 2012 Add some AUC information as suggested by Akbar Waljee.

  if (traditional && binomialTarget() &&
      packageIsAvailable("pROC", Rtxt("calculate AUC confidence interval")))
  {
    roc.cmd <- "pROC::roc(crs$rf$y, as.numeric(crs$rf$predicted))"
    ci.cmd  <- "pROC::ci.auc(crs$rf$y, as.numeric(crs$rf$predicted))"

    appendLog(Rtxt("The `pROC' package implements various AUC functions."))

    appendLog("Calculate the Area Under the Curve (AUC).", roc.cmd)
    appendLog("Calculate the AUC Confidence Interval.", ci.cmd, include.libs=FALSE)
    
    addTextview(TV,
                "\n\nAnalysis of the Area Under the Curve (AUC)",
                "  \n==========================================\n",
                collectOutput(roc.cmd),
                "\n\n",
                collectOutput(ci.cmd))
  }
  
  # Display the variable importance.

  # 100107 There is a very good importance measure from cforest that
  # needs to go here.
  
  if (traditional)
  {
    if (numericTarget())
      varimp.cmd <- paste("rn <- crs$rf %>%",
                          "    randomForest::importance() %>%",
                          "    round(2)",
                          "    rn[order(rn[,1], decreasing=TRUE),]",
                              sep="\n")
    else
      varimp.cmd <- paste("rn <- round(randomForest::importance(crs$rf), 2)",
                          # 100521 Sort on the accuracy measure rather
                          # than the Gini measure, since the Gini is
                          # biased in favour of categoric variables
                          # with many categories.
                          # "rn[order(rn[,3] + rn[,4], decreasing=TRUE),]",
                          "rn[order(rn[,3], decreasing=TRUE),]",
                          sep="\n")
  }
  else
  {
    varimp.cmd <- paste("data.frame(Importance=sort(party::varimp(crs$rf),",
                        "decreasing=TRUE))")
  }

  appendLog(Rtxt("List the importance of the variables."), varimp.cmd)

  result <- try(collectOutput(varimp.cmd), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    msg <- errorMessageFun(ifelse(traditional,
                                  "randomForest::importance",
                                  "party::varimp"), result)
    errorDialog(msg)
    return(FALSE)
  }
  
  addTextview(TV, sprintf("\n\n%s", Rtxt("Variable Importance")),
              "\n===================\n\n", result, "\n")

  # 100107 What is the purpose of this?

  if (sampling) crs$smodel <- union(crs$smodel, crv$RF)

  # Now that we have a model, make sure the buttons are sensitive.

  showModelRFExists(traditional=traditional, conditional=conditional)

  # Finish up.

  time.taken <- Sys.time()-start.time

  reportTimeTaken(TV, time.taken, commonName(crv$RF))

  return(TRUE)
}
