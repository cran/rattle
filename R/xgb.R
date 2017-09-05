# Rattle TwoClass Xgb
#
# This is a model or template "module" for rattle.
#
# Time-stamp: <2017-07-11 15:34:33 Graham Williams>
#
# Copyright (c) 2009-2017 Togaware Pty Ltd
#
# This files is part of Rattle.
#
# Rattle is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Rattle is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Rattle. If not, see <http://www.gnu.org/licenses/>.

# This implements a generic interface for interacting with the ada
# modeller and ada models. It can be used independent of the Rattle
# GUI, but is designed for use by it.

buildModelXgb <- function(formula,
                          dataset,
                          missing=NA,
                          weight=NULL,
                          tv=NULL,
                          seed="crv$seed",
                          max_depth=6,
                          eta=0.3,
                          num_parallel_tree=1,
                          nthread=2,
                          nround=50,
                          metrics=list("error"),
                          objective="binary:logistic")
{
  # If tv is not null then we will be updating the textview object as
  # we proceed, as well as sending information to the log. The aim is
  # for this function to run totally independent of the GUI, but to
  # also support it. A developer can use this function, supply their
  # own textview object and their own implementations of resetTextview
  # and appendTextview for the modelling output, and startLog and
  # appendLog for a log of the commands, and setStatusBar for a
  # summary of what has been done.

  gui <- not.null(tv) && not.null(var)
  if (gui) startLog(commonName(crv$XGB))
  
  if (! packageIsAvailable("xgboost", Rtxt("build an Xgboost model"))) return(FALSE)
  
  if (gui) appendLog(Rtxt("The `xgboost' package implements the extreme gradient boost algorithm."))

  # Replicate rows according to the integer weights variable.
  
  if(! is.null(crs$weights))
    dataset <- paste(dataset,
                     "[rep(row.names(",
                     dataset,
                     "),\n                                                  ",
                     # Use eval since crs$weights could be a formula
                     'as.integer(eval(parse(text = "', crs$weights,
                     '"))[crs$sample])),]',
                     sep="")
  
  # Construct the appropriate parameter.

  #param <- sprintf(paste0(",\n%20s",
  #                          "param=list(max_depth=%d,\n",
  #                          "%49seta=%f,\n",
  #                          "%49snum_parallel_tree=%d, \n",
  #                          "%49snthread=%d,)"),
  #                   " ", max_depth, " ", eta, " ", num_parallel_tree, " ", nthread)
  
  # Build a model. Note that there is randomness in this
  # implementation of xgboost, so set the seed to get the same result
  # each time.

  model.cmd <- paste(sprintf("set.seed(%s)\n\n", seed),
                     "xgboost(", formula, ",\n",                    
                     "  data              = ", dataset, ",\n",              
                     "  max_depth         = ", max_depth, ",\n",
                     "  eta               = ", eta, ", \n",
                     "  num_parallel_tree = ", num_parallel_tree, ", \n",
                     "  nthread           = ", nthread, ", \n",
                     "  nround            = ", nround, ",\n",
                     "  metrics           = '", metrics, "',\n",
                     "  objective         = '", objective, "')",
                     sep="")

  if (gui) appendLog(sprintf(Rtxt("Build the %s model."), commonName(crv$XGB)),
                     gsub('xgboost\\(', 'crs$ada <- xgboost(', model.cmd))

  # Note that this crs$ada is not the global crs$ada! We use it here
  # to be consistent in terms of the commands that are reported to the
  # log, but we return this value and in the outer call we globally
  # assign to crs$ada, at least in the context of the Rattle GUI.
  
  start.time <- Sys.time()
  crs$ada <- try(eval(parse(text=model.cmd)), silent=TRUE)
  time.taken <- Sys.time()-start.time

  if (inherits(crs$ada, "try-error"))
  {
    msg <- errorMessageFun("xgb", crs$ada)
    if (gui)
    {
      errorDialog(msg)
      return(NULL)
    }
    stop(msg)
  }
  
  # Print the results of the modelling.

  if (gui)
  {
    print.cmd <- paste("print(crs$ada)\n",
                       "cat('\\nFinal iteration error rate:\\n')",
                       "print(round(crs$ada$evaluation_log[crs$ada$niter, ], 2))\n",
                       "cat('\\nImportance/Frequency of variables actually used:\\n')",
                       paste0("print(crs$imp <- importance(crs$ada, ", dataset, "))"),
                       sep="\n")
    appendLog(Rtxt("Print the results of the modelling."), print.cmd)
    resetTextview(tv, tvsep=FALSE,
                  sprintf(Rtxt("Summary of the %s model:"),
                          commonName(crv$XGB)),
                  "\n\n",
                  collectOutput(print.cmd),
                  "\n")
  }

  # Finish up.
  
  if (gui) reportTimeTaken(tv, time.taken, model=commonName(crv$XGB))

  return(crs$ada)
}

genProbabilityXgb <- function(dataset)
{
  # Generate a command to obtain the probability when applying the
  # model to new data.
  
  return(sprintf("crs$pr <- predict(crs$ada, %s)", dataset))
}

#genProbabilityXgb <- function(dataset)
#{
  # Generate a command to obtain the probability when applying the
  # model to new data.
  
#  return(genProbabilityXgb(dataset))
#}

genResponseXgb <- function(dataset)
{
  # Generate a command to obtain the prediction results when applying
  # the model to new data.
  
  threshold <- 0.5
  return(sprintf(paste0("lvls <- levels(as.factor(crs$dataset[[crs$target]]))\n",
                        "crs$pr <- factor(ifelse(%s > %s,\n\t\t\tlvls[2], lvls[1]))"),
                 sub("crs\\$pr <- ", "", genProbabilityXgb(dataset)), threshold))
}

plotImportanceXgb <- function()
{
  # Generate a plot of the variable importances.
  
  # Make sure there is a model object first.

  if (is.null(crs$ada))
  {
    errorDialog(Rtxt("E135: Should not be here.",
                     "There is no XGB model and attempting to plot importance.",
                     "The button should not be active."), crv$support.msg)
    return()
  }

  # Plot the variable importance.
  
  newPlot()
  plot.cmd <- "ggVarImp(crs$ada)"
  appendLog(Rtxt("Plot the relative importance of the variables."), plot.cmd)
  plot.cmd <- paste0("print(", plot.cmd, ")")
  eval(parse(text=plot.cmd))

  setStatusBar(Rtxt("Variable Importance has been plotted."))
}

plotErrorsXgb <- function()
{
  # Generate a plot of the error rate as we increase the number of iteration 
  
  # Make sure there is a model object first.
  
  if (is.null(crs$ada))
  {
    errorDialog(Rtxt("E136: There is no XGB model and attempting to plot error.",
                     "The button should not be active."), crv$support.msg)
    return()
  }
  
  # Plot the error rates.
  
  newPlot()
  plot.cmd <- "plot(crs$ada$evaluation_log, type='o')"
  appendLog(Rtxt("Plot the error rate as we increase the number of iteration."), plot.cmd)
  eval(parse(text=plot.cmd))
  
  setStatusBar(Rtxt("Xgb errors has been plotted."))
}

displayHelpXgb <- function()
{
  if (showHelpPlus(Rtxt("Extreme Gradient Boosting builds multiple models for binary classification. The",
                        "models might be decision trees that have just one split -",
                        "these are often called decision stumps. After building each",
                        "model any training entities that the model misclassifies",
                        "are boosted - they are given more weight or more importance",
                        "in the next model building step. The resulting model is",
                        "then the weighted sum of the ensemble of models built.",
                        "<<>>",
                        "The xgboost package is used to build the extreme gradient boosting model.")))
    {
      popupTextviewHelpWindow("xgb", "xgb")
    }
}


