# Rattle TwoClass Ada
#
# This is a model or template "module" for rattle.
#
# Time-stamp: <2013-01-19 16:19:57 Graham Williams>
#
# Copyright (c) 2009 Togaware Pty Ltd
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

## BUG FIX TO ALLOW ada.update TO WORK....

## From: Mark Vere Culp <culpm@umich.edu>
## Date: Thu, 22 Mar 2007 21:00:36 -0400 (EDT)
## To: Graham Williams <Graham.Williams@togaware.com>
## X-Spam-Status: No, score=-2.6 required=5.0 tests=BAYES_00 autolearn=ham
##         version=3.1.7-deb

## Hello Graham,
##   Well it is going to be a while before I get the next update of ada on
## the web, there are some errors that I have to trace through.  Anyways, the
## reason, I'm sending this, is that I was thinking in the meantime you could
## just have this formula function avialable (like a wrapper to ada).  It
## should default to this one.  Also, I think I will go with the random
## forest tree engine for ada.

## Thanks,
## Mark

## TO TEST:
##
## library(ada)
## audit <- read.csv("audit.csv")
## set.seed(42)
## mysample <- sample(nrow(audit), 1400)
## myada <- ada(Adjusted ~ ., data=audit[mysample,c(2:4,6:10,13)],
##              control=rpart.control(maxdepth=30, cp=0.010000,
##                                    minsplit=20, xval=10),
##              iter=5)
## myada <- update(myada, audit[mysample,c(2:4,6:10)],
##                 audit[mysample,c(13)], n.iter=10)
##
## WITHOUT THE FIX WE GET
##
## Error in table(sign(fits), y) : all arguments must have the same length

# This function is written by Mark Vere Culp <culpm@umich.edu> and
# used with his permission, as noted in the email above.

"ada.formula" <-
function(formula, data,...,subset,na.action=na.rpart){
 ## m = match.call(expand.dots = FALSE)
  ##m[[1]] = as.name("model.frame")
  ##m$...=NULL
  ##m =eval(m,parent.frame())

  m <- match.call(expand.dots=FALSE)
  m$model <- m$method <- m$control <- NULL
  m$x <- m$y <- m$parms <- m$... <- NULL
  m$cost <- NULL
  m$na.action <- na.action
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())

  Terms = attr(m, "terms")
  y = as.vector(model.extract(m,"response"))
  preds<-attr(attributes(m)$terms,"term.labels")
  x<-as.data.frame(m[,!is.na(match(names(m),preds))])
  res = ada:::ada.default(x,y,...,na.action=na.action)
  res$terms = Terms
  cl = match.call()
  cl[[1]] = as.name("ada")
  res$call = cl
  res
}


## This implements a generic interface for interacting with the ada
## modeller and ada models. It can be used independent of the Rattle
## GUI, but is designed for use by it.

buildModelAda <- function(formula,
                          dataset,
                          tv=NULL,
                          seed="crv$seed",
                          maxdepth=30,
                          minsplit=20,
                          cp=0.01,
                          xval=10,
                          ntree=50)
{
  ## If tv is not null, then we will be updating the textview object
  ## as we proceed, as well as sending information to the log. The aim
  ## is for this function to run totally independent of the GUI, but
  ## to also support it. A developer can use this function, supply
  ## their own textview object and their own implementations of
  ## resetTextview and appendTextview for the modelling output, and
  ## startLog and appendLog for a log of the commands, and
  ## setStatusBar for a summary of what has been done.

  gui <- not.null(tv)
  if (gui) startLog(commonName(crv$ADA))

  # Load the required package into the library.

  lib.cmd <-  "require(ada, quietly=TRUE)"
  if (! packageIsAvailable("ada", Rtxt("build an AdaBoost model"))) return(FALSE)
  if (gui) appendLog(Rtxt("The `ada' package implements the boost algorithm."), lib.cmd)
  eval(parse(text=lib.cmd))

  # Replicate rows according to the integer weights variable.
  
  if(! is.null(crs$weights))
    dataset <- paste(dataset,
                     "[rep(row.names(",
                     dataset,
                     "),\n                                    ",
                     # Use eval since crs$weights could be a formula
                     'as.integer(eval(parse(text = "', crs$weights,
                     '"))[crs$sample])),]',
                     sep="")
  
  # Construct the appropriate rpart control.
  
  control <- sprintf(paste(",\n      control=rpart.control(maxdepth=%d,",
                           "cp=%f,", "minsplit=%d,", "xval=%d)",
                           sep="\n           "),
                     maxdepth, cp, minsplit, xval)
  
  # Build a model. Note that there is randomness in this
  # implementation of AdaBoost, so set the seed to get the same result
  # each time.

  model.cmd <- paste(sprintf("set.seed(%s)\n", seed),
                     "ada(", formula, ",\n      data=", dataset,
                     control, ",\n      iter=", ntree, ")",
                     sep="")

  if (gui) appendLog(sprintf(Rtxt("Build the %s model."), commonName(crv$ADA)),
                     gsub('ada\\(', 'crs$ada <- ada(', model.cmd))

  # Note that this crs$ada is not the global crs$ada! We use it here
  # to be consistent in terms of the commands that are reported to the
  # log, but we return this value and in the outer call we globally
  # assign to crs$ada, at least in the context of the Rattle GUI.
  
  start.time <- Sys.time()
  crs$ada <- try(eval(parse(text=model.cmd)), silent=TRUE)
  time.taken <- Sys.time()-start.time

  if (inherits(crs$ada, "try-error"))
  {
    msg <- errorMessageFun("ada", crs$ada)
    if (gui)
    {
      errorDialog(msg)
      return(NULL)
    }
    stop(msg)
  }
  
  ## Print the results of the modelling.

  if (gui)
  {
    print.cmd <- paste("print(crs$ada)",
                       "round(crs$ada$model$errs[crs$ada$iter,], 2)",
                       "cat('Variables actually used in tree construction:\\n')",
                       "print(sort(names(listAdaVarsUsed(crs$ada))))",
                       "cat('\\nFrequency of variables actually used:\\n')",
                       "print(listAdaVarsUsed(crs$ada))",
                       sep="\n")
    appendLog(Rtxt("Print the results of the modelling."), print.cmd)
    resetTextview(tv, tvsep=FALSE,
                  sprintf(Rtxt("Summary of the %s model:"),
                          commonName(crv$ADA)),
                  "\n\n",
                  collectOutput(print.cmd),
                  "\n")
  }

  # Finish up.
  
  if (gui) reportTimeTaken(tv, time.taken, model=commonName(crv$ADA))

  return(crs$ada)
}

continueModelAda <- function(niter)
{
  ## The current ada.update only works when the model is built using
  ## the x, y interface rather than the formula interface. Mark Culp,
  ## the author of ada, sent a fixed ada.formula wrapper which I've
  ## included above until he gets to release a new version of ada.

  ## I.e., the following works

  ## crs$ada <- ada(x,y)
  ## crs$ada <- update(crs$ada, x, y, n.iter=100)
  
  ## but the following does not:
  
  ## crs$ada <- ada(Adjust ~ ., data=ds)
  ## crs$ada <- update(crs$ada, x, y, n.iter=100)
  
  tv <- theWidget("ada_textview")
  gui <- not.null(tv)
  if (gui) startLog(Rtxt("Ada Boost Update"))
  
  ## Load the required package into the library.

  lib.cmd <-  "require(ada, quietly=TRUE)"
  if (! packageIsAvailable("ada", Rtxt("update the AdaBoost model"))) return(FALSE)
  if (gui) appendLog(sprintf(Rtxt("Require the %s package."), "ada"), lib.cmd)
  eval(parse(text=lib.cmd))

  ## We use the gdata funtion remove.vars to simply remove
  ## variables. Seems a bit much needing it here, but makes it
  ## simpler.

  lib.cmd <-  "require(gdata, quietly=TRUE)"
  if (! packageIsAvailable("gdata", Rtxt("update the AdaBoost model"))) return(FALSE)
  if (gui) appendLog(sprintf(Rtxt("Require the %s package."), "gdata"), lib.cmd)
  eval(parse(text=lib.cmd))
  
  ## Build up the update command, which needs the data rather than
  ## formula interface. 

  vname <- strsplit(deparse(parse(text=crs$ada$call)[2][[1]]), " ")[[1]][1]
  dname <- deparse(parse(text=crs$ada$call)[3][[1]], width.cutoff=500)
  update.cmd <- sprintf(paste("update(crs$ada,",
                              'remove.vars(%s, c("%s"), info=FALSE),',
                              '%s$%s, n.iter=%s)'),
                        dname, vname, dname, vname, niter)

  if (gui) appendLog(Rtxt("Update the adaboost model."),
                     gsub('update\\(', 'crs$ada <- update(', update.cmd))

  start.time <- Sys.time()
  crs$ada <- try(eval(parse(text=update.cmd)), silent=TRUE)
  time.taken <- Sys.time()-start.time
  
  if (inherits(crs$ada, "try-error"))
  {
    msg <- errorMessageFun("ada", crs$ada)
    if (gui)
    {
      errorDialog(msg)
      return(FALSE)
    }
    stop(msg)
  }

  # Print the results of the modelling.

  if (gui)
  {
    print.cmd <- paste("print(crs$ada)", "summary(crs$ada)", sep="\n")
    appendLog(Rtxt("Print the results of the modelling."), print.cmd)
    resetTextview(tv, tvsep=FALSE,
                  paste(Rtxt("Summary of the updated AdaBoost model:"), "\n\n"),
                  collectOutput(print.cmd))
  }

  # Finish up.

  msg <- sprintf(Rtxt("The %s model has been updated."), commonName(crv$ADA))
  if (gui) reportTimeTaken(tv, time.taken, msg=msg)

  return()
}


genPredictAda <- function(dataset)
{
  # Generate a command to obtain the prediction results when applying
  # the model to new data.
  
  return(sprintf("crs$pr <- predict(crs$ada, %s)", dataset))
}

genResponseAda <- function(dataset)
{
  # Generate a command to obtain the response when applying the model
  # to new data.
  
  return(genPredictAda(dataset))
}

genProbabilityAda <- function(dataset)
{
  # Generate a command to obtain the probability when applying the
  # model to new data.
  
  return(sprintf("%s[,2]", gsub(")$", ', type="prob")',
                                genPredictAda(dataset))))
}

plotImportanceAda <- function()
{
  ## Generate a plot of the variable importances.
  
  ## Make sure there is a model object first.

  if (is.null(crs$ada))
  {
    errorDialog(Rtxt("E135: Should not be here.",
                     "There is no ADA model and attempting to plot importance.",
                     "The button should not be active."), crv$support.msg)
    return()
  }

  ## Plot the variable importance.
  
  newPlot()
  plot.cmd <- "varplot(crs$ada)"
  appendLog(Rtxt("Plot the relative importance of the variables."), plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar(Rtxt("Variable Importance has been plotted."))
}
  
plotErrorsAda <- function()
{
  ## Generate a plot of the error rate as we add more models to the
  ## ensemble.
  
  ## Make sure there is a model object first.

  if (is.null(crs$ada))
  {
    errorDialog(Rtxt("E136: There is no ADA model and attempting to plot error.",
                     "The button should not be active."), crv$support.msg)
    return()
  }

  ## Plot the error rates.
  
  newPlot()
  plot.cmd <- "plot(crs$ada)" #, kappa=TRUE)"
  appendLog(Rtxt("Plot the error rate as we increase the number of trees."), plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar(Rtxt("Ada errors has been plotted."))
}

listTreesAda <- function(model, trees=0)
{
  stopifnot(require(ada, quietly=TRUE))
  ntrees <- length(model$model$trees)
  if (trees == 0) trees=1:ntrees
  for (i in trees)
  {
    cat(sprintf(Rtxt("\nTree %d of %d:\n"), i, ntrees))
    print(model$model$trees[[i]])
  }
}

drawTreesAda <- function(model,
                         trees=0,
                         title="")
{
  stopifnot(require(ada, quietly=TRUE))
  ntrees <- length(model$model$trees)
  if (length(trees) == 1 && trees == 0) trees <- 1:ntrees
  for (i in trees)
  {
    newPlot()
    drawTreeNodes(model$model$trees[[i]])
    eval(parse(text=genPlotTitleCmd(sprintf(Rtxt("Tree %d of %d%s"), i,
                 ntrees, title))))
  }
}

displayHelpAda <- function()
{
  if (showHelpPlus(Rtxt("Boosting builds multiple, but generally simple, models. The",
                        "models might be decision trees that have just one split -",
                        "these are often called decision stumps. After building each",
                        "model any training entities that the model misclassifies",
                        "are boosted - they are given more weight or more importance",
                        "in the next model building step. The resulting model is",
                        "then the weighted sum of the ensemble of models built.",
                        "<<>>",
                        "The ada package is used to build the boosted model.")))
    {
      require(ada, quietly=TRUE)
      popupTextviewHelpWindow("ada")
    }
}

listAdaVarsUsed <- function(model) 
  sort(table(unlist(sapply(model$model$trees, 
                           function(x) setdiff(unique(x$frame$var), 
                                               "<leaf>")))), 
       decreasing=TRUE)
