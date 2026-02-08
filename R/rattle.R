# Rattle: A GUI for Data Science in R
#
# BASE FUNCTIONS
#
# Time-stamp: <Sunday 2026-02-08 14:36:18 +1100 Graham Williams>
#
# Copyright (c) 2009-2023 Togaware Pty Ltd
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
# along with Rattle. If not, see <https://www.gnu.org/licenses/>.

# 120704 Avoid "no visible binding for global variable" warnings on a
# check. However, this then requires R >= 2.15.1, so only do this
# conditionally, particularly that a lot of users are not in the
# upgrade habit as yet, and Revolution R is not up to 2.15 yet.

if(getRversion() >= "2.15.1")
  utils::globalVariables(c("rattle.entered.dataset",
                           "gladeXMLNew",
                           "gladeXMLSignalAutoconnect",
                           "biocLite",
                           "Caseload",
                           "Risk",
                           "Precision",
                           "Measure",
                           "Importance",
                           "Variable",
                           "IncNodePurity",
                           "Feature",
                           "Gain",
                           "desc",
                           "pos",
                           "ticks",
                           "target",
                           "ignore",
                           "digit", "variable",
                           "split.labels",
                           "rbin",
                           "pacc",
                           "x",
                           "y",
                           "lbl",
                           "hj",
                           "vj",
                           "fpr",
                           "tpr",
                           "score",
                           "low",
                           "high",
                           "."
                           ))

# The function paste0() was introduced in 2.15.0

if (! exists("paste0")) paste0 <- function(...) paste(..., sep="")

Rtxt <- function(...)
{
    gettext(paste(...), domain="R-rattle")
}

# This is used to avoid the string being identified as a translation, as in
# RtxtNT(paste(vals ...))

RtxtNT <- Rtxt

VERSION <- "5.6.2"
DATE <- "2026-02-09"

# 091223 Rtxt does not work until the rattle GUI has started, perhaps?
COPYRIGHT <- paste(Rtxt("Copyright"), "(C) 2006-2026 Togaware Pty Ltd.")

# Acknowledgements: Frank Lu has provided much feedback and has
# extensively tested early versions of Rattle. Many colleagues at the
# Australian Taxation Office have used Rattle and made many and
# varied suggestions. These include Anthony Nolan, Stuart Hamilton,
# Liyin Zue, Weiqiang Lin, Robert Williams, Shawn Wicks, Ray Lindsay.

# LICENSE
#
# This files is part of Rattle.
#
# Rattle is open source software: you can redistribute it and/or modify it
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
# along with Rattle. If not, see <https://www.gnu.org/licenses/>.

# STYLE GUIDE
#
#    Use the "_" convention only for Glade variables and functions.
#    Use capitalised verbs for own functions: displayPlotAgain
#    Use dot separated words for variables: list.of.frames, lib.cmd
#    RGtk2 uses the capitalised word convention.
#    Use same names in R code as for the Glade objects.
#    Hide global variables, all capitalised, by including in crv$

# INTERFACE STYLE
#
#    080427 For options like a button to display a model once it has been
#    built or which model builders are available given the nature of
#    the data, we generally toggle the Sensistivity of the widgets
#    appropraitely.
#
#    In general, show all available widgets at any time, but grey out
#    those that can not yet be used because, for example, a model has
#    not yet been built.
#
#    If the functionality is not yet implemented, full stop, then have
#    the interface item(s) not present. This is better than having
#    them greyed out as the expectation is that perhaps there is some
#    way within the interface of getting it not to be greyed out! But
#    displaying future functionality also encourages those with an
#    interest in the greyed out bits to either complain (i.e., I get
#    to know what is wanted) or else help implement them!
#
#    If the functionality is not appropriate in a particular
#    circumstance then don't provide the button. Simply check, in the
#    one place in the code (e.g., when the button is pushed) and pop
#    up an error dialogue.
#
#    This doesn't always work, as in the case of sample where you do
#    want greyed out functionality, but you don't want it to mean not
#    yet implemented.

# BUGS
#
#   Tooltips used to have issues on GNU/Linux. Just fine on
#   MS/Windows.
#
#   The RGtk2 author, Michael Lawrence, notes that most of the GUI
#   functionality in Gnome (i.e., libgnome and libgnomeui) will soon
#   be merged into GTK. At that time, that functionality will be part
#   of RGtk2.

# GLOBALS
#
#   Original design placed state variables into the crs list and
#   global constants into . variables that then moved into the crv
#   list instead, after R CMD check started complaining about possibly
#   unbound variables. The real solution seems to be
#   environments. This was implemented temporarily simply by replacing
#   crv and crs with environments. The list notation then continued to
#   work for them! 090316 Finally removed all <<- assignments into the
#   environments, since, as Chambers (2008) page 124 points out a
#   reference to the environemt ralways refers to the same
#   environment.
#
#   Be aware that the trick of doing
#
# 	crs <- crs
#
#   within functions only works if we <<- assign to crs and don't make
#   use of the value in crs after it might change within the function
#   (or a sub function)! Probably not a good thing to do.

########################################################################
#
# INITIALISATIONS

## overwritePackageFunction <- function(fname, fun, pkg)
## {
##   # 090207 This allows a plugin to easily overwrite any Rattle funtion
##   # with their own functionality. Simply define your own FUN that is
##   # to overwrite the Rattle defined function FNAME. 090517 We do it
##   # this way rather than having to export the function to be
##   # overridden. Note that the override only happens within the
##   # namespace of the package. Thus it does not make sense to use this
##   # overwrite function to overwrite an exported function, since the
##   # overwrite will not be seen externally to the package. 120117
##   # Remove this for now since it could be harmful. Kurt has suggested
##   # only allowing overwriting when 're' is asNamespace('rattle') to
##   # reduce risk of malicious use by other packages.

##   re <- eval(parse(text=sprintf("environment(%s)", pkg)))
##   if (re == asNamespace('rattle')) # NOT RIGHT
##   {
##     unlockBinding(fname, re)
##     assign(fname, fun, re)
##     lockBinding(fname, re)
##   }
## }

toga <- function() browseURL("https://rattle.togaware.com")

########################################################################
# RATTLE Version 2

rattle <- function(csvname=NULL, dataset=NULL, useGtkBuilder=TRUE)
{
  "The rattle GUI has migrated to flutter. Visit https://rattle.togaware.com"
}

rattleReport <- function()
{
}

########################################################################
# Configurable functions - these are here because plugins may want to
# overwrite them.

configureGUI <- function()
{
}

setDefaultsGUI <- function()
{
}

fixMacAndGtkBuilderTypes <- function()
{
}

fixGtkBuilderAdjustments <- function()
{
}

fixTranslations <- function(w=theWidget("rattle_window"))
{
}

translateMenus <- function()
{
}


translateComboBoxes <- function()
{
}

displayWelcomeTabMessage <- function()
{
}

writeCSV <- function(x, file="", ...)
{
  write.csv(x, file=file, row.names=FALSE, ...)
}

rattleTodo <- function(...) cat("Rattle TODO:", ..., "\n")


resetRattle <- function(new.dataset=TRUE)
{
}

########################################################################
# UTILITIES

"%notin%" <- function(x,y) ! x %in% y

not.null <- function(x) ! is.null(x)

uri2file <- function(u)
{
  sub("^file://", "", u)
}

listVersions <- function(file="", ...)
{
  result <- installed.packages()[,c("Package", "Version")]
  row.names(result) <- NULL
  write.csv(result, file=file, ...)
  invisible(result)
}

########################################################################
## Common Dialogs

debugDialog <- function(...)
{
}

infoDialog <- function(...)
{
}

warnDialog <- function(...)
{
}

errorDialog <- function(...)
{
}

questionDialog <- function(...)
{
}

notImplemented <- function(action, window)
{
}

noDatasetLoaded <- function()
{
}

variablesHaveChanged <- function(action)
{
}

# 110703: I used to test if the package name was in the result from
# installed.packages(), but as Brian Ripley points out and from the
# man page for the function itself, installed.packages() is very slow
# on MS/Windows and on networked file systems as it touches a couple
# of files for each package, and with over a thousand packages
# installed that will be a lot of files. So simply check for the
# package using system.file().

package.installed <- function(package) nchar(system.file(package=package)) > 0

packageIsAvailable <- function(pkg, msg=NULL, use.git=FALSE, alt.msg=NULL)
{
  # 160904 XDF TODO Add new arguments use.git and alt.msg. The package
  # dplyrXdf, for example, is available from github rather than
  # CRAN. So use.git is set to the github (or other) URL for
  # installing the package. The package RevoScaleR is not readily
  # available and so we need to have an alternative message to say
  # that this needs to be obtained from Microsoft.

  appname <- ifelse(exists("crv") && ! is.null(crv$appname), crv$appname, "Rattle")
  localmsg <- sprintf(Rtxt("The package '%s' is required to %s.",
                           "It does not appear to be installed.",
                           "This package (and its dependencies) can be installed",
                           "using the following R command:",
                           "\n\ninstall.packages('%s')",
                           "\n\nThis one-time install will allow access to",
                           "the full functionality of %s.",
                           "\n\nWould you like %s to install the package now?"),
                      pkg, msg, pkg, appname, appname)
  if (! package.installed(pkg))
  {
    if (not.null(msg))
    {
      if (questionDialog(localmsg))
      {
        install.packages(pkg)
        return(TRUE)
      }
    }
    else if (not.null(alt.msg))
    {
      infoDialog(alt.msg)
      return(FALSE)
    }
    return(FALSE)
  }
  else
    return(TRUE)
}

sampleNeedsExecute <- function()
{
  # Popup an error dialog if sampling needs to be executed and return
  # TRUE.

  # If sampling is active, make sure there is a sample.

  if (theWidget("data_sample_checkbutton")$getActive()
      && is.null(crs$train))
  {
    errorDialog(Rtxt("Sampling is active but has not been Executed.",
                     "Either ensure you Execute the sampling by clicking",
                     "the Execute button on the Transform tab,",
                     "or else de-activate Sampling on the Data tab."))
    return(TRUE)
  }

  # If sampling is inactive, make sure there is no sample. 080601 Why
  # would I need this test?

###   if (! theWidget("data_sample_checkbutton")$getActive()
###       && not.null(crs$train))
###   {
###     errorDialog("Sampling is inactive but has not been Executed",
###                  "since being made inactive.",
###                  "Please ensure you Execute the Transform tab",
###                  "after de-activating the Sampling on the Transform tab.")
###         return(TRUE)
###   }

  return(FALSE)
}

errorMessageFun <- function(call, result)
{
  # 100109 Generate a message reporting an error in a function call.

  return(sprintf(Rtxt("An error occured in the call to '%s'.",
                      "The error message was:\n\n%s\n\n%s"),
                 call, result, crv$support.msg))
}

errorMessageCmd <- function(call, result)
{
  # 100109 Generate a message reporting an error in a command line.

  return(sprintf(Rtxt("An error occured in the following command:\n\n%s.",
                      "\n\nThe error message was:\n\n%s\n\n%s"),
                 call, result, crv$support.msg))
}

errorReport <- function(cmd, result)
{
  # A standard command error report that is not being captured by
  # Rattle. Eventually, all of these should be identified by Rattle
  # and a sugggestion given as to how to avoid the error.

  errorDialog(errorMessageCmd(cmd, result))
}

########################################################################
#
# Simplify updates to status bar
#

setMainTitle <- function(title=NULL)
{
  standard <- Rtxt("R Data Miner - [Rattle]")
  if (is.null(title))
    theWidget("rattle_window")$setTitle(standard)
  else
  {
    Encoding(title) <- "UTF-8" # 100415 Just in case? Japanese window title not proper in RStat
    theWidget("rattle_window")$setTitle(sub("]",
                                            sprintf(" (%s)]", title),
                                            standard))
  }
}

setStatusBar <- function(..., sep=" ")
{
}

reportTimeTaken <- function(tv, time.taken, model, msg)
{
  # 091224 This is called after building a model to report on how long
  # the build took in the text view, to append the time taken to the
  # log for information purposes, and to update the status bar. At
  # least one of and only one of model or msg must be supplied.

  if (missing(model) && missing(msg) || (!missing(model) && !missing(msg)))
    stop(Rtxt("rattle: reportTimeTaken:",
              "one and only one of model/msg must be supplied."))

  time.msg <- sprintf(Rtxt("Time taken: %0.2f %s"),
                      time.taken, Rtxt (attr(time.taken, "units")))

  # Rtxt("secs") Rtxt("mins") So that the above units gets
  # translated. Note also the gap after Rtxt above to avoid it being
  # picked up as a string to be translated.

  addTextview(tv, "\n", time.msg, textviewSeparator())
  appendLog(time.msg)

  if (missing(msg))
    msg <- sprintf(Rtxt("The %s model has been built."), model)

  setStatusBar(msg, time.msg)
}


collectOutput <- function(command, use.print=FALSE, use.cat=FALSE,
                          width=getOption("width"), envir=parent.frame())
{
  # TODO Should this use cat or print? Cat translates the \n to a
  # newline and doesn't precede the output by [1].  For pretty output
  # with sprintf() you probably want cat(), but if you have a vector
  # of formatted text and you want to look at it (as data), print()
  # would be better.

  owidth <- getOption("width")
  options(width=width)
  if (use.print)
    command <- paste("print(", command, ")", sep="")
  else if (use.cat)
    command <- paste("cat(", command, ")", sep="")

  # 080829 - Let's try out capture.output as a simpler way of doing
  # sink. Seems to work okay!

  if (FALSE)
  {
    zz <- textConnection("commandsink", "w", TRUE)
    sink(zz)
    result <- try(eval(parse(text=command)))#121212, envir=envir))
    sink()
    close(zz)
  }
  else
  {
    result <- try(commandsink <- capture.output(eval(parse(text=command))))#121212, envir=envir)))
  }

  if (inherits(result, "try-error"))
  {
    if (any(grep("cannot allocate vector", result)) ||
        any(grep("vector size specified is too large", result)))
      errorDialog(Rtxt("E141: The dataset is too large for this operation.",
                       "It is terminating now without any output.",
                       "The R Console may contain further information."))
    else
      errorDialog(sprintf(Rtxt("E142: A command has failed\n\n%s\n\n",
                               "The action you requested has not been completed.",
                               "Refer to the R Console for details."),
                          command))
    commandsink <- Rtxt("No output generated.")
  }
  options(width=owidth)
  return(paste(commandsink, collapse="\n"))
}

########################################################################
##
## Miscellaneous Support
##

theWidget <- function(widget)
{
  #crv$rattleGUI <- Global_.rattleGUI # Global - to avoid a "NOTE" from "R CMD check"

  if (crv$useGtkBuilder)
    return(crv$rattleGUI$getObject(widget))
  else
    return(crv$rattleGUI$getWidget(widget))
}

getNotebookPageLabel <- function(nb, page)
{
  # Given a notebook object and a numeric page (from 0 to npages-1),
  # return the label on the tab for that page.

  # 100301 Japanese on MS/Windows returns what might be a Shift-JIS
  # string from nb$getTabLabelText(nb$getNthPage(nb$getCurrentPage()))
  # rather than UTF-8, and so the tab name comparisons fail. For now
  # we assume the tab ordering, and so get the tab page number and
  # then map that to the tab label.

  # 100408 Remove the special code for Japanese - instead, we simply
  # need to ensure the encoding of the string returned from GTK is
  # UTF-8 rather than "unknown". That seems to fix the problem.

  # TODO - Remove the commented code.

  ## if (! isJapanese()) # Test this first to avoid too much testing otherwise.
    label <- nb$getTabLabelText(nb$getNthPage(page))
    Encoding(label) <- "UTF-8"
  ## else if (nb == crv$NOTEBOOK)
  ##   label <- switch(page+1,
  ##                   Rtxt ("Data"),
  ##                   Rtxt ("Explore"),
  ##                   Rtxt ("Test"),
  ##                   Rtxt ("Transform"),
  ##                   Rtxt ("Cluster"),
  ##                   Rtxt ("Associate"),
  ##                   Rtxt ("Predictive"),
  ##                   Rtxt ("Evaluate"),
  ##                   Rtxt ("Log"))
  ## else if (nb == crv$EXPLORE)
  ##   label <- switch(page+1,
  ##                   Rtxt ("summary"),
  ##                   Rtxt ("explot"),
  ##                   Rtxt ("correlation"),
  ##                   Rtxt ("prcomp"),
  ##                   Rtxt ("interactive"))
  ## else if (nb == crv$TRANSFORM)
  ##   label <- switch(page+1,
  ##                   Rtxt ("normalise"),
  ##                   Rtxt ("impute"),
  ##                   Rtxt ("remap"),
  ##                   Rtxt ("outliers"),
  ##                   Rtxt ("cleanup"))
  ## else if (nb == crv$CLUSTER)
  ##   label <- switch(page+1,
  ##                   Rtxt ("kmeans"),
  ##                   Rtxt ("clara"),
  ##                   Rtxt ("hclust"),
  ##                   Rtxt ("biclust"))
  ## else if (nb == crv$MODEL)
  ##   label <- switch(page+1,
  ##                   Rtxt ("rpart"),
  ##                   Rtxt ("ada"),
  ##                   Rtxt ("rf"),
  ##                   Rtxt ("svm"),
  ##                   Rtxt ("glm"),
  ##                   Rtxt ("nnet"),
  ##                   Rtxt ("gbm"),
  ##                   Rtxt ("survival"))
  ## else if (nb == crv$EVALUATE)
  ##   label <- switch(page+1,
  ##                   Rtxt ("confusion"),
  ##                   Rtxt ("lift"),
  ##                   Rtxt ("roc"),
  ##                   Rtxt ("precision"),
  ##                   Rtxt ("sensitivity"),
  ##                   Rtxt ("risk"),
  ##                   Rtxt ("pvo"),
  ##                   Rtxt ("score"),
  ##                   Rtxt ("costcurve"))
  ## else
  ##   # Fall through to the default.
  ##   label <- nb$getTabLabelText(nb$getNthPage(page))

  return(label)
}

getNotebookPage <- function(notebook, label)
{
  # Obtain the notebook page number given its tab's label's text
  # (already translated using Rtxt when it is passed in.  Return NULL
  # if the label is not found.

  for (i in 0:(notebook$getNPages()-1))
   if (getNotebookPageLabel(notebook, i) == label) return(i)
  return(NULL)
}

getCurrentPageLabel <- function(nb)
{
  return(getNotebookPageLabel(nb, nb$getCurrentPage()))
}

isMac <- function()
{
  # 140307 Added to check for GUI tings not migrated back into the Mac
  # GUI XML.

  # Perhaps should use .Platform$OS.type as below for isWindows.
  return(Sys.info()["sysname"] == "Darwin")
}


isWindows <- function()
{
  # The use of .Platform$OS.type is as recommended in the R.version
  # manual page.
  return(.Platform$OS.type == "windows")
}

fixWindowsSlash <- function(s)
{
  if (isWindows()) s <- gsub('\\\\', '/', s)
  return(s)
}

isLinux <- function()
{
  return(.Platform$OS.type == "unix")
}

isJapanese <- function()
{
  # 091222 For plots and pdf export under MS/Windows. Tested by
  # acken_sakakibara@ibi.com

  return(isWindows() && Sys.getlocale("LC_CTYPE") == "Japanese_Japan.932")
}

listBuiltModels <- function(exclude=NULL)
{
  # Build a list of models that have been built.
  models <- c()
  for (m in setdiff(c(crv$PREDICT, crv$DESCRIBE), exclude))
    if (not.null(eval(parse(text=sprintf("crs$%s", m)))))
      models <- c(models, m)
  return(models)
}

########################################################################
## PLOTTING
##
## Callbacks

on_plot_save_button_clicked <- function(action)
{
  # To know which window we are called from we extract the plot
  # number from the window title!!!. This then ensures we save the
  # right device.
  #
  # Also, export to pdf (from Cairo) is not too good it seems. Gets a
  # grey rather than white background. PNG and JPEG look just fine.
  # This is being fixed by Michael Lawrence.

  ttl <- action$getParent()$getParent()$getParent()$getParent()$getTitle()
  savePlotToFileGui(dev.num(ttl))
}

on_plot_copy_button_clicked <- function(action)
{
  ttl <- action$getParent()$getParent()$getParent()$getParent()$getTitle()
  dn <- dev.num(ttl)
  startLog(Rtxt("Copy the plot to the clipboard."))
  appendLog(sprintf(Rtxt("Copy the plot on device %d to the clipboard."), dn),
            sprintf('copyPlotToClipboard(%s)', dn))
  copyPlotToClipboard(dn)
  setStatusBar(sprintf(Rtxt("Plot %d has been copied to the clipboard",
                            "using the PNG format."),
                       dn))
}

on_plot_print_button_clicked <- function(action)
{
  ## To know which window we are called from we extract the plot
  ## number from the window title!!!. This then ensures we save the
  ## right device.

  ttl <- action$getParent()$getParent()$getParent()$getParent()$getTitle()
  dn <- dev.num(ttl)
  startLog(Rtxt("Print the plot."))
  appendLog(sprintf(Rtxt("Send the plot on device %d to the printer."), dn),
            sprintf('printPlot(%s)', dn))
  printPlot(dn)
  setStatusBar(sprintf(Rtxt("Plot %d has been sent to the printer",
                             "using the command: %s."),
                       dn, options("printcmd")))
}

on_plot_close_button_clicked <- function(action)
{
  ttl <- action$getParent()$getParent()$getParent()$getParent()$getTitle()
  dn <- dev.num(ttl)
  dev.off(dn)

  pw <- action$getParentWindow()

  # 100830 "destroy" causes R to crash. So try hide - but does that
  # not release the object and hence accumulates memory usage. Does
  # withdraw do any better?

  # pw$destroy()
  # pw$hide()
  pw$withdraw()
}

dev.num <- function(title)
{
  # 100408 Return the device number for the device with the given
  # title. This was needed because Japanes on MS/Windows was returning
  # a title in some encoding that was not the original, and sub(".* ",
  # "", ttl) was failing.

  Encoding(title) <- "UTF-8"
  return(as.integer(sub(".* ", "", title)))
}


########################################################################

newPlot <- function(pcnt=1)
{
}

########################################################################

copyPlotToClipboard <- function(dev.num=dev.cur())
{
}

savePlotToFileGui <- function(dev.num=dev.cur(), name="plot")
{
}

savePlotToFile <- function(file.name, dev.num=dev.cur())
{
  cur <- dev.cur()
  dev.set(dev.num)
  ext <- get.extension(file.name)
  if (ext == "pdf")
    # Set version to 1.4 since dev.copy from a Cairo device needs
    # this.  It is done automatically with a warning anyhow, but might
    # as well avoid the warning so as not to worry anyone.  091222 Add
    # the test for Japanese to add the family option so we get
    # Japanese fonts. This also kind of works on GNU/Linux but the
    # viewer compains about missing fonts. Cairo_pdf works just fine
    # on GNU/Linux, and if it works also on MS/Windows in Japanese the
    # we will go with that.
    #if (isJapanese())
    #  dev.copy(pdf, file=file.name, width=10, height=10, version="1.4", family="Japan1")
    #else
    # 20260208 gjw cairoDevice no longer available
    # dev.copy(cairoDevice::Cairo_pdf, file=file.name, width=10, height=10)
    dev.copy(pdf, file=file.name, width=10, height=10, version="1.4")
  else if (ext == "png")
    dev.copy(png, file=file.name, width=1000, height=1000)
  else if (ext == "jpg")
    dev.copy(jpeg, file=file.name, width=1000, height=1000)
  else if (ext == "wmf")
    eval(parse(text=sprintf("dev.copy(win.metafile, file='%s', width=10, height=10)", file.name)))
  else
  {
    infoDialog(sprintf(Rtxt("The supplied filename extension '%s'",
                            "is not supported."), ext))
    return(FALSE)
  }
  dev.off()
  dev.set(cur)
  return(TRUE)
}

printPlot <- function(dev.num=dev.cur())
{
  cur <- dev.cur()
  dev.set(dev.num)
  if (isWindows())
    eval(parse(text="dev.print(win.print)"))
  else
    dev.print()
  dev.set(cur)
}

########################################################################

genPlotTitleCmd <- function(..., vector=FALSE)
{
  # 080817 Use month name rather than number - less ambiguous.

  if (! exists("crv"))
  {
    crv <- list()
    crv$appname <- "Rattle"
    crv$verbose <- TRUE
    crv$show.timestamp <- TRUE
  }

  main = paste(...)
  if(vector)
  {
    if (! crv$verbose)
      sub <- ""
    else if (crv$show.timestamp)
      sub <- sprintf("%s %s %s", crv$appname,
                     format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"])
    else
      sub <- sprintf(Rtxt("Generated by %s"), crv$appname)
    return(c(main, sub))
  }
  else
  {
    if (! crv$verbose)
      sub <- ""
    else if (crv$show.timestamp)
      sub <- sprintf(paste('paste("%s", format(Sys.time(),',
                           '"%%Y-%%b-%%d %%H:%%M:%%S"), Sys.info()["user"])'),
                     crv$appname)
    else
      sub <- sprintf('paste("%s")', sprintf(Rtxt("Generated by %s"), crv$appname))

    return(sprintf('title(main="%s",\n    sub=%s)', main, sub))
  }
}

set.cursor <- function(cursor="left-ptr", message=NULL)
{
}

simplifyNumberList <- function(nums)
{
  ## Convert 3 4 6 7 8 9 10 12 14 16 17 18 19 21 to
  ## "3:4, 6:10, 12, 14, 16:19, 21"

  if (length(nums) == 1)
    return(sprintf("%s", nums))
  else if (is.null(nums) || length(nums) == 0)
    return(NULL)

  result <- ""
  start <- nums[1]
  len <- 1

  for (i in 2:length(nums))
  {
    if (nums[i] != start + len)
    {
      if (len == 1)
        result <- sprintf("%s, %d", result, start)
      else
        result <- sprintf("%s, %d:%d", result, start, nums[i-1])
      start <- nums[i]
      len <- 1
    }
    else
      len <- len + 1
  }

  if (len == 1)
    result <- sprintf("%s, %d", result, start)
  else
    result <- sprintf("%s, %d:%d", result, start, nums[i])

  result <- sub('c\\(, ', 'c(', sprintf("c(%s)", result))
  return(result)
}

get.extension <- function(path)
{
  ## Extract and return the extension part of a filename

  parts <- strsplit(path, "\\.")[[1]]
  if (length(parts) > 1)
    last <- parts[length(parts)]
  else
    last <- ""
  last
}

get.stem <- function(path)
{
  # Given a filename PATH extract the basename, and from this, the
  # name without an extension.  090718 If the PATH supplied is a
  # string with no extension than just return the PATH.

  parts <- strsplit(basename(path), "\\.")[[1]]
  if (length(parts) > 1)
    last <- paste(parts[seq_len(length(parts)-1)], collapse=".")
  else
    last <- parts
  last
}

########################################################################
#
# Shared callbacks
#

on_rattle_window_delete_event <- function(action, window)
{
  if (crv$close %in% c("quit", "ask"))
  {
    msg <-sprintf(Rtxt("Do you want to terminate %s?"), crv$appname)
    if (!questionDialog(msg))
      return(TRUE)
    else
      if (crv$close == "quit")
        quit(save="no")
      else
        return(FALSE)
  }
}

close_rattle <- function(action, window)
{
  # 090401 This callback seems to be called after the window is
  # destroyed!!!  So the question serves no purpose... Not clear how
  # to fix that.

  closeRattle()
}

quit_rattle <- function(action, window)
{
  # 080815 This function used to return NULL or "yes" and I always
  # tested whether it's results was NULL. But why not return a
  # logical? Start doing that now, by returning TRUE instead of "yes",
  # and look to return FALSE instead of NULL on a negative response to
  # the question.

  closeRattle(TRUE)
}

closeRattle <- function(ask=FALSE)
{
  if (ask || crv$close %in% c("quit", "ask"))
  {
    msg <- sprintf(Rtxt("Do you want to terminate %s?"), crv$appname)
    if (!questionDialog(msg)) return(FALSE)
  }

  # Don't remove the graphics for now. In moving to the Cairo device,
  # this blanks the device, but does not destroy the containing
  # window. I wonder if there is some way to get a list of the plot
  # windows, and destroy each one?

  # graphics.off() # for (i in dev.list()) dev.off(i)

  # 080523 When this is called as a callback from the destroy signal
  # of the GtkObject, the window has already been destroyed, so no
  # need to try again.

  rw <- theWidget("rattle_window")
  if (not.null(rw)) rw$destroy()

  # Communicate to R that Rattle has finished. This is used by the
  # rattle script on GNU/Linux using the littler package which allows
  # one to use R as a scripting language. But rattle dispatches
  # itself from R, and so normally the script immediately
  # terminates. Instead we can have a loop that checks if crv$rattleGUI
  # is NULL, and when it is we finish! Seems to work.

  crv$rattleGUI <- NULL

  # 080511 Restore options to how they were when Rattle was started.

  options(crv$options)

  # if (crv$tooltiphack) gtkMainQuit() # Only needed if gtkMain is run.

  # 080906 Deal with R not finishing up when rattle is called from
  # littler or R CMD BATCH and we close rather than quit.

  if (crv$close == "quit") quit(save="no")
}

interrupt_rattle <- function(action, window)
{
  # The multicore or fork packages may provide some hope under
  # GNU/Linux, but not MS/Wdinwos. Under MS the Esc seems to send a
  # SIGBREAK to the R process. How to do that?

  infoDialog(Rtxt("This operation is not yet functional."))
}

########################################################################

## General Menu Callbacks

on_rattle_menu_activate <- function(action, window)
{
  browseURL("https://rattle.togaware.com")
}

on_delete_menu_activate <- notImplemented

## on_connectr_toolbutton_clicked <- function(action, window)
## {
##   browseURL("https://connect-r.com/posting.php?mode=post&f=2")
## }

## Map the unchanged glade defaults

on_cut1_activate <- notImplemented

on_about_menu_activate <-  function(action, window)
{
}

configureAbout <- function(ab)
{
  ab["program-name"] <- "Rattle"
  ab$setCopyright(paste(DATE, "\n\n", COPYRIGHT, "\n" ,
                        Rtxt("All rights reserved.")))

#XX#  ab$setLicense(paste("This program (Rattle) is copyright software, owned by Togaware Pty Ltd.",
#XX#                      "\n\nThis program is licensed and distributed by Togaware Pty Ltd",
#XX#                      "\nto XLICENSEEX for up to XNUSERSX users until XEXPIREX.",
#XX#                      "\nThe license number is XSNX.",
#XX#                      "\n\nThe program is made available under the terms of the GNU",
#XX#                      "\nGeneral Public License as published by the Free",
#XX#                      "\nSoftware Foundation; either version 2 of the License, or (at your",
#XX#                      "\noption) any later version. See the file gpl-license in the",
#XX#                      "\ndistribution and at https://www.gnu.org/copyleft/gpl.html for details.",
#XX#                      "\n\nThis program is distributed without any warranty; without even the",
#XX#                      "\nimplied warranty of merchantability or fitness for a particular purpose.",
#XX#                      "\nPlease see the GNU General Public License for more details.",
#XX#                      "\n\nBy using this program the licensee acknowledges that they have",
#XX#                      "\nevaluated the program and accept the program as is."))

}

on_paste1_activate <- notImplemented
on_copy1_activate <- notImplemented

on_tooltips_activate <- function(action, window)
{
}

on_verbose_menuitem_toggled <- function(action, window)
{
  crv$verbose <- theWidget("verbose_menuitem")$getActive()
}

##----------------------------------------------------------------------

## Miscellaneous callbacks

on_notebook_switch_page <- function(notebook, window, page)
{
  ## notebook is the GtkNotebook object.
  ## window is ??.
  ## page is the index of the page switched to.

  #ct <- current_(page)

  switchToPage(page)
}

on_tools_data_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.DATA.NAME))
  switchToPage(crv$NOTEBOOK.DATA.NAME)
}

on_tools_test_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.TEST.NAME))
  switchToPage(crv$NOTEBOOK.TEST.NAME)
}

on_tools_transform_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.TRANSFORM.NAME))
  switchToPage(crv$NOTEBOOK.TRANSFORM.NAME)
}

on_tools_explore_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.EXPLORE.NAME))
  switchToPage(crv$NOTEBOOK.EXPLORE.NAME)
}

on_tools_cluster_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.CLUSTER.NAME))
  switchToPage(crv$NOTEBOOK.CLUSTER.NAME)
}

on_tools_model_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.MODEL.NAME))
  switchToPage(crv$NOTEBOOK.MODEL.NAME)
}

on_tools_evaluate_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.EVALUATE.NAME))
  switchToPage(crv$NOTEBOOK.EVALUATE.NAME)
}

on_tools_log_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.LOG.NAME))
  switchToPage(crv$NOTEBOOK.LOG.NAME)
}

switchToPage <- function(page)
{

  # Blank the status bar whenever we change pages

  setStatusBar()

  # This function used to accept numeric pages, so check for that and
  # convert to the page name rather than the now changing page number
  # (page numbers used to be fixed).

  if (is.numeric(page))
    page <- getNotebookPageLabel(crv$NOTEBOOK, page)

  # Record the current page so when we change we know which was last.

  crs$page <- page

}
