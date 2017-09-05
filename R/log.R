# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2017-07-04 07:33:34 Graham Williams>
#
# Implement LOG functionality.
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

########################################################################
# CALLBACKS

on_log_export_rename_checkbutton_toggled <- function(button)
{
  theWidget("log_export_rename_entry")$setSensitive(button$getActive())
}

initiateLog <- function()
{
  # 100407 Change the font to monospace, like all other textviews.
  
  if (! isJapanese())
    theWidget("log_textview")$modifyFont(RGtk2::pangoFontDescriptionFromString(crv$textview.font))
  
  if (! is.null(crv$log.intro))
    appendTextview("log_textview",
                   paste0("#============================================================\n\n",
                          crv$log.intro),
                   tvsep=FALSE)

  startLog(paste(sprintf(Rtxt("%s version %s user '%s'"),
                         crv$appname, crv$version, Sys.info()["user"]),
#LOG_LICENSE
                 #sprintf("# Started %s by %s\n\n", Sys.time(), Sys.info()["user"]),
    "\n\n",
    Rtxt("# This log captures Rattle interactions as an R script.",
         "\n\n# For repeatability export this log of all activity to a",
         "\n# file using the Export button or the Tools menu. This",
         "\n# script can serve as a starting point for developing your",
         "\n# own scripts. Exporting to a file called 'model.R' will",
         "\n# allow you to type into a new R Console the command",
         "\n#\"source('model.R')\" and so repeat all actions. Generally,",
         "\n# you will want to edit the file to suit your own needs.",
         "\n# You can also edit this log in place to record additional",
         "\n# information before exporting the script.",
         "\n",
         "\n# Note that saving/loading projects retains this log."),
    "\n",
    '\n# We begin most scripts by loading the required packages.',
    '\n# Here are some initial packages to load and others will be',
    '\n# identified as we proceed through the script. When writing',
    '\n# our own scripts we often collect together the library',
    '\n# commands at the beginning of the script here.\n\n',
    crv$library.command,
    '   # Access weather dataset and utilities.',
    '\nlibrary(magrittr) # For the %>% and %<>% pipeline operators.',
    "\n\n",
    Rtxt("# This log generally records the process of building a model.",
         "\n# However, with very little effort the log can also be used",
         "\n# to score a new dataset. The logical variable 'building'",
         "\n# is used to toggle between generating transformations,",
         "\n# when building a model and using the transformations,",
         "\n# when scoring a dataset."),
    "\n\nbuilding <- TRUE",
    "\nscoring  <- ! building",
    # Removed to avoid loading librarys or suggesting such
    # Moving to using namespace :: in the script.
    #ifelse(packageIsAvailable("colorspace"),
    #       paste("\n",
    #             Rtxt("# The colorspace package is used to generate",
    #                  "the colours used in plots,",
    #                  "if available."),
    #             "\n\n",
    #             "library(colorspace)", sep=""), ""),
    "\n\n",
    Rtxt("# A pre-defined value is used to reset the random seed",
         "\n# so that results are repeatable."),
    "\n\ncrv$seed <- ", crv$seed,
    sep=""))
  
}

startLog <- function(msg=NULL)
{
  # Output a suitable separator to the log textview, and if there is
  # an optional MSG, display that message, as an introduction to this
  # section.
  
  if (is.null(crv$rattleGUI)) return()

  appendLog(paste("\n\n#",
                  paste(rep("=", 60), collapse=""),
                  if (not.null(crv$show.timestamp) && crv$show.timestamp)
                  paste("\n# ", crv$appname, " ", Rtxt("timestamp:"), " ",
                        Sys.time(), " ", version$platform, sep=""),
                  sep=""),
          no.start=TRUE)
  if (not.null(msg))
    appendLog(paste(sep="", crv$start.log.comment, msg), no.start=TRUE)
}

appendLog <- function(start, cont=NULL, ..., sep=" ", no.start=FALSE)
{
  # 100330 cont is used to identify whether there is more than a
  # single string to print. If not, then don't include the
  # crv$end.log.comment otherwise there is too much white space in the
  # log.
  
  if (is.null(crv$rattleGUI)) return()

  if (no.start)
    msg <- paste(sep=sep, start, cont, ...)
  else if (is.null(cont))
    msg <- paste(sep="", crv$start.log.comment, start)
  else
    msg <- paste(sep="", crv$start.log.comment, start, crv$end.log.comment, cont, ...)
  if (length(msg) == 0) msg <-""

  # 150712 Remove and Rtxt(...), leaving just ...

  msg <- stringr::str_replace(msg, 'Rtxt\\(([^\\)]*)\\)', '\\1')
  
  # Always place text at the end, irrespective of where the cursor is.

  log.buf <- theWidget("log_textview")$getBuffer()
  location <- log.buf$getEndIter()$iter

  log.buf$insert(location, msg)
}

exportLogTab <- function()
{
  # Obtain filename to the LOG textview to.
  
  dialog <- RGtk2::gtkFileChooserDialog(Rtxt("Export Log"), NULL, "save",
                                 "gtk-cancel", RGtk2::GtkResponseType["cancel"],
                                 "gtk-save", RGtk2::GtkResponseType["accept"])
  dialog$setDoOverwriteConfirmation(TRUE)

  if(not.null(crs$dataname))
    dialog$setCurrentName(sprintf("%s_script.R", get.stem(crs$dataname)))

  ff <- RGtk2::gtkFileFilterNew()
  ff$setName(Rtxt("R Files"))
  ff$addPattern("*.R")
  dialog$addFilter(ff)

  ff <- RGtk2::gtkFileFilterNew()
  ff$setName(Rtxt("All Files"))
  ff$addPattern("*")
  dialog$addFilter(ff)
  
  if (dialog$run() == RGtk2::GtkResponseType["accept"])
  {
    save.name <- dialog$getFilename()
    dialog$destroy()
  }
  else
  {
    dialog$destroy()
    return()
  }

  if (get.extension(save.name) != "R")
    save.name <- sprintf("%s.R", save.name)

  save.text <- getTextviewContent("log_textview")
  if (!theWidget("log_export_comments_checkbutton")$getActive())
    save.text <- gsub("\n\n+", "\n", gsub("#[^\n]*\n", "", save.text))
  if (theWidget("log_export_rename_checkbutton")$getActive())
  {
    nm <- theWidget("log_export_rename_entry")$getText()
    save.text <- gsub("crs\\$", nm, save.text)
  }
  write(save.text, save.name)

  setStatusBar(sprintf(Rtxt("The log has been exported to '%s'."), save.name))
}

packageProvides <- function(pkg, fun)
{
  return(sprintf(Rtxt("The '%s' package provides the '%s' function."), pkg, fun))
}

