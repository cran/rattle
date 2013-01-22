# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2013-01-07 12:43:39 Graham Williams>
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
    theWidget("log_textview")$modifyFont(pangoFontDescriptionFromString(crv$textview.font))
  
  if (! is.null(crv$log.intro))
    appendTextview("log_textview", crv$log.intro, tvsep=FALSE)

  startLog(paste(sprintf(Rtxt("%s version %s user '%s'"),
                         crv$appname, crv$version, Sys.info()["user"]),
#LOG_LICENSE
                 #sprintf("# Started %s by %s\n\n", Sys.time(), Sys.info()["user"]),
                 "\n\n",
                 Rtxt("# Export this log textview to a file using the Export",
                      "button or the Tools",
                      "\n# menu to save a log of all activity. This facilitates",
                      "repeatability. Exporting",
                      "\n# to file 'myrf01.R', for example, allows us to the",
                      "type in the R Console",
                      "\n# the command source('myrf01.R') to repeat the",
                      "process automatically.",
                      "\n# Generally, we may want to edit the file to",
                      "suit our needs. We can also directly",
                      "\n# edit this current log textview to record",
                      "additional information before exporting.",
                      "\n",
                      "\n# Saving and loading projects also retains this log."),
                 "\n\n",
                 crv$library.command,
                 "\n\n",
                 Rtxt("# This log generally records the process of building a model.",
                      "However, with very",
                      "\n# little effort the log can be used to score a new dataset.",
                      "The logical variable",
                      "\n# 'building' is used to toggle between generating",
                      "transformations, as when building",
                      "\n# a model, and simply using the transformations,",
                      "as when scoring a dataset."),
                 "\n\nbuilding <- TRUE",
                 "\nscoring  <- ! building\n",
                 ifelse(packageIsAvailable("colorspace"),
                        paste("\n",
                              Rtxt("# The colorspace package is used to generate",
                                   "the colours used in plots,",
                                   "if available."),
                              "\n\n",
                              "library(colorspace)", sep=""), ""),
                 "\n\n",
                 Rtxt("# A pre-defined value is used to reset the random seed",
                      "so that results are repeatable."),
                 "\n\ncrv$seed <- ", crv$seed,
                 sep=""))

}

startLog <- function(msg=NULL)
{
  # Output a suitable separator to the log textview, and if there is
  # an optional MSG, display that message, as an introduction to this
  # section.
  
  if (is.null(crs$rattleGUI)) return()

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
  
  if (is.null(crs$rattleGUI)) return()

  if (no.start)
    msg <- paste(sep=sep, start, cont, ...)
  else if (is.null(cont))
    msg <- paste(sep="", crv$start.log.comment, start)
  else
    msg <- paste(sep="", crv$start.log.comment, start, crv$end.log.comment, cont, ...)
  if (length(msg) == 0) msg <-""

  # Always place text at the end, irrespective of where the cursor is.

  log.buf <- theWidget("log_textview")$getBuffer()
  location <- log.buf$getEndIter()$iter

  log.buf$insert(location, msg)
}

exportLogTab <- function()
{
  # Obtain filename to the LOG textview to.
  
  dialog <- gtkFileChooserDialog(Rtxt("Export Log"), NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])
  dialog$setDoOverwriteConfirmation(TRUE)

  if(not.null(crs$dataname))
    dialog$setCurrentName(sprintf("%s_script.R", get.stem(crs$dataname)))

  ff <- gtkFileFilterNew()
  ff$setName(Rtxt("R Files"))
  ff$addPattern("*.R")
  dialog$addFilter(ff)

  ff <- gtkFileFilterNew()
  ff$setName(Rtxt("All Files"))
  ff$addPattern("*")
  dialog$addFilter(ff)
  
  if (dialog$run() == GtkResponseType["accept"])
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

