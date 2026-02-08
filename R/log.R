# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <Sunday 2026-02-08 14:41:48 +1100 Graham Williams>
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
# along with Rattle. If not, see <https://www.gnu.org/licenses/>.

########################################################################
# CALLBACKS

on_log_export_rename_checkbutton_toggled <- function(button)
{
  theWidget("log_export_rename_entry")$setSensitive(button$getActive())
}

initiateLog <- function()
{
}

startLog <- function(msg=NULL)
{
  # Output a suitable separator to the log textview, and if there is
  # an optional MSG, display that message, as an introduction to this
  # section.

  if (is.null(crv$rattleGUI)) return()

  appendLog(paste("\n\n#",
                  paste(rep("=", 71), collapse=""),
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
}

packageProvides <- function(pkg, fun)
{
  return(sprintf(Rtxt("The '%s' package provides the '%s' function."), pkg, fun))
}
