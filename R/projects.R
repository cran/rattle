# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <Sunday 2026-02-08 14:44:36 +1100 Graham Williams>
#
# Project functionality.
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

on_new_activate <- function(action, window)     { newProject()  }
on_new_button_clicked <- function(action, window)  { newProject() }
on_close_activate <- function(action, window)     { closeProject()  }

on_open_activate <- function(action, window)
{
  # Wrap the actual call with a "try" so that the watch cursor turns
  # off even on error.

  setStatusBar()
  set.cursor("watch")
  try(loadProject())
  set.cursor()
}

on_save_menu_activate <- function(action, window)
{
  setStatusBar()
  set.cursor("watch")
  on.exit(set.cursor())
  saveProject()
}

on_save_as_activate <- function(action, window)
{
  setStatusBar()
  set.cursor("watch")
  on.exit(set.cursor())
  saveProject()
}

on_open_button_clicked <- function(action, window)
{
  setStatusBar()
  set.cursor("watch")
  on.exit(set.cursor())
  loadProject()
}

on_save_button_clicked <- function(action, window)
{
  setStatusBar()
  set.cursor("watch")
  on.exit(set.cursor())
  saveProject()
}

########################################################################
# NEW PROJECT

newProject <- function(ask.overwrite=TRUE)
{
  if (not.null(listBuiltModels()) && ask.overwrite)
  {
    if (! questionDialog(Rtxt("You have requested to start a new project.\n\n",
                              "This will clear the current project (dataset",
                              "and models).\n\n",
                              "If you choose not to continue you can save",
                              "the project, and then start a new project.\n\n",
                              "Do you wish to continue, and overwrite the",
                              "current project?")))
      return()
  }

  resetRattle()

  # Clear out the log and re-initialise it.

  setTextviewContents("log_textview", "")
  initiateLog()

  # Ensure data sources are enabled again.

  enableDataSourceFunctions()

  # Reset things that can't be done in resetRattle()

  # 090314 I was finding that the filechooserbutton was being set to a
  # folder with the setFilename, so try using unselectFilename instead.

  #theWidget("data_filechooserbutton")$setFilename("")
  filename <- theWidget("data_filechooserbutton")$getFilename()
  if (not.null(filename)) theWidget("data_filechooserbutton")$unselectFilename(filename)

  theWidget("data_name_combobox")$setActive(-1)

  # TODO Plenty of other things that should be reset as well.

  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.DATA.NAME))
  switchToPage(crv$NOTEBOOK.DATA.NAME)

}

enableDataSourceFunctions <- function(enable=TRUE)
{
  # 080707 Turn the data source options on/off. This is used when
  # loading a project to avoid datasets being loaded. Logically we
  # don't load a new dataset if we have loaded a project. We can
  # always click New to then load a new dataset.

  widgets <- c("data_type_label", "data_csv_radiobutton", "data_arff_radiobutton",
                "data_rdata_radiobutton", "data_rdataset_radiobutton",
                "data_library_radiobutton", "data_odbc_radiobutton",
                "data_corpus_radiobutton")
  for (w in widgets) theWidget(w)$setSensitive(enable)
}

########################################################################
# Close Project
#
# For now, Close is just like New.

closeProject <- newProject

########################################################################
# Save Project

saveProject <- function()
{
}

loadProject <- function()
{
}
