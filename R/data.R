# R Data Scientist: Gtk interface to R for Data Science
#
# Time-stamp: <Sunday 2026-02-08 14:51:40 +1100 Graham Williams>
#
# DATA TAB
#
# Copyright (c) 2009-2017 Togaware Pty Ltd
#
# This file is part of Rattle.
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
#
########################################################################
#
# I've removed the Data Entry radio button because why, really, would
# anyone be interested in manually entering some data - use Gnumeric
# or some other spreadsheet to do that.
#
########################################################################
# TODO
#
# 100308 Consider using vcdExtras for displaying categoric data.

########################################################################
# UTILITIES

overwriteModel <- function()
{
  # 080523 If a model exists then warn the user about losing the model
  # on loading a new dataset. Perhaps this could be generalised to any
  # kind of opration that replaces the current model.

  if (not.null(listBuiltModels()))
    return(questionDialog(Rtxt("You have chosen to load/reload the dataset.",
                               "This will clear the current project",
                               "(dataset and models).",
                               "If you choose not to continue",
                               "you can then save the current project before",
                               "loading the dataset.",
                               "\n\nDo you wish to continue and so overwrite",
                               "the current project?")))
  else
    return(TRUE)
}

dataTabShow <- function(...)
{
  # A support function to display the indicated widgets and hide all
  # others, within the data tab. When new widgets are added to the tab
  # through editting the XML file with glade, be sure to add it to the
  # list of known widgets here.

  widgets <- c(...)
  known <- c("data_filename_label",
             "data_filechooserbutton",
             "data_separator_label",
             "data_separator_entry",
             "data_decimal_label",
             "data_decimal_entry",
             "data_header_checkbutton",
             "data_name_label",
             "data_name_combobox",
             "data_odbc_dsn_label",
             "data_odbc_dsn_entry",
             "data_odbc_table_label",
             "data_odbc_table_combobox",
             "data_odbc_limit_label",
             "data_odbc_limit_spinbutton",
             "data_odbc_believeNRows_checkbutton")
  for (w in widgets) theWidget(w)$show()
  for (w in setdiff(known, widgets)) theWidget(w)$hide()
}

showDataViewButtons <- function(action=TRUE)
{
  # Rattle starts up with the View (081228 but not now the Edit)
  # buttons of the Data tab not sensitive. Once data has been loaded
  # we make these tabs sensitive. The ACTION option allows for the
  # case where we might want to make them not sensitive. This option
  # (action=FALSE) is not currently used but cold be in the future,
  # probably when we click New project.

  if (! is.logical(action)) warning(Rtxt("action must be a logical"))

  theWidget("data_view_button")$setSensitive(action)
  theWidget("data_edit_button")$setSensitive(action)
}

urlModTime <- function(filename)
{
  # Return the modification time of the file. Strip out any "file://"
  # prefix to the filename. We note that this will not work for
  # https:// urls.

  return(file.info(gsub("file:///", "/", filename))$mtime)
}

dataNeedsLoading <- function()
{
  # 20080520 Determine whether any of the data source aspects of the
  # Data tab have changed. This is probably limited to checking things
  # relevant to the currently selected data source radio button.

  # 20080712 If there is no dataname stored, then don't bother testing
  # any other conditions. The dataset should be loaded.  090315 Never
  # reload unless there is nothing loaded - that won't work when user
  # changes Filename we want to load.

  if (is.null(crs$dataname)) return(TRUE)

  # 20080712 Check what data source is active, and act
  # appropriately. For those I have yet to work on, simply return TRUE
  # so that at least the data always gets loaded. But this does then
  # wipe out any changes the user makes to selections.

  if (theWidget("data_csv_radiobutton")$getActive() ||
      theWidget("data_arff_radiobutton")$getActive())
  {

    # 100409 Do the URLdecode here, then encode as UTF-8. Previously
    # no UTF-8 and the URLdecode was done 5 separate times below. The
    # mtime below did not URLdecode, but do so now, and make sure it
    # still works. Seems okay.

    filename <- theWidget("data_filechooserbutton")$getUri()
    if (is.null(filename)) return(TRUE)

    filename <- URLdecode(filename)
    Encoding(filename) <- "UTF-8"

    if (is.null(crs$dwd)) return(TRUE)

    if (isWindows())
    {
      # MS/Windows is not case sensitive.
      if (tolower(basename(filename))
          != tolower(crs$dataname) ||
          tolower(dirname(filename)) != tolower(crs$dwd))
        return(TRUE)
    }
    else
    {
      if (basename(filename) != crs$dataname ||
          dirname(filename) != crs$dwd)
        return(TRUE)
    }

    # 20080606 TODO Test if file date has changed, and if so, return
    # TRUE.  Note that file.info does not handle URLs so have to
    # specially handle this. Note that under MS/Windows this returns
    # NA so we don't get a chance to notice updated files.

    now.mtime <- urlModTime(filename)
    if (not.null(crs$mtime) && not.null(now.mtime) && now.mtime > crs$mtime)
      return(TRUE)

  }

  # 20170414 Should we be using else here to slightly reduce
  # testing. Leave as is for now in case there was a reason that else
  # was not used.

  if (theWidget("data_rdataset_radiobutton")$getActive())
  {
    dataname <- theWidget("data_name_combobox")$getActiveText()

    if (is.null(dataname) || crs$dataname != dataname)
      return(TRUE)

    # 20170414 Add an extra check to determine if the R dataset itself
    # has been changed as might happen if the user modifies the data
    # frame themselves whilst it is also loaded into Rattle. We
    # introduce a checksum to manage this using bitops::cksum(). This
    # could take a few seconds so let's try it out and see if there is
    # a significant new lag in loading a dataset!

    if (crs$cksum != (dataname %>%
                        get() %>%
                        paste(collapse="") %>%
                        paste(paste(names(get(dataname)), collapse="")) %>%
                        bitops::cksum()))
      return(TRUE)
  }

  if (theWidget("data_library_radiobutton")$getActive())
  {
    dataname <- theWidget("data_name_combobox")$getActiveText()
    if (is.null(crs$datapkg) || is.null(dataname))
      return(TRUE)
    adsname <- gsub('([^ :]*).*$', '\\1', unlist(strsplit(dataname, ":"))[1])
    dspkg <- unlist(strsplit(dataname, ":"))[2]
    if (crs$dataname != adsname
        || crs$datapkg != dspkg)
      return(TRUE)
  }

  if (theWidget("data_rdata_radiobutton")$getActive())
  {
    dataname <- theWidget("data_name_combobox")$getActiveText()

    if (is.null(dataname) || crs$dataname != dataname) return(TRUE)
  }

  if (theWidget("data_odbc_radiobutton")$getActive())
  {
   table <- theWidget("data_odbc_table_combobox")$getActiveText()

   if (is.null(table) || crs$dataname != table) return(TRUE)
  }

  if (theWidget("data_corpus_radiobutton")$getActive())
  {
    filename <- theWidget("data_corpus_location_filechooserbutton")$getUri()
    if (is.null(filename)) return(TRUE)
    return(TRUE) # Always reload for now.
  }

  if (theWidget("data_script_radiobutton")$getActive())
  {
    return(TRUE)
  }

  # Return FALSE if we did not detect any changes.

  return(FALSE)
}

updateFilenameFilters <- function(button, fname)
{
}

newSampling <- function()
{
  return(crv$appname != "RStat")
}

validateSampleEntry <- function()
{
  sampling <- parseSampleEntry()

  result <- TRUE

  if (sampling[1] == 0)
  {
    errorDialog(Rtxt("A training set partition of 0 does not make sense.",
                     "\n\nPlease choose a non-zero, positive percentage, up to 100."))
    result <- FALSE
  }
  else if (any(sampling < 0))
  {
    errorDialog(Rtxt("A percentage of less than 0 for the partition",
                     "does not make sense.",
                     "\n\nPlease choose percentages in the range 0-100."))
    result <- FALSE
  }
  else if (sum(sampling) != 100)
  {
    errorDialog(sprintf(Rtxt("The sum of the partition proportions does not add",
                             "to 100 (percent): %d + %d + %d = %d.",
                             "\n\nPlease rectify."),
                        sampling[1], sampling[2], sampling[3], sum(sampling)))
    result <- FALSE
  }

  return(result)
}

parseSampleEntry <- function()
{
  ptext <- theWidget("data_sample_entry")$getText()

  splitter <- function(x) as.integer(strsplit(x, "/")[[1]])

  if (! nchar(ptext))
    partition <- splitter(crv$default.sample)
  else
    partition <- splitter(ptext)

  if (length(partition) == 1)
    partition <- c(partition, 0, 100-partition)
  else if (length(partition) == 2)
    partition <- c(partition[1], 100-sum(partition), partition[2])

  return(partition)
}

getTrainingPercent <- function()
{
  return(parseSampleEntry()[1])
}

#-----------------------------------------------------------------------
# These are for handling protos (or envs for now). Moved into package
# container.

whichNumerics <- function(data)
{
  names(data)[sapply(data, is.numeric)]
}

setupDataset <- function(env, seed=NULL)
{
  # We assume the following dataset specific variables exist in env
  #   data            This is the actual data frame containing the dataset
  #   target          The single target variable for prediction
  #   [risk]          The single risk variable
  #   [inputs]        The other variables used as inputs to predictive model
  #   [ignore]        This overrides inputs if it is given.
  # Then we add the following variables to env
  #   vars             Variables used for modelling
  #   numerics         The numeric vars within inputs
  #   nobs             The number of observations
  #   ninputs          The number of input variables
  #   form             Formula for building models
  #   train            A 70% training dataset

  if (! is.null(seed)) set.seed(seed)

  evalq({
    if (! exists("risk", inherits=FALSE))
      risk <- NULL
    if (exists("ignore", inherits=FALSE) && ! exists("inputs", inherits=FALSE))
      inputs <- setdiff(names(data), c(target, risk, ignore))
    if (! exists("inputs", inherits=FALSE))
      inputs <- setdiff(names(data), c(target, risk))
    vars <- c(inputs, target)
    ninputs <- length(inputs)
    nobs <- nrow(data)
    numerics <- whichNumerics(data[inputs])
    form <- as.formula(paste(target, "~ ."))
    train <- sample(nobs, 0.7*nobs)
    test <- setdiff(1:nobs, train)
    na.obs <- attr(na.omit(data[vars]), "na.action")
    train.na.omit <- setdiff(train, na.obs)
    test.na.omit <- setdiff(test, na.obs)

    time.stamp <- date()

  }, env)
}



########################################################################
# CALLBACKS

on_data_csv_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    if (crv$mrs)
      dataTabShow("data_filename_label",
                  "data_filechooserbutton",
                  "data_separator_label",
                  "data_separator_entry",
                  "data_decimal_label",
                  "data_decimal_entry",
                  "data_header_checkbutton",
                  "data_xdf_checkbutton")
    else
      dataTabShow("data_filename_label",
                  "data_filechooserbutton",
                  "data_separator_label",
                  "data_separator_entry",
                  "data_decimal_label",
                  "data_decimal_entry",
                  "data_header_checkbutton")
    updateFilenameFilters("data_filechooserbutton", "CSV")
    if (not.null(crs$data.tab.csv.filename))
      theWidget("data_filechooserbutton")$setUri(crs$data.tab.csv.filename)
  }
  else
  {
    crs$data.tab.csv.filename <- theWidget("data_filechooserbutton")$getUri()
  }
}

on_data_arff_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    dataTabShow("data_filename_label",
                "data_filechooserbutton")
    updateFilenameFilters("data_filechooserbutton", "ARFF")
    if (not.null(crs$data.tab.arff.filename))
      theWidget("data_filechooserbutton")$setUri(crs$data.tab.arff.filename)
  }
  else
  {
    crs$data.tab.arff.filename <- theWidget("data_filechooserbutton")$getUri()
  }
}

on_data_rdata_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    dataTabShow("data_filename_label",
                "data_filechooserbutton",
                "data_name_label",
                "data_name_combobox")
    updateFilenameFilters("data_filechooserbutton", "Rdata")
    cbox <- theWidget("data_name_combobox")
    cbox$getModel()$clear()
    if (not.null(crs$data.tab.rdata.filename))
      theWidget("data_filechooserbutton")$setUri(crs$data.tab.rdata.filename)
    if (not.null(crs$data.tab.rdata.active))
    {
      theWidget("data_name_combobox")$setActive(crs$data.tab.rdata.active)
    }
  }
  else
  {
    crs$data.tab.rdata.filename <- theWidget("data_filechooserbutton")$getUri()
    crs$data.tab.rdata.active <- theWidget("data_name_combobox")$getActive()
  }
}

on_data_rdataset_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    dataTabShow("data_name_label", "data_name_combobox")
    updateRDatasets(current=crs$data.tab.rdataset.name)
  }
  else
  {
    crs$data.tab.rdataset.name <- theWidget("data_name_combobox")$getActiveText()
  }
}

on_data_corpus_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    crv$DATA.NOTEBOOK$setCurrentPage(crv$DATA.CORPUS.TAB)
  }
  else
  {
    crv$DATA.NOTEBOOK$setCurrentPage(crv$DATA.CSV.TAB)
  }
}

# 20080907 Trying to get an event that will auto update the combobox
# without having to move to another radio button and then back again.

on_data_name_combobox_button_press_event <- function(button)
{
  print("Button Press")
  updateRDatasets()
}

on_data_name_combobox_enter_notify_event <- function(button)
{
  print("Enter Notify")
  updateRDatasets()
}

on_data_name_combobox_focus <- function(button)
{
  print("Focus")
  updateRDatasets()
}

on_data_name_combobox_set_focus_child<- function(direction, data)
{
  print("Focus Child")
  #print(direction)
  print(data)
  #updateRDatasets()
}

on_data_name_combobox_focus_in_event<- function(direction, data)
{
  print("Focus In")
  #print(direction)
  #updateRDatasets()
}

#

on_data_library_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    dataTabShow("data_name_label", "data_name_combobox")
    updateDataLibrary(crs$data.tab.library.name)
  }
  else
  {
    crs$data.tab.library.name <- theWidget("data_name_combobox")$getActiveText()
  }
}

on_data_odbc_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    dataTabShow("data_odbc_dsn_label",
                "data_odbc_dsn_entry",
                "data_odbc_table_label",
                "data_odbc_table_combobox",
                "data_odbc_limit_label",
                "data_odbc_limit_spinbutton",
                "data_odbc_believeNRows_checkbutton")
}

updateRDatasets <- function(current=NULL, cbox.name="data_name_combobox")
{
  # Update a combo box with just the available data frames and matrices.

  set.cursor("watch", Rtxt("Determining the available datasets...."))

  # 130126 We might be able to use get.objects("data.frame") here?

  dl <- unlist(sapply(ls(sys.frame(0)),
                      function(x)
                      {
                        cmd <- sprintf(paste("is.data.frame(%s) ||",
                                             'inherits(%s,',
                                             '"sqlite.data.frame")'), x, x)
                        var <- try(ifelse(eval(parse(text=cmd), sys.frame(0)),
                                          x, NULL), silent=TRUE)
                        if (inherits(var, "try-error"))
                          var <- NULL
                        return(var)
                      }))

  cbox <- theWidget(cbox.name)

  cbox$getModel()$clear()
  if (not.null(dl))
  {
    lapply(dl, cbox$appendText)

    # Set the selection to that which was is supplied.

    if (not.null(current) && current %in% dl)
      cbox$setActive(which(sapply(dl, function(x) x==current))[1]-1)
  }
  set.cursor(message=Rtxt("Data Names updated."))
}

on_data_target_survival_radiobutton_toggled <- function(button)
{
  # 091206 When the Survival radio button is toggled, change the names
  # of the Target/Risk columns to match the paradigm.

  target <- theWidget("select_treeview")$getColumn(crv$COLUMN["target"])
  risk <- theWidget("select_treeview")$getColumn(crv$COLUMN["risk"])

  if (button$getActive())
  {
    target$setTitle(Rtxt("Time"))
    risk$setTitle(Rtxt("Status"))
  }
  else
  {
    target$setTitle(Rtxt("Target"))
    risk$setTitle(Rtxt("Risk"))
  }
}

########################################################################
# EXECUTE

executeDataTab <- function(csvname=NULL)
{
  # Dispatch to the task indicated by the selected radio button within
  # the Data tab. 20090315 Previously I tested if there was a change
  # to the data source (with dataNeedsLoading) but this continually
  # got complicated between different OS and different data sources,
  # etc. So now we never reload a dataset, unless no dataset is
  # loaded. To load a new dataset, click New project first. Unless the
  # data type label is not sensitive (i.e., we have loaded a project),
  # simply update the variable roles without reloading the data.

#  if (not.null(csvname))
#  {
#    if (! executeDataCSV(csvname)) return(FALSE)
#  }
  if (theWidget("data_type_label")$isSensitive() && dataNeedsLoading())
  {
    if (theWidget("data_csv_radiobutton")$getActive())
    {
      if (! executeDataCSV(csvname)) return(FALSE)
    }
    else if (theWidget("data_arff_radiobutton")$getActive())
    {
      if (! executeDataARFF()) return(FALSE)
    }
    else if (theWidget("data_odbc_radiobutton")$getActive())
    {
      if (! executeDataODBC()) return(FALSE)
    }
    else if (theWidget("data_rdata_radiobutton")$getActive())
    {
      if (! executeDataRdata()) return()
    }
    else if (theWidget("data_rdataset_radiobutton")$getActive())
    {
      if (! executeDataRdataset()) return()
    }
    else if (theWidget("data_library_radiobutton")$getActive())
    {
      if (! executeDataLibrary()) return()
    }
    else if (theWidget("data_corpus_radiobutton")$getActive())
    {
      if (! executeDataCorpus()) return()
    }
    else if (theWidget("data_script_radiobutton")$getActive())
    {
      if (! executeDataScript()) return()
    }
    else
      return()

    # Update the select treeview. This is done on a Data execute only
    # when a new dataset has been loaded. If the user has simply
    # changed some of the roles or the sampling then we do not do a
    # reset, just an update.

    createVariablesModel(colnames(crs$dataset))

    # Whether we have changed the dataset or not we need to generate the
    # sample and then record the variable roles.

    # Turn sampling on, set range bounds and generate the default 70%
    # sample. Do the range bounds first since otherwise the value gets
    # set back to 1. Also, need to set both the percentage and the count
    # since if the old percentage is 70 and the new is 70, then no
    # change in value is noticed, and thus the count is not
    # automatically updated.

    # 090315 Sampling should be on by default. I had a test here
    # "!is.null(RATTLE.SCORE.IN)" which, after cleaning up the
    # handling of global variables, is now FALSE, whereas previously
    # it must have been TRUE. Simply set to TRUE here until we find
    # why that was being done. Might need another crv tuning
    # parameter.

    theWidget("data_sample_checkbutton")$setActive(TRUE)

    # 090513 Reset the default sample size percentage and ensure it
    # holds (hence we need more than just setting the percentage spin
    # button.
    nrows <- nrow(crs$dataset)
    per <- crv$default.train.percentage
    srows <- round(nrows * per / 100)
    theWidget("sample_count_spinbutton")$setRange(1,nrows)
    theWidget("sample_count_spinbutton")$setValue(srows)
    theWidget("sample_percentage_spinbutton")$setValue(per)
    theWidget("data_sample_entry")$setText(crv$default.sample)
  }
  else
    resetRattle(new.dataset=FALSE)

  # 090416 Move the following from the above if branch to here. Reset
  # the sampling options here, except for whether sampling is
  # on/off. Thus, on loading a new dataset, sampling is set on
  # above. But if we modify the dataset external to Rattle, we want to
  # set new parameters here, yet leave the sampling checkbutton as it
  # was. The extra settings here are often redundant, but needed for
  # the "modified in R" case. 090513 Though now that I have this code
  # both here and above, we might need to revist the logic!
  #
  # We set range bounds and generate the default 70% sample. Do the
  # range bounds first since otherwise the value gets set back to
  # 1. Also, need to set both the percentage and the count since if
  # the old percentage is 70 and the new is 70, then no change in
  # value is noticed, and thus the count is not automatically updated,
  # even if the number of rows has been changed.

  nrows <- nrow(crs$dataset)
  # 090513 Remove the resetting of the sample size to 70 from here,
  # but get the current value. Otherwise, the sample size is always
  # reset to 70 on each Execute of the Data tab - not desired. Now
  # need to only reset it to 70 on loading a new dataset.

  if (newSampling())
    per <- getTrainingPercent()
  else
    per <- theWidget("sample_percentage_spinbutton")$getValue()
  srows <- round(nrows * per / 100)
  theWidget("sample_count_spinbutton")$setRange(1,nrows)
  theWidget("sample_count_spinbutton")$setValue(srows)
  theWidget("sample_percentage_spinbutton")$setValue(per)

  crv$DATA.DISPLAY.NOTEBOOK$setCurrentPage(crv$DATA.DISPLAY.TREEVIEW.TAB)

#  else
#  {
#    resetRattle(new.dataset=FALSE)
#
#    if (dataNeedsLoading())
#    {
#
#      # Just duplicate above for now to get this working.
#      createVariablesModel(colnames(crs$dataset)) # BUT THIS REVERTS TO DEFAULTS
#      nrows <- nrow(crs$dataset)
#      per <- 70
#      srows <- round(nrows * per / 100)
#      theWidget("data_sample_checkbutton")$setActive(not.null(RATTLE.SCORE.IN))
#      theWidget("sample_count_spinbutton")$setRange(1,nrows)
#      theWidget("sample_count_spinbutton")$setValue(srows)
#      theWidget("sample_percentage_spinbutton")$setValue(per)
#    }
#
#  }

  # TODO 20080520 Change the name to updateSample.

  ## 20080603 NOT NEEDED AS DONE IN executeSelectTab
  ## executeSelectSample()

  # Execute the SELECT tab. Changes have bene made and we need to
  # ensure the cached role variables are updated, or else we might see
  # unexpected warnings about changes having been made but not
  # EXECTUEd. [071125]

  if (theWidget("data_sample_checkbutton")$getActive() &&
      ! validateSampleEntry()) return(FALSE)

  # TODO 20080520 Change the name to updateRoles.

  setGuiDefaultsSurvival() # 100505 Moved here from below

  executeSelectTab()
  resetTestTab()
  resetExploreTab()

# 100505 Move to before executeSelectTab, ohterwise the labels get set
# back to stating no variables selected.
 # setGuiDefaultsSurvival()

  # Set the risk label appropriately.

  theWidget("evaluate_risk_label")$setText(crs$risk)

  # Enable the Data View and Edit buttons.

  showDataViewButtons()

  return()
}

#-----------------------------------------------------------------------
# EXECUTE DATA CSV

executeDataCSV <- function(filename=NULL)
{
}

########################################################################
# OLD DATA TAB STUFF MIGRATING TO THE ABOVE
#

on_data_view_button_clicked <- function(button)
{
  viewData()
}

on_data_edit_button_clicked <- function(button)
{
  editData()
}

on_data_filechooserbutton_file_set <- function(button)
{
  # When the filename has been changed on the Data tab check if
  # further action is required. If RData File is active, then load the
  # corresponding .Rdata file and extract the dataset names to be
  # chosen from.

  if (theWidget("data_rdata_radiobutton")$getActive())
    updateRDataNames()
}

updateRDataNames <- function(filename=NULL)
{
  # Collect relevant data

  filename <- theWidget("data_filechooserbutton")$getFilename()
  crs$dwd <- dirname(filename)
  crs$mtime <- urlModTime(filename)

  # Fix filename for MS - otherwise eval/parse strip the \\.

  if (isWindows()) filename <- gsub("\\\\", "/", filename)

  # Generate commands to read the data and then display the structure.

  load.cmd <- sprintf('crs$rdata.datasets <- load("%s")', filename)

  # Start logging and executing the R code.

  startLog()

  appendLog(Rtxt("Load an RData file containing R objects."), load.cmd)
  set.cursor("watch", Rtxt("Loading the RData file..."))
  eval(parse(text=load.cmd), .GlobalEnv) # Env so datasets are globally available.

  # Add new data frames to the combo box.

  combobox <- theWidget("data_name_combobox")
  if (not.null(crs$rdata.datasets))
  {
    combobox$getModel()$clear()
    lapply(crs$rdata.datasets, combobox$appendText)
  }

  setStatusBar(Rtxt("The list of available datasets has been updated",
                    "from the supplied data file.",
                    "Choose one dataset from the Data Name box."))
}

#-----------------------------------------------------------------------
# DATA LIBRAY
#

# 080522 Migrated this from the old interface to the new
# interface. Maybe this is now called whenever the Library radio
# button is activated.
#
# OLD: Update the library combo box with all of the available
# datasets. Can take a little time the first time to generate the
# list. I've associated this with the focus callback, but then it is
# called also when it loses focus!!!

updateDataLibrary <- function(current=NULL)
{
  # OLD: TODO How to tell that this is a "gain focus" action and not a
  # "lose focus" action, since we only want to build the list on
  # gaining focus.

  data.name.combobox <- theWidget("data_name_combobox")

  # Record the current selection so that we can keep it as the default.

  if (is.null(current)) current <- data.name.combobox$getActiveText()

  ## if (not.null(current)) return()

  # This could take a little while, so use to watch cursor to indicate
  # we are busy.

  set.cursor("watch", Rtxt("Determining the available datasets from all packages...."))

  # 090418 Suppress warnings about datasets having moved to 'datasets'

  opt <- options(warn=-1)
  da <- data(package = .packages(all.available = TRUE))
  options(opt)

  dl <- sort(paste(da$results[,'Item'], ":", da$results[,'Package'],
                   ":", da$results[,'Title'], sep=""))
  # Add the entries to the combo box.

  data.name.combobox$getModel()$clear()
  if (not.null(dl))
  {
    lapply(dl, data.name.combobox$appendText)

    # Set the selection to that which was already selected, if possible.

    if (not.null(current) && current %in% dl)
      data.name.combobox$setActive(which(sapply(dl, function(x) x==current))[1]-1)
  }

  set.cursor(message="")

}

#-----------------------------------------------------------------------

open_odbc_set_combo <- function(button)
{
  openODBCSetTables()
}

openODBCSetTables <- function()
{
  # This is for use in the callback for when the ODBC DSN name has
  # changed (associated with the "activate" signal).  Load the known
  # tables from the specified ODBC database. The ODBC connection will
  # be opened and queried for the list of tables.

  # Obtain the name of the DSN.

  DSNname <- theWidget("data_odbc_dsn_entry")$getText()

  # Check if we should believe the number of rows.

  bnumrows <- sprintf(", believeNRows=%s",
                      ifelse(theWidget("data_odbc_believeNRows_checkbutton")$getActive(),
                             "TRUE", "FALSE"))

  # Generate commands to connect to the database and retrieve the tables.

  lib.cmd <- sprintf("library(RODBC)")
  connect.cmd <- sprintf('crs$odbc <- odbcConnect("%s"%s)', DSNname, bnumrows)
  tables.cmd  <- sprintf('crs$odbc.tables <- sqlTables(crs$odbc)$TABLE_NAME')

  # Ensure the RODBC library is available or else we can not support ODBC.

  if (! packageIsAvailable("RODBC", Rtxt("connect to an ODBC database"))) return(FALSE)

  startLog(Rtxt("Open an ODBC connection."))

  appendLog(Rtxt("Require the RODBC package."), lib.cmd)
  # 140906 Move to using namespaces within the code, though still
  # expose the interactive commands.
  #set.cursor("watch")
  #eval(parse(text=lib.cmd))
  #set.cursor()

  # Close all currently open channels. This assumes that the user is
  # not openning channels themselves. It could be a bad choice, but
  # assume we are addressing the usual Rattle user.

  # RODBC::odbcCloseAll()

  appendLog(Rtxt("Open the connection to the ODBC service."), connect.cmd)
  result <- try(eval(parse(text=connect.cmd)))
  if (inherits(result, "try-error"))
  {
    errorDialog(Rtxt("The attempt to open the ODBC connection failed.",
                     "Please check that the DSN is correct.",
                     "See the R Console for further details."))
    return(FALSE)
  }

  appendLog(Rtxt("Load the names of available tables."), tables.cmd)
  set.cursor("watch")
  result <- try(eval(parse(text=tables.cmd)))
  set.cursor()
  if (inherits(result, "try-error"))
  {
    errorDialog(Rtxt("The attempt to query the ODBC connection failed.",
                     "Please check that the DSN is correct.",
                     "See the R Console for further details."))
    return(FALSE)
  }

  # Add list of tables to the combo box.

  combobox <- theWidget("data_odbc_table_combobox")
  if (not.null(crs$odbc.tables))
  {
    combobox$getModel()$clear()
    lapply(crs$odbc.tables, combobox$appendText)
  }

  setStatusBar(Rtxt("ODBC connection to database established. Now select a table."))

  return(TRUE)
}

#----------------------------------------------------------------------
#
# Execution
#
resetVariableRoles <- function(variables, nrows, input=NULL, target=NULL,
                               risk=NULL, ident=NULL, ignore=NULL, weight=NULL,
                               zero=NULL, mean=NULL,
                               boxplot=NULL,
                               hisplot=NULL, cumplot=NULL, benplot=NULL,
                               barplot=NULL, dotplot=NULL, mosplot=NULL, paiplot=NULL,
                               resample=TRUE, autoroles=TRUE)
{
  # Update the SELECT treeview with the dataset variables.

  createVariablesModel(variables, input, target, risk, ident, ignore,
                       weight, zero, mean, boxplot, hisplot, cumplot,
                       benplot, barplot, dotplot, mosplot, paiplot,
                       autoroles=autoroles)

  if (resample)
  {
    # Turn sampling on, set range bounds and generate the default 70%
    # sample. Do the range bounds first since otherwise the value gets
    # set back to 1. Also, need to set both the percentage and the
    # count since if the old percentage is 70 and the new is 70, then
    # no change in value is noticed, and thus the count is not
    # automatically updated.

    per <- 70
    srows <- round(nrows * per / 100)
    theWidget("data_sample_checkbutton")$setActive(TRUE)
    theWidget("sample_count_spinbutton")$setRange(1,nrows)
    theWidget("sample_count_spinbutton")$setValue(srows)
    theWidget("sample_percentage_spinbutton")$setValue(per)
    theWidget("data_sample_entry")$setText(crv$default.sample)

    executeSelectSample()
  }

  # Execute the SELECT tab. Changes have bene made and we need to
  # ensure the cached role variables are updated, or else we might see
  # unexpected warnings about changes having been made but not
  # EXECTUEd. [071125]

  executeSelectTab(resample)

  # Set the risk label appropriately.

  theWidget("evaluate_risk_label")$setText(crs$risk)
}

resetDatasetViews <- function(input, target, risk, ident, ignore, weight=NULL)
{

  # Reset the treeviews.

  theWidget("select_treeview")$getModel()$clear()
  theWidget("impute_treeview")$getModel()$clear()
  theWidget("categorical_treeview")$getModel()$clear()
  theWidget("continuous_treeview")$getModel()$clear()

  # Recreate the treeviews, setting the roles as provided.

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset),
                     input=input, target=target, risk=risk,
                     ident=ident, ignore=ignore, weight=weight,
                     resample=FALSE, autoroles=FALSE)

}

executeDataScript <- function()
{
  setStatusBar(Rtxt("The script option is not yet implemented."))
  return(FALSE)
}

executeDataARFF <- function()
{

  if (!exists("getRversion", baseenv()) || getRversion() <= "2.4.0")
  {
    infoDialog(Rtxt("Support for ARFF is only available in R 2.5.0 and beyond."))
    return(FALSE)
  }

  # Collect relevant data

  filename <- theWidget("data_filechooserbutton")$getUri()

  # If no filename is given then return without doing anything.

  if (is.null(filename))
  {
    errorDialog(Rtxt("No ARFF Filename has been chosen yet.",
                     "You must choose one before execution."))
    return(FALSE)
  }

  filename <- URLdecode(filename)

  crs$dwd <- dirname(filename)
  crs$mtime <- urlModTime(filename)

  # We need the foreign package to read ARFF data.

  if (! packageIsAvailable("foreign", Rtxt("read an ARFF dataset"))) return(FALSE)
  lib.cmd <- "library(foreign, quietly=TRUE)"

  # If there is a model warn about losing it.

  if (! overwriteModel()) return(FALSE)

  # Fix filename for MS - otherwise eval/parse strip the \\.

  if (isWindows()) filename <- gsub("\\\\", "/", filename)

  # Generate commands to read the data and then display the structure.

  read.cmd <- sprintf('crs$dataset <- read.arff("%s")', filename)
  str.cmd  <- "str(crs$dataset)"

  # Start logging and executing the R code.

  startLog()
  ##theWidget(TV)$setWrapMode("none") # On for welcome msg
  ##resetTextview(TV)

  appendLog(packageProvides("foreign", "read.arff"), lib.cmd)
  eval(parse(text=lib.cmd))

  appendLog(Rtxt("Load an ARFF file."), read.cmd)
  resetRattle()
  eval(parse(text=read.cmd))
  crs$dataname <- basename(filename)
  setMainTitle(crs$dataname)

  # appendLog(Rtxt("Display a simple summary (structure) of the dataset."), str.cmd)
  ##appendTextview(TV, sprintf("Structure of %s.\n\n", filename),
  ##                collectOutput(str.cmd))

  ## Update the select treeview and samples.

##  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset))

  # Enable the Data View button.

##  showDataViewButtons()

  setStatusBar(sprintf(Rtxt("The ARFF data has been loaded: %s."), crs$dataname))

  return(TRUE)
}

executeDataODBC <- function()
{
  # Retrieve data from a data source name (DSN) as provided through
  # the data_odbc_dsn_entry. Note that there is no standard LIMIT
  # option in SQL, but it is LIMIT in Teradata, so perhaps we go with
  # that for now?

  dsn.name <- theWidget("data_odbc_dsn_entry")$getText()
  table <- theWidget("data_odbc_table_combobox")$getActiveText()
  row.limit <- theWidget("data_odbc_limit_spinbutton")$getValue()
  believe.nrows <- theWidget("data_odbc_believeNRows_checkbutton")$getActive()
  # warn.many <- theWidget("data_odbc_warnmany_checkbutton")$getActive()
  sql.query <- "" # theWidget("odbc_sql_entry")$getText()

  # If the ODBC channel has not been openned, then tell the user how
  # to do so.

  if (!inherits(crs$odbc, "RODBC"))
  {
    errorDialog(Rtxt("A connection to an ODBC data source name (DSN) has not been",
                     "established. Please enter the DSN and press the Enter key.",
                     "This will also populate the list of tables to choose from.",
                     "After establishing the connection you can choose a table",
                     "or else enter a specific SQL query to retrieve a dataset."))
    return(FALSE)
  }

  # Error if no table from the database has been chosen.

  if (sql.query == "" && is.null(table))
  {
    errorDialog(Rtxt("No table nor SQL query has been specified.",
                     "Please identify the name of the table you wish to load.",
                     "All tables in the connected database are listed",
                     "once a connection is made.",
                     "\n\nAlternatively, enter a query to retrieve a dataset."))
    return(FALSE)
  }

  # If there is a model warn about losing it.

  if (! overwriteModel()) return(FALSE)

  if (sql.query != "")
    sql <- sql.query
  else
  {
    sql <- sprintf("SELECT * FROM %s", table)
    if (row.limit > 0) sql <- paste(sql, "LIMIT", row.limit)
  }

  #assign.cmd <- "crs$dataset <- sqlFetch(crs$odbc, table)"
  assign.cmd <- paste("crs$dataset <- sqlQuery(crs$odbc, ", '"', sql, '"',
                      ifelse(believe.nrows, "", ", believeNRows=FALSE"),
                      ")", sep="")
  str.cmd  <- "str(crs$dataset)"

  if (row.limit == 0)
  {
    # Double check with the user if we are about to extract a large
    # number of rows.

    numRows <- 0 # RODBC::sqlQuery(crs$odbc, sprintf("SELECT count(*) FROM %s", table))
    if (crv$odbc.large != 0 && numRows > crv$odbc.large)
      if (! questionDialog(sprintf(Rtxt("You are about to extract %s",
                                        "rows from the table %s",
                                        "of the %s ODBC connection.",
                                        "\n\nDo you wish to continue?"),
                                   numRows, table, dsn.name)))
        return()
  }

  # Start logging and executing the R code.

  startLog()
  appendLog(Rtxt("Load dataset from ODBC database table."), assign.cmd)
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <- table
  setMainTitle(crs$dataname)

  appendLog(Rtxt("Display a simple summary (structure) of the dataset."), str.cmd)

  setStatusBar(sprintf(Rtxt("The ODBC data has been loaded: %s."), crs$dataname))

  return(TRUE)
}

executeDataRdata <- function()
{

  # Collect relevant data.

  filename <- theWidget("data_filechooserbutton")$getFilename()
  dataset <- theWidget("data_name_combobox")$getActiveText()

  # Error exit if no filename is given.

  if (is.null(filename))
  {
    errorDialog(Rtxt("No Rdata filename has been chosen yet.",
                     "You must choose one before execution."))
    return(FALSE)
  }

  crs$dwd <- dirname(filename)
  crs$mtime <- urlModTime(filename)

  # Error if no dataset from the Rdata file has been chosen.

  if (is.null(dataset))
  {
    errorDialog(Rtxt("No R dataset name has been specified.",
                     "Please identify the name of the R dataset.",
                     "Any data frames that were found in the loaded Rdata",
                     "file are available to choose from in the Data Name",
                     "combo box."))
    return(FALSE)
  }

  # If there is a model warn about losing it.

  if (! overwriteModel()) return(FALSE)

  # Generate commands.

  assign.cmd <- sprintf('crs$dataset <- %s', dataset)
  str.cmd  <- "str(crs$dataset)"

  # Start logging and executing the R code.

  startLog()

  appendLog(Rtxt("Load an RData file."), assign.cmd)
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <- dataset
  setMainTitle(crs$dataname)

  setStatusBar(sprintf(Rtxt("The data has been loaded: %s.",
                            "Please wait whilst we extract its structure..."),
                       crs$dataname))

  return(TRUE)
}

executeDataRdataset <- function()
{

  # Collect relevant data from the GUI.

  .dataset <- theWidget("data_name_combobox")$getActiveText()

  # 20080907 Can we do this here each time? I haven't worked out a way
  # yet to update the combobox when it is clicked - this is what would
  # be best! But at least having it in here means we can update it
  # when it is executed.

  updateRDatasets(current=.dataset)

  if (is.null(.dataset))
  {
    errorDialog(Rtxt("No R dataset name has been specified.",
                     "Please identify the name of the R dataset.",
                     "Data frames that exist in the R Console",
                     "are available from the Data Name combo box."))
    return(FALSE)
  }

  # If there is a model then warn about losing it and ask if we are to
  # proceed.

  if (! overwriteModel()) return(FALSE)

  # Generate the commands that are to be run.

  assign.cmd <- sprintf('crs$dataset <- %s', .dataset)
  str.cmd <- "str(crs$dataset)"

  # Start logging and executing the R code.

  startLog()
  #theWidget(TV)$setWrapMode("none") # On for welcome msg
  #resetTextview(TV)

  appendLog(Rtxt("Load an R data frame."), assign.cmd)
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <- .dataset
  setMainTitle(crs$dataname)

  # 20170414 Record the new dataset's checksum so as to be able to
  # identify a change to an R dataset.  This could take a few seconds
  # so let's try it out and see if there is a significant new lag in
  # loading a dataset!

  setStatusBar(Rtxt("Calculate the R dataset's checksum..."))

  crs$dataname %>%
    get() %>%
    paste(collapse="") %>%
    paste(paste(names(get(crs$dataname)), collapse="")) %>%
    bitops::cksum() ->
  crs$cksum

  # 080328 Fix up any non-supported characters in the variable names,
  # otherwise they cause problems, e.g. "a-b" when used as ds$a-b is
  # interpreted as (ds$a - b)!

  names(crs$dataset) <- make.names(names(crs$dataset))

  appendLog(Rtxt("Display a simple summary (structure) of the dataset."), str.cmd)

  setStatusBar(Rtxt("The R dataset has been loaded.",
                    "Please wait whilst we extract its structure..."))

  return(TRUE)
}

executeDataLibrary <- function()
{
  # 080521 Load a dataset from a particular R package.

  # Collect relevant data.

  dataset <- theWidget("data_name_combobox")$getActiveText()

  if (is.null(dataset))
  {
    errorDialog(Rtxt("No dataset from the R libraries has been specified.",
                     "\n\nPlease identify the name of the dataset",
                     "you wish to load using the Data Name chooser."))
    return(FALSE)
  }

  # Actual dataset name as known when loaded.

  adsname <- gsub('([^ :]*).*$', '\\1', unlist(strsplit(dataset, ":"))[1])

  # Some datasets are loaded through loading another name (which
  # appears in parentheses. Extract the actual name of the dataset
  # that has to be named to be loaded.

  dsname <- gsub('.* \\((.*)\\)$', '\\1', unlist(strsplit(dataset, ":"))[1])

  # Extract the name of the package from which the dataset is loaded.

  dspkg <- unlist(strsplit(dataset, ":"))[2]

  # If there is a model then warn about losing it.

  if (! overwriteModel()) return()

  # Generate commands. 090321 Add a command to fix the variable
  # names. Some datasets, like AdultUCI in arules, have names like
  # education-num, which is some cases looks like a subtraction in
  # R. Without changing it here I would need to fix other code up to
  # quote the use of the variable name, and it might be that rpart has
  # an issue with it also (but not confirmed).

  assign.cmd <- sprintf(paste('data(list = "%s", package = "%s")\n',
                              'crs$dataset <- %s\n',
                              'names(crs$dataset) <- ',
                              'gsub("-", ".", names(crs$dataset))',
                              sep=""),
                        dsname, dspkg, adsname)

  # Start logging and executing the R code.

  startLog()

  appendLog(Rtxt("Load an R dataset."), assign.cmd)
  resetRattle()
  eval(parse(text=assign.cmd))
  if (! inherits(crs$dataset, "data.frame"))
  {
    errorDialog(sprintf(Rtxt("The selected dataset, '%s', from the '%s' package",
                             "is not of class data frame (the data type).",
                             "Its data class is '%s.'",
                             "This is not currently supported by %s",
                             "and so it  can not be loaded. Perhaps choose a different",
                             "dataset from the library."),
                        adsname, dspkg, class(crs$dataset), crv$appname))
    return(FALSE)
  }

  crs$dataname <- adsname
  crs$datapkg <- dspkg
  setMainTitle(crs$dataname)

  setStatusBar(Rtxt("The R package data is now available."))

  return(TRUE)
}

viewData <- function()
{
  startLog(Rtxt("View the dataset."))

  ## if (FALSE && packageIsAvailable("RGtk2Extras", Rtxt("view data in a spreadsheet")))
  ## {
  ##   # 20191016 RGtk2Extras was removed from the CRAN repositoryand
  ##   # archived on 2019-04-22 as check errors were not corrected
  ##   # depsite reminders.
  ##   #
  ##   # 20151115 We currently get the issue:
  ##   #
  ##   # Error in MakeDFEditWindow(.local, .local$theFrame, size.request, col.width) (from <text>#1) :
  ##   #  could not find function "gtkTreePathNewFromString"
  ##   #
  ##   # This is a NAMESPACE issue and a workaround is to
  ##   # require(RGkt2Extras). Eventually need to work out the correct
  ##   # solution.

  ##   lib.cmd <- sprintf("library(RGtk2Extras)")
  ##   appendLog(packageProvides("RGtk2Extras", "dfedit"), lib.cmd)
  ##   eval(parse(text=lib.cmd))

  ##   view.cmd <- paste('RGtk2Extras::dfedit(crs$dataset,\n',
  ##                     '                  ',
  ##                     'dataset.name=Rtxt("Rattle Dataset"),\n',
  ##                     '                  ',
  ##                     'size=c(800, 400))')
  ##   appendLog(Rtxt("Please note that any edits will be ignored."), view.cmd)
  ##   eval(parse(text=view.cmd))
  ## }
  ## else
    View(crs$dataset, "Data Viewer")
}

editData <- function()
{

  # Check if there is a model first and then warn about losing it.

  if (! overwriteModel()) return()

  # Start logging.

  startLog(Rtxt("Edit the dataset."))

  # Generate command to execute.

  assign.cmd <- if (is.null(crs$dataset))
                  'crs$dataset <- edit(data.frame())'
                ## else if (FALSE && packageIsAvailable("RGtk2Extras"))
                ##   # 20191016 RGtk2Extras was removed from the CRAN repositoryand
                ##   # archived on 2019-04-22 as check errors were not corrected
                ##   # depsite reminders.
                ##   paste('crs$dataset <- RGtk2Extras::dfedit(crs$dataset,\n',
                ##         '                                 ',
                ##         'dataset.name=Rtxt("Rattle Dataset"),\n',
                ##         '                                 ',
                ##         'size=c(800, 400))')
                else
                  'crs$dataset <- edit(crs$dataset)'

  # Update the log withe the command that is run.

  appendLog(Rtxt("Note that edits overwrite the current dataset."), assign.cmd)

  # These are needed because resetRattle clears everything

  ds <- crs$dataset

  resetRattle()
  crs$dataset <- ds
  eval(parse(text=assign.cmd))

  crs$dataname <- "dataset"
  # TODO fn <- theWidget("data_filechooserbutton")$getValue()

  setMainTitle(crs$dataname)

  # Update the select treeview and samples.

  createVariablesModel(colnames(crs$dataset))

  # Ensure we are viewing the treeview tab rather than the Welcome
  # message.

  crv$DATA.DISPLAY.NOTEBOOK$setCurrentPage(crv$DATA.DISPLAY.TREEVIEW.TAB)

  setStatusBar(Rtxt("The supplied data is now available."))

  set.cursor()

}

exportDataTab <- function()
{
}

########################################################################
# DATA ROLES
#
# The DATA Execute will perform a sampling of the data and stores
# the indicies in crs$train. It will also build the list of variable
# roles and stores these in crs$input, crs$ident, crs$ignore,
# crs$target, and crs$risk. This is then used in MODEL to limit the
# dataset in the call to rpart to just the crs$input variables.  In
# EVALUATE the crs$risk is used for the Risk Chart.

#------------------------------------------------------------------------
# Interface

on_data_sample_checkbutton_toggled <- function(button)
{
  if (button$getActive())
  {
    theWidget("sample_percentage_spinbutton")$setSensitive(TRUE)
    theWidget("sample_percentage_label")$setSensitive(TRUE)
    theWidget("sample_count_spinbutton")$setSensitive(TRUE)
    theWidget("sample_count_label")$setSensitive(TRUE)
    theWidget("sample_seed_spinbutton")$setSensitive(TRUE)
    theWidget("sample_seed_button")$setSensitive(TRUE)
    theWidget("data_sample_entry")$setSensitive(TRUE)
    # 090617 Do not show this label in the tool bar - It is mixing
    # information with actions and thus is conceptually not a good
    # thing to do. [Rado]
    # theWidget("explore_sample_label")$show()
  }
  else
  {
    theWidget("sample_percentage_spinbutton")$setSensitive(FALSE)
    theWidget("sample_percentage_label")$setSensitive(FALSE)
    theWidget("sample_count_spinbutton")$setSensitive(FALSE)
    theWidget("sample_count_label")$setSensitive(FALSE)
    theWidget("sample_seed_spinbutton")$setSensitive(FALSE)
    theWidget("sample_seed_button")$setSensitive(FALSE)
    theWidget("data_sample_entry")$setSensitive(FALSE)
    # theWidget("explore_sample_label")$hide()
  }
  crs$train <- crs$validate <- crs$test <- NULL
  setStatusBar()
}

on_sample_percentage_spinbutton_changed <- function(action, window)
{
  if (is.null(crs$dataset)) return()
  per <- theWidget("sample_percentage_spinbutton")$getValue()
  rows <- round(nrow(crs$dataset) * per / 100)
  crows <- theWidget("sample_count_spinbutton")$getValue()
  if (rows != crows)
    theWidget("sample_count_spinbutton")$setValue(rows)
  setStatusBar()
}

on_sample_count_spinbutton_changed <- function(action, window)
{
  if (is.null(crs$dataset)) return()
  rows <- theWidget("sample_count_spinbutton")$getValue()
  per <- round(100*rows/nrow(crs$dataset))
  cper <- theWidget("sample_percentage_spinbutton")$getValue()
  if (per != cper)
    theWidget("sample_percentage_spinbutton")$setValue(per)
  setStatusBar()
}

on_sample_seed_button_clicked <- function(button)
{
  rseed <- as.integer(runif(1, 0, 1000000))
  theWidget("sample_seed_spinbutton")$setValue(rseed)
}

item.toggled <- function(cell, path.str, model)
{
}

on_variables_toggle_ignore_button_clicked <- function(action, window)
{
  # Set the ignore flag for all selected variables, and ensure all
  # other roles are unchecked.

  #ptm <- proc.time()
  set.cursor("watch")
  tree.selection <- theWidget("select_treeview")$getSelection()

  # Under MS/Windows with Terminal Services to the host we get very
  # slow redraws? Tried fixing it with freezeUpdates and thawUpdates
  # but it had no impact. Changing 500 variables takes 5
  # seconds. When connected over terminal services the elapsed time
  # is 16 seconds, still with 5 seconds user time.

  # theWidget("rattle_window")$getWindow()$freezeUpdates()

  # 071113 Use the data parameter to avoid an RGtk2 bug in 2.12.1,
  # fixed in next release.
  tree.selection$selectedForeach(function(model, path, iter, data)
  {
    model$set(iter, crv$COLUMN[["ignore"]], TRUE)

    columns <- setdiff(crv$COLUMNstart:crv$COLUMNend,
                       crv$COLUMN[["ignore"]])

    # Timing indicates the for loop is slower on GNU/Linux but faster
    # on MS/Windows 500! But the extra test also slows things down,
    # so best not to conditionalise for now.

    #if (isWindows())
      for (c in columns)
        if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    #else
    #  lapply(columns, function(x) model$set(iter, x, FALSE))

    return(FALSE) # Keep going through all rows
  }, data=TRUE)

  #cat("->Ig", proc.time() - ptm, "\n")
  set.cursor()

  # theWidget("rattle_window")$getWindow()$thawUpdates()
}

on_variables_toggle_input_button_clicked <- function(action, window)
{
  # Set the input flag for all selected variables within the Select
  # tab, and ensure all other roles for these variables are unchecked.

  #ptm <- proc.time()
  set.cursor("watch")

  treeview <- theWidget("select_treeview")
  tree.selection <- treeview$getSelection()
  #theWidget("rattle_window")$getWindow()$freezeUpdates()

  # Use the data parameter to avoid an RGtk2 bug in 2.12.1, fixed in
  # next release. 071113
  tree.selection$selectedForeach(function(model, path, iter, data)
  {
    model$set(iter, crv$COLUMN[["input"]], TRUE)
    columns <- setdiff(crv$COLUMNstart:crv$COLUMNend,
                       crv$COLUMN[["input"]])

    #if (isWindows())
      for (c in columns)
        if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    #else
    #  lapply(columns, function(x) model$set(iter, x, FALSE))

    return(FALSE) # Keep going through all rows
  }, data=TRUE)

  #cat("->In", proc.time() - ptm, "\n")
  set.cursor()
  #theWidget("rattle_window")$getWindow()$thawUpdates()
}

#----------------------------------------------------------------------
# Execution

executeSelectTab <- function(resample=TRUE)
{
  # 080520 TODO May want to rename this as SELECT is no longer a tab
  # but is now part of the DATA tab. Perhaps we call it
  # resetSelections.

  # Check for pre-requisites.

  # Can not do any preparation if there is no dataset.

  if (noDatasetLoaded()) return()

  set.cursor("watch", Rtxt("Determining variable roles and characteristics..."))

  startLog(Rtxt("Action the user selections from the Data tab."))

  if (resample) executeSelectSample()

  input   <- getSelectedVariables("input")
  target  <- getSelectedVariables("target")
  risk    <- getSelectedVariables("risk")
  ident   <- getSelectedVariables("ident")
  ignore  <- getSelectedVariables("ignore")
  weight  <- getSelectedVariables("weight")
  weights <- theWidget("weight_entry")$getText()
  if (weights == "") weights <- NULL

  # Fail if there is more than one target.

  if (length(target) > 1)
  {
    errorDialog(sprintf(Rtxt("Multiple Targets have been identified (%s).",
                             "Only a single Target is allowed."),
                        paste(getSelectedVariables("target", FALSE), target,
                              sep=":", collapse=", ")))
    return()
  }

  # Ask if the Target does not look like a target.

  if (length(target))
    target.levels <- length(levels(as.factor(crs$dataset[[target]])))
  else
    target.levels <- 0

  # Fail if there is more than one risk.

  if (length(risk) > 1)
  {
    errorDialog(sprintf(Rtxt("More than a single %s",
                             "variable has been identified (%s).",
                             "Only a single variable is allowed.\n",
                             "\nPlease change the role of one of the variables."),
                        ifelse(survivalTarget(), "Status", "Risk"),
                        paste(getSelectedVariables("risk", FALSE), risk,
                              sep=":", collapse=", ")))
    return()
  }

  # Fail if the Risk column is not numeric.

  if (length(risk) && ! is.numeric(crs$dataset[[risk]]))
  {
    errorDialog(sprintf(Rtxt("The variable selected for your %s (%s)",
                             "is not numeric.",
                             "\n\nPlease select a numeric variable."),
                        ifelse(survivalTarget(), "Status", "Risk"), risk))
    return()
  }

  # Deal with weights.

  # 100829 Fail if there is more than one weight selected. Note that
  # once a weight is selected the Weight Calculator is not sensitive
  # and so any Weight formula there will be ignored.

  if (length(weight) > 1)
  {
    errorDialog(sprintf(Rtxt("Multiple Weights have been identified (%s).",
                             "Only a single Weight is allowed.\n",
                             "\nPlease reconfigure the roles."),
                        paste(getSelectedVariables("weight", FALSE), weight,
                              sep=":", collapse=", ")))
    return()
  }
  else if (length(weight) == 1)
  {
    weights <- sprintf("crs$dataset$%s", weight)
  }
  else if (theWidget("weight_entry")$isSensitive() &&
           not.null(weights) &&
           nchar(weights) > 0)
  {
    identifiers <- unlist(strsplit(weights, "[^a-zA-Z._]"))
    identifiers <- identifiers[nchar(identifiers) > 0]
    identifiers <- union(identifiers,identifiers) # Each var/id just once
    funs <- unlist(lapply(identifiers,
                          function(x)
                          {
                            try(eval(parse(text=sprintf("class(%s)", x))),
                                silent=TRUE) == "function"}))
    vars <- ! funs

    allvars <- union(input, union(target, union(risk, union(ident, ignore))))
    for (i in seq_len(sum(vars)))
    {
      # Check for any missing variables

      if (identifiers[vars][i] %notin% allvars)
      {
        errorDialog(sprintf(Rtxt("The Weight Calculator contains the variable %s",
                                 "which is not known in the dataset."),
                            identifiers[vars][i]))
        return()
      }

      # Check if Weight variables are not ignored, and inform user if not

      if (identifiers[vars][i] %notin%
                        union(ident, union(target, union(ignore, risk))))
      {
        infoDialog(sprintf(Rtxt("You have used the variable %s",
                                "in the weights formula but it is an input.",
                                "This is unusual since it is both an input variable",
                                "and used to weight the outputs.",
                                "It is suggested that you ignore this variable."),
                           identifiers[vars][i]))
      }

      # For each Weights variable, replace with full reference to
      # crs$dataset, since the variable is ignored.

      weights <- gsub(identifiers[vars][i],
                      sprintf("crs$dataset$%s", identifiers[vars][i]),
                      weights)

    }
  }

  #------------------------------------------------------------------------

  # Record appropriate information.

  crs$input     <- input
  crs$target    <- target
  crs$risk      <- risk
  crs$ident     <- ident
  crs$ignore    <- ignore
  crs$weights   <- weights
  crs$numeric   <- colnames(crs$dataset)[getNumericVariables(type="indicies")]
  crs$categoric <- getCategoricVariables(type="names")

  # 091206 Add the information to the Log tab

  convertOneMany <- function(x)
    switch(min(length(x)+1, 3), 'NULL', sprintf('"%s"', x),
           sprintf('c("%s")', paste(x, collapse='", "')))

  # Note the use of xxxxx to force proper line wrap of first line which will
  # not be indented at all.

  appendLog(Rtxt("The following variable selections have been noted."),
            'crs$input     <- ',   convertOneMany(input) %>%
                                   paste("xxxxxxxxxxxxxxxxxxx", .) %>%
                                   strwrap(crv$log_width, 0, 19, simplify=TRUE) %>%
                                   paste(collapse="\n") %>%
                                   sub('^x+ ', '', .),
            '\n\ncrs$numeric   <- ', convertOneMany(crs$numeric) %>%
                                   paste("xxxxxxxxxxxxxxxxxxx", .) %>%
                                   strwrap(crv$log_width, 0, 19, simplify=TRUE) %>%
                                   paste(collapse="\n") %>%
                                   sub('^x+ ', '', .),
            '\n\ncrs$categoric <- ', convertOneMany(crs$categoric) %>%
                                   paste("xxxxxxxxxxxxxxxxxxx", .) %>%
                                   strwrap(crv$log_width, 0, 19, simplify=TRUE) %>%
                                   paste(collapse="\n") %>%
                                   sub('^x+ ', '', .),
            '\n',
            '\ncrs$target    <- ', convertOneMany(target),
            '\ncrs$risk      <- ', convertOneMany(risk),
            '\ncrs$ident     <- ', convertOneMany(ident),
            '\ncrs$ignore    <- ', convertOneMany(ignore),
            '\ncrs$weights   <- ', convertOneMany(weights))

  # 090801 Update the transforms list, so that any transforms that are
  # not ignore/ident will be noted as active. The status is used when
  # exporting to XML since we want to keep ignored transforms (since
  # they might be used in other transforms) but don't want them
  # exported unnecessarily.

  for (i in seq_along(crs$transforms))
    if (names(crs$transforms)[i] %in% union(ident, ignore))
      crs$transforms[[i]]$status <- "inactive"
    else
      crs$transforms[[i]]$status <- "active"

  # Update MODEL targets

  the.target <- ifelse(length(target), sprintf(Rtxt("Target: %s"), target),
                       Rtxt("No Target"))
  the.risk <- ifelse(length(risk), sprintf(Rtxt("Status: %s"), risk),
                     Rtxt("No Risk"))

  theWidget("explot_target_label")$setText(the.target)

  theWidget("test_groupby_target_label")$setText(the.target)

  theWidget("rpart_target_label")$setText(the.target)
  theWidget("rf_target_label")$setText(the.target)
  theWidget("svm_target_label")$setText(the.target)
  # theWidget("gbm_target_label")$setText(the.target)
  theWidget("ada_target_label")$setText(the.target)
  theWidget("glm_target_label")$setText(the.target)
  theWidget("nnet_target_label")$setText(the.target)

  theWidget("model_survival_radiobutton")$setSensitive(TRUE)
  theWidget("model_survival_time_var_label")$setText(sub(Rtxt("Target:"),
                                                         Rtxt("Time:"), the.target))
  theWidget("model_survival_status_var_label")$setText(the.risk)

  # Update MODEL weights

  if (not.null(weights))
  {
    weights.display <- gsub('crs\\$dataset\\$', '', weights)
    the.weight <- sprintf(Rtxt("Weights: %s"), weights.display)
    # 080815 Just display Weights if there is a weights value, and
    # empty otherwise.
    # theWidget("model_tree_rpart_weights_label")$setText(the.weight)
    theWidget("model_tree_rpart_weights_label")$setText(Rtxt("Weights in use."))
  }
  else
  {
    theWidget("model_tree_rpart_weights_label")$
    setText("")
  }

  # 080413 Update MODEL types that are available.

  # With more than two classes we can't use AdaBoost since the current
  # package does not support more than 2 classes.

  if (categoricTarget() && target.levels <= 2)
    theWidget("boost_radiobutton")$setSensitive(TRUE)
  else
    theWidget("boost_radiobutton")$setSensitive(FALSE)

  # Remove/restore tab functionality for XDF/CSV.

  theWidget("test_tab_widget")$setSensitive(!not.null(crs$xdf))
  theWidget("test_tab_label")$setSensitive(!not.null(crs$xdf))
  theWidget("transform_tab_widget")$setSensitive(!not.null(crs$xdf))
  theWidget("transform_tab_label")$setSensitive(!not.null(crs$xdf))
  theWidget("associate_tab_widget")$setSensitive(!not.null(crs$xdf))
  theWidget("associate_tab_label")$setSensitive(!not.null(crs$xdf))

  # Disable various options for XDF file load.

  if(not.null(crs$xdf))
  {
    # Explore

    theWidget("explore_distr_radiobutton")$hide()
    theWidget("prcomp_radiobutton")$hide()
    theWidget("explore_interactive_radiobutton")$hide()

    # Cluster

    theWidget("ewkm_radiobutton")$hide()
    theWidget("hclust_radiobutton")$hide()
    theWidget("biclust_radiobutton")$hide()
  }
  else
  {
    # Explore

    theWidget("explore_distr_radiobutton")$show()
    theWidget("prcomp_radiobutton")$show()
    theWidget("explore_interactive_radiobutton")$show()

    # Cluster

    theWidget("ewkm_radiobutton")$show()
    theWidget("hclust_radiobutton")$show()
    theWidget("biclust_radiobutton")$show()
  }

  # Update various MODEL options

  if (survivalTarget())
  {
    theWidget("model_survival_radiobutton")$setSensitive(TRUE)
    theWidget("model_survival_radiobutton")$setActive(TRUE)
    theWidget("rpart_radiobutton")$setSensitive(FALSE)
    theWidget("boost_radiobutton")$setSensitive(FALSE)
    theWidget("rf_radiobutton")$setSensitive(FALSE)
    theWidget("svm_radiobutton")$setSensitive(FALSE)
    theWidget("model_linear_radiobutton")$setSensitive(FALSE)
    theWidget("nnet_radiobutton")$setSensitive(FALSE)
    theWidget("all_models_radiobutton")$setSensitive(FALSE)
  }
  else if (categoricTarget())
  {
    theWidget("model_survival_radiobutton")$setSensitive(FALSE)

    theWidget("rpart_radiobutton")$setSensitive(TRUE)
    theWidget("rf_radiobutton")$setSensitive(TRUE)
    if(not.null(crs$xdf))
      theWidget("svm_radiobutton")$setSensitive(FALSE)
    else
    theWidget("svm_radiobutton")$setSensitive(TRUE)

    theWidget("model_linear_radiobutton")$setSensitive(TRUE)

    theWidget("all_models_radiobutton")$setSensitive(TRUE)

    # For linear models, if it is categoric and binomial then assume
    # logistic regression (default to binmoial distribution and the
    # logit link function) otherwise it is multinomial so assume
    # poisson regression (default to poisson distribution and log link
    # function).

    theWidget("model_linear_poisson_radiobutton")$setSensitive(FALSE)

    if (binomialTarget())
    {
      theWidget("model_linear_builder_label")$setText("glm (Logistic)")
      theWidget("glm_linear_radiobutton")$setSensitive(FALSE)
      theWidget("glm_gaussian_radiobutton")$setSensitive(FALSE)
      theWidget("glm_logistic_radiobutton")$setSensitive(TRUE)
      theWidget("glm_logistic_radiobutton")$setActive(TRUE)
      theWidget("model_linear_probit_radiobutton")$setSensitive(TRUE)
      theWidget("glm_multinomial_radiobutton")$setSensitive(FALSE)

      theWidget("nnet_radiobutton")$setSensitive(TRUE)
      theWidget("nnet_hidden_nodes_label")$setSensitive(TRUE)
      theWidget("nnet_hidden_nodes_spinbutton")$setSensitive(TRUE)
      theWidget("nnet_builder_label")$setText("nnet (0/1)")

    }
    else
    {
      theWidget("model_linear_builder_label")$setText("multinom")
      theWidget("glm_linear_radiobutton")$setSensitive(FALSE)
      theWidget("glm_gaussian_radiobutton")$setSensitive(FALSE)
      theWidget("glm_logistic_radiobutton")$setSensitive(FALSE)
      theWidget("model_linear_probit_radiobutton")$setSensitive(FALSE)
      theWidget("glm_multinomial_radiobutton")$setSensitive(TRUE)
      theWidget("glm_multinomial_radiobutton")$setActive(TRUE)

      theWidget("nnet_radiobutton")$setSensitive(FALSE)
      # I don't think these need tgo be done. We can't see the options
      # when the nnet button is not sensitive
      #theWidget("nnet_hidden_nodes_label")$setSensitive(FALSE)
      #theWidget("nnet_hidden_nodes_spinbutton")$setSensitive(FALSE)
      #theWidget("nnet_builder_label")$setText("")
    }
  }
  else if (numericTarget())
  {
    theWidget("model_survival_radiobutton")$setSensitive(FALSE)

    theWidget("rpart_radiobutton")$setSensitive(TRUE)
    theWidget("rf_radiobutton")$setSensitive(TRUE) # 090301 Support regression
    theWidget("svm_radiobutton")$setSensitive(FALSE)

    # For linear models, if it is numeric we are probably going to use
    # a lm so set the default family to nothing! This is becasue lm
    # simply does gaussian and an identity link function.

#    theWidget("glm_family_comboboxentry")$setActive(0)

    theWidget("model_linear_radiobutton")$setSensitive(TRUE)
    theWidget("model_linear_builder_label")$setText("lm")
    theWidget("glm_linear_radiobutton")$setSensitive(TRUE)
    theWidget("glm_linear_radiobutton")$setActive(TRUE)
    theWidget("glm_gaussian_radiobutton")$setSensitive(TRUE)
    theWidget("glm_logistic_radiobutton")$setSensitive(FALSE)
    theWidget("model_linear_probit_radiobutton")$setSensitive(FALSE)

    if (countTarget())
      theWidget("model_linear_poisson_radiobutton")$setSensitive(TRUE)
    else
      theWidget("model_linear_poisson_radiobutton")$setSensitive(FALSE)


    theWidget("glm_multinomial_radiobutton")$setSensitive(FALSE)

    theWidget("nnet_radiobutton")$setSensitive(TRUE)
    theWidget("nnet_hidden_nodes_label")$setSensitive(TRUE)
    theWidget("nnet_hidden_nodes_spinbutton")$setSensitive(TRUE)
    theWidget("nnet_builder_label")$setText("nnet (Regression)")

    theWidget("all_models_radiobutton")$setSensitive(TRUE)

  }
  else # What else could it be? No target!
  {
    theWidget("rpart_radiobutton")$setSensitive(FALSE)
    theWidget("rf_radiobutton")$setSensitive(FALSE)
    theWidget("svm_radiobutton")$setSensitive(FALSE)
    theWidget("model_linear_radiobutton")$setSensitive(FALSE)
    theWidget("nnet_radiobutton")$setSensitive(FALSE)
    theWidget("all_models_radiobutton")$setSensitive(FALSE)
    theWidget("nnet_hidden_nodes_label")$setSensitive(FALSE)
    theWidget("nnet_hidden_nodes_spinbutton")$setSensitive(FALSE)
    # 080719 - remove, or else we can't sample and cluster!!
    # theWidget("data_sample_checkbutton")$setActive(FALSE)
    theWidget("glm_linear_radiobutton")$setSensitive(FALSE)
    theWidget("glm_gaussian_radiobutton")$setSensitive(FALSE)
    theWidget("model_linear_poisson_radiobutton")$setSensitive(FALSE)
    theWidget("glm_logistic_radiobutton")$setSensitive(FALSE)
    theWidget("model_linear_probit_radiobutton")$setSensitive(FALSE)
    theWidget("glm_multinomial_radiobutton")$setSensitive(FALSE)
    theWidget("model_survival_radiobutton")$setSensitive(FALSE)
  }

  # Update EVALUATE risk variable

  theWidget("evaluate_risk_label")$setText(crs$risk)

  # Update defaults that rely on the number of variables.

  crv$rf.mtry.default <- floor(sqrt(length(crs$input)))
  theWidget("rf_mtry_spinbutton")$setValue(crv$rf.mtry.default)

  # 080505 We auto decide whether the target looks like a categoric
  # or numeric, but if it ends up being a categoric (the user
  # overrides with the type radio button) with very many classes,
  # then complain!

  if (not.null(target)
      && categoricTarget()
      && target.levels > crv$max.categories)
  {
    if (! questionDialog(sprintf(Rtxt("The column selected as a Target (%s)",
                                      "will be treated as a categoric variable",
                                      "since Target Type is set to Categoric.",
                                      "\n\nThe variable has %d distinct values",
                                      "whch is greater than the threshold of %d.",
                                      "That is unusual and some algorithms will",
                                      "take a long time.\n\nYou may like to",
                                      "consider using fewer classes for the",
                                      "target categoric variable or select",
                                      "Target Type as Numeric.",
                                      "\n\nDo you want to continue anyhow?"),
                                 target, target.levels, crv$max.categories)))
      return()
  }

  # 091206 Check that we have both a target and risk for a survival
  # model.

  if (not.null(target)
      && !length(risk)
      && survivalTarget())
  {
    errorDialog(Rtxt("You have chosen Survial models as the target type,",
                     "but no Status variable has been identified.",
                     "Survival models require both a Time and a Status",
                     "variable.\n",
                     "\nPlease identify the Status variable and then",
                     "Execute this tab once again."))
    return(FALSE)
  }

  # Finished - update the status bar.

  roles.msg <- sprintf(Rtxt("Roles noted. %s%s observations and %s input variables."),
                       ifelse(not.null(crs$xdf), paste("Subset", format(nrow(crs$dataset), big.mark=","), "of "), ""),
                       format(ifelse(is.null(crs$xdf), nrow(crs$dataset), nrow(crs$xdf)), big.mark=","),
                       format(length(crs$input), big.mark=","))
  if (length(crs$target) == 0)
    model.msg <-  Rtxt("No target thus no predictive",
                       "modelling nor sampling.")

  else if (survivalTarget())
    model.msg <- sprintf(Rtxt("The target is %s with %s. Survival models enabled."),
                         crs$target, crs$risk)
  else if (categoricTarget())
    model.msg <- sprintf(Rtxt("The target is %s. Categoric %d.",
                              "Classification models enabled."),
                         crs$target, target.levels)
  else
    model.msg <- sprintf(Rtxt("The target is %s. Numeric.",
                              "Regression models enabled."),
                         crs$target)

  setStatusBar(roles.msg, model.msg)
}

executeSelectSample <- function()
{
  # Identify if there are observations without a target value. TODO
  # 080426. I started looking at noting those observations with missing
  # target values. This is recorded in crs$nontargets. Currently I'm
  # not using it. The intention was to only sample from those with
  # targets, etc. But the impacts need to be carefuly thought through.
  #
  # Perhaps the philosophy should go back to the fact that the user
  # can split the dataset up themselves quite easily, and I do
  # provide a mechanism for them to load their dataset for scoring.

  #target <- getSelectedVariables("target")
  #print(target)
  #crs$nontargets <- which(is.na(crs$dataset[[target]]))

  # 160902 Record the number of rows in the full dataset, differently
  # calculated for XDF dataset.

  nr <- ifelse(is.null(crs$xdf), nrow(crs$dataset), nrow(crs$xdf))
  ds <- ifelse(is.null(crs$xdf), "dataset", "xdf")

  # Record that a random sample of the dataset is desired and the
  # random sample itself is loaded into crs$train. 080425 Whilst we
  # are at it we also set the variable crs$targeted to be those row
  # indicies that have a non NA target.

  if (theWidget("data_sample_checkbutton")$getActive())
  {
    if (newSampling())
    {
      ssizes <- parseSampleEntry()/100
      ssize <- round(nr * ssizes[1])
      vsize <- round(nr * ssizes[2])
      if (ssizes[3] == 0)
        tsize <- 0
      else
        tsize <- nr - ssize - vsize
    }
    else
      #ssize <- theWidget("sample_percentage_spinbutton")$getValue()
      #ssize <- floor(nrow(crs$dataset)*ssize/100)
      ssize <- theWidget("sample_count_spinbutton")$getValue()

    seed <- theWidget("sample_seed_spinbutton")$getValue()
    if (seed == crv$seed) seed <- "crv$seed"

    if (newSampling())
    {
      sample.cmd <- sprintf("set.seed(%s)\n", seed)

      sample.intro <- paste0('# nobs=', nr, ' train=', ssize)

      sample.cmd <- sprintf(paste0("%s\n",
                                   "crs$nobs <- nrow(crs$%s)\n\n",
                                   "crs$train <- ",
                                   "sample(crs$nobs, %s*crs$nobs)\n"),
                            sample.cmd, ds, round(ssizes[1], 2))

      sample.intro %<>% paste0(' validate=', vsize)

      if (vsize > 0)
      {
        if (tsize > 0)
        {
          sample.cmd <- paste(sample.cmd,
                              "crs$nobs %>%",
                              "  seq_len() %>%",
                              "  setdiff(crs$train) %>%",
                              paste0("  sample(", round(ssizes[2], 2), "*crs$nobs) ->"),
                              "crs$validate\n",
                              sep="\n")
        }
        else
        {
          sample.cmd <- paste(sample.cmd,
                              "crs$nobs %>%",
                              "  seq_len() %>%",
                              "  setdiff(crs$train) ->",
                              "crs$validate",
                              sep="\n")
        }
      }
      else
      {
        sample.cmd <- sprintf("%scrs$validate <- NULL\n", sample.cmd)
      }

      sample.intro %<>% paste0(' test=', nr-ssize-vsize)

      if (tsize > 0)
      {
        sample.cmd <- paste(sample.cmd,
                            "crs$nobs %>%",
                            "  seq_len() %>%",
                            "  setdiff(crs$train) %>%",
                            "  setdiff(crs$validate) ->",
                            "crs$test",
                            sep="\n")
      }
      else
      {
        sample.cmd <- sprintf("%s\n\ncrs$test <- NULL\n", sample.cmd)
      }

      sample.cmd <- paste0(sample.intro, "\n\n", sample.cmd)
    }
    else
    {
      # 100417 Even for RStat make sure we maintain crs$train as it is
      # now starting to be used.

      sample.cmd <- paste(sprintf("set.seed(%s)\n\n", seed),
                          "crs$train <- sample(crs$nobs, ",
                          ssize,
                          ")", sep="")
    }

    appendLog(Rtxt("Build the train/validate/test datasets."), sample.cmd)
    eval(parse(text=sample.cmd))

    # 20160902 Partition the XDF into the train/validate/test
    # datasets.

    if (! is.null(crs$xdf))
    {
      # Identify a variable so that rxSplit() can split the dataset.

      part.cmd <- sprintf(paste0(
        "crs$xdf %%<>%%\n",
        "  mutate(.train=factor(sample(1:3,\n",
        "                              size    = .rxNumRows,\n",
        "                              replace = TRUE,\n",
        "                              prob    = c(%0.2f, %0.2f, %0.2f)),\n",
        "                       levels=1:3))"),
        ssizes[1], ssizes[2], ssizes[3])
      appendLog(Rtxt("Add train/validate/test flag to each observation."),
                part.cmd)
      eval(parse(text=part.cmd))

      # Split into appropriate datasets.

      split.cmd <- 'crs$xdf.split <- rxSplit(crs$xdf, splitByFactor=".train")'
      appendLog(Rtxt("Split dataset into train/validate/test."), split.cmd)
      eval(parse(text=split.cmd))

    }
  }
  else
  {
    crs$train <- crs$validate <- crs$test <- NULL

    theWidget("evaluate_validation_radiobutton")$setSensitive(FALSE)
    theWidget("evaluate_testing_radiobutton")$setSensitive(FALSE)
    if (exists("RATTLE.SCORE.IN") && not.null(RATTLE.SCORE.IN))
      theWidget("evaluate_csv_radiobutton")$setActive(TRUE)
    else
      theWidget("evaluate_training_radiobutton")$setActive(TRUE)
  }

  crs$smodel <- vector()

  # TODO For test/train, use sample,split from caTools?

  ## Set some defaults that depend on sample size.

  #if (is.null(crs$train))
  #  crv$rf.sampsize.default <- length(crs$dataset)
  #else
  #  crv$rf.sampsize.default <- length(crs$train)
  #theWidget("rf_sampsize_spinbutton")$setValue(crv$rf.sampsize.default)

  ## 080520 Don't set the status bar - it is overwritten by the
  ## message about variable roles being noted.

##  setStatusBar()

##  if (theWidget("data_sample_checkbutton")$getActive())
##    setStatusBar("The sample has been generated.",
##                  "There are", length(crs$train), "observations.")
##  else
##    setStatusBar("Sampling is inactive.")
}

getSelectedVariables <- function(role, named=TRUE)
{
  # DESCRIPTION
  # Generate a list of variables marked with the specified role.
  #
  # ARGUMENTS
  # role  = a string naming the role to query on
  # named = if TRUE return variable names as strings, if FALSE, numbers
  #
  # DETAILS The select_treeview, categorical_treeview and
  # continuous_treeview are places where a variable can be identified
  # as having a given role. Whilst the role of "ignore" is common
  # across all three treeviews, only the ignore from the main
  # select_treeview is considered. If a role is not found, simply
  # return NULL, rather than an error (for no particular reason).
  #
  # ASSUMPTIONS The variable and number columns are assumed to be the
  # same in each of crv$COLUMNS, crv$CATEGORICAL, and crv$CONTINUOUS.

  variables <- NULL
  type <- "logical"

  if (role %in% c("input", "target", "risk", "ident", "ignore", "weight"))
  {
    model <- theWidget("select_treeview")$getModel()
    rcol  <- crv$COLUMN[[role]]
  }

  else if (role %in% c("boxplot", "hisplot", "cumplot", "benplot"))
  {
    model <- theWidget("continuous_treeview")$getModel()
    rcol  <- crv$CONTINUOUS[[role]]
  }

  else if (role %in% c("barplot", "dotplot", "mosplot"))
  {
    model <- theWidget("categorical_treeview")$getModel()
    rcol  <- crv$CATEGORICAL[[role]]
  }

  else if (role %in% c("paiplot"))
  {
    model <- theWidget("continuous_treeview")$getModel()
    rcol  <- crv$CONTINUOUS[[role]]
    model2 <- theWidget("categorical_treeview")$getModel()
    rcol2  <- crv$CATEGORICAL[[role]]
  }

  else
    return(NULL)

  vcol <- crv$COLUMN[["variable"]]
  ncol <- crv$COLUMN[["number"]]
  model$foreach(function(model, path, iter, data)
                {
                  flag <- model$get(iter, rcol)[[1]]
                  if (named)
                    variable <- model$get(iter, vcol)[[1]]
                  else
                    variable <- model$get(iter, ncol)[[1]]
#                  if (type=="character")
#                  {
#                    if (role == "zero" && flag == "Zero/Missing")
#                      variables <<- c(variables, variable)
#                    if (role == "mean" && flag == "Mean")
#                      variables <<- c(variables, variable)
#                    if (role == "median" && flag == "Median")
#                      variables <<- c(variables, variable)
#                  }
#                  else
                    if (flag) variables <<- c(variables, variable)
                  return(FALSE) # Keep going through all rows
                }, TRUE)

  if (role %in% c("paiplot")) # we need to collect the categorical variables too
  {
    model2$foreach(function(model2, path, iter, data)
    {
      flag <- model2$get(iter, rcol2)[[1]]
      if (named)
        variable <- model2$get(iter, vcol)[[1]]
      else
        variable <- model2$get(iter, ncol)[[1]]
        if (flag) variables <<- c(variables, variable)
      return(FALSE) # Keep going through all rows
    }, TRUE)
  }

  # Set the data parameter to TRUE to avoid an RGtk2 bug in 2.12.1, fixed in
  # next release. 071117

  # 2009-11-30 Apparently Gtk always returns UTF-8 strings (Acken
  # Sakakibara). Thus we convert to the locale of the system.
  #
  # 2022-08-30 localeToCharset() can return a list so always just get
  # the first element. Note that x[1] is NA even if x is NA.

  if (! is.na(localeToCharset()[1]))
    variables <- iconv(variables, "UTF-8", localeToCharset()[1])

  return(variables)
}

initialiseVariableViews <- function()
{
}

createVariablesModel <- function(variables, input=NULL, target=NULL,
                                 risk=NULL, ident=NULL, ignore=NULL, weight=NULL,
                                 zero=NULL, mean=NULL,
                                 boxplot=NULL,
                                 hisplot=NULL, cumplot=NULL, benplot=NULL,
                                 barplot=NULL, dotplot=NULL, mosplot=NULL,
                                 paiplot=NULL,
                                 autoroles=TRUE)
{
}

#----------------------------------------------------------------------
#
# Support
#

getIncludedVariables <- function(numonly=FALSE, listall=FALSE, risk=FALSE, target=TRUE)
{
  # 20110102 TODO Stop using this function, or else have this function
  # always return the string "c(crs$input, crs$target)" etc, as
  # appropriate, so we use symbolic names rather than lists of
  # variable numbers.

  # DESCRIPTION
  # Generate a numeric list of variables not ignored.
  #
  # ARGUMENTS
  # numonly = Only include numeric variables
  # listall = Don't simplify a full list to NULL
  # risk =  Include any risk variable in the returned list
  #
  # RETURNS
  # A string of comma separated numbers
  #
  # DETAILS Generates a list of input variable indicies and the
  # target variable index and, optionally, the risk variable index.
  # If the list contains all variables, then return NULL (as the
  # dataset does not then need to be indexed to subset the variables).
  #
  # TODO This last assumption of returning NULL causes problems since we
  # don't know whether this means all variables or no variables!

  fi <- getVariableIndicies(crs$input)
  if (target)
    ti <- getVariableIndicies(crs$target)
  else
    ti <- NULL
  if (risk)
    ri <- getVariableIndicies(crs$risk)
  else
    ri <- NULL

  if (numonly)
    fl <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  else
    fl <- 1:ncol(crs$dataset)

  if (! listall && setequal(union(fi,union(ti, ri)), fl))
    return(NULL)
  else
    return(simplifyNumberList(intersect(fl, union(fi, union(ti, ri)))))
}

inputVariables <- function(numonly=FALSE)
{
  # Return, as a comma separated list (as a string), the list of input
  # variable indicies. If the list contains all variables except for
  # the target variable, then return NULL (as the dataset does not then
  # need to be indexed to subset the variables).

  fi <- getVariableIndicies(crs$input)
  ti <- getVariableIndicies(crs$target)

  if (is.null(crs$input))
  {
    errorDialog(Rtxt("No input variables have been selected.",
                     "This doesn't make a lot of sense.",
                     "Please choose some input variables before proceeding."))
    stop(Rtxt("no input variables specified"))
  }

  if (numonly)
    fl <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  else
    fl <- 1:ncol(crs$dataset)

  if (setequal(fi, fl))
    return(NULL)
  else
    return(simplifyNumberList(intersect(fl,fi)))
}

used.variables <- function(numonly=FALSE)
{
  # Return, as a comma separated list (as a string) the list of all
  # variable indicies for those that are not ignored. If the list
  # contains all variables except for the ignored variables, then
  # return NULL.

  ii <- union(getVariableIndicies(crs$ignore), getVariableIndicies(crs$ident))

  if (numonly)
    fl <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  else
    fl <- 1:ncol(crs$dataset)

  if (setequal(fl, ii))
    return(NULL)
  else
    return(simplifyNumberList(setdiff(fl, ii)))
}

getCategoricVariables <- function(type="string", include.target=FALSE)
{
  # Return a list of categoric variables from amongst those with an
  # INPUT role. If type is "names" than return the list of variable
  # names. If there is a target variable and include.target is TRUE
  # and the target variable is categoric, then include that in the
  # returned value.

  # 20180923 Don't try to include the target if there is not one!

  include.target <- ifelse(length(crs$target), include.target, FALSE)

  include <- NULL
  cats <- seq(1, ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.factor))]
  if (length(cats) > 0)
  {

    indicies <- getVariableIndicies(crs$input)

    # 160919 I tried the concept of adding the target even if it is
    # numeric, but then pairs plot fails. The aim was to allow
    # Benfords to group by the target by default, even if target is
    # numeric. For pairs plot and box plot and histogram I now add a
    # mutate prior to the ggplot to convert the target to a factor
    # then all is good! So now it is okay to include a numeric target
    # in the list of categoric variables if target is requested. If
    # this has wider implications then add the target specifically to
    # the Group By combo box rather than changing the semantics here.

    included <- intersect(cats, indicies)

    if (include.target)
    {
      target.levels <- length(levels(as.factor(crs$dataset[[crs$target]])))
      if (target.levels <= crv$max.categories)
        included <- c(included, getVariableIndicies(crs$target))
    }

    if (type=="names")
      include <- names(crs$dataset)[included]
    else
      include <- simplifyNumberList(included)
  }
  return(include)
}

getNumericVariables <- function(type="string")
{
  # Returns a list of numeric variables. 080803 Add support to return
  # a list of indicies rather than the default string that needs to be
  # executed to identfy the indicies.

  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- intersect(nums, getVariableIndicies(crs$input))
    if (type == "string")
      indicies <- simplifyNumberList(indicies)
  }
  else
    indicies <- NULL

 return(indicies)
}
