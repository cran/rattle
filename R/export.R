# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <Sunday 2026-02-08 14:46:51 +1100 Graham Williams>
#
# Implement functionality associated with the Export button and Menu.
#
# Copyright (c) 2009-2020 Togaware Pty Ltd
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

on_export_button_clicked <- function(action, window)
{
  # Wrap the actual call with a "try" so that the watch cursor turns
  # off even on error.

  setStatusBar()
  set.cursor("watch")
  on.exit(set.cursor())
  dispatchExportButton()

}

dispatchExportButton <- function()
{
  # Check which tab of notebook and dispatch to appropriate execute action

  ct <- getCurrentPageLabel(crv$NOTEBOOK)

  if (ct == crv$NOTEBOOK.CLUSTER.NAME)
  {
    exportClusterTab()
  }
  else if (ct == crv$NOTEBOOK.MODEL.NAME)
  {
    exportModelTab()
  }
  else if (ct == crv$NOTEBOOK.ASSOCIATE.NAME)
  {
    exportAssociateTab()
  }
  else if (ct == crv$NOTEBOOK.DATA.NAME ||
           ct == crv$NOTEBOOK.TRANSFORM.NAME)
  {
    # For any of the DATA, SELECT, or TRANSFORM tabs, the logical
    # thing to EXPORT is the dataset.

    exportDataTab()
  }
##   else if (ct == crv$NOTEBOOK.EVALUATE.NAME)
##   {
##     exportEvaluateTab()
##   }
  else  if (ct == crv$NOTEBOOK.LOG.NAME)
  {
    exportLogTab()
  }
  else if (ct == crv$NOTEBOOK.EVALUATE.NAME &&
           theWidget("evaluate_score_radiobutton")$getActive())
    # 091123 Users expect the Export to save the scores, so make this
    # the same as clicking the Execute button.
    executeEvaluateTab()
  else
    {
      # 100424 This is required for MS/Windows and Japanese and
      # sprintf for some reason - presumably a bug.
      if (isJapanese()) Encoding(ct) <- "unknown"
      infoDialog(sprintf(Rtxt("No export functionality is available for the",
                              "%s tab. Nothing done."), ct))
    }
}

## This is handled by the Cairo device save button now. Might want to
## interpret Export differently for these now.

## exportExploreTab <- function()
## {
##   if (theWidget("explore_distr_radiobutton")$getActive())
##     exportPlot("dist")
##   else if (theWidget("explore_correlation_radiobutton")$getActive())
##     exportPlot("corr")
##   else if (theWidget("explore_correlation_hier_radiobutton")$getActive())
##     exportPlot("hiercorr")
##   else if (theWidget("prcomp_radiobutton")$getActive())
##     exportPlot("prcomp")
##   else
##     infoDialog("No export functionality is available for the",
##                "selected option.")
## }

## ########################################################################

## exportEvaluateTab <- function()
## {
##   if (theWidget("risk_radiobutton")$getActive())
##     exportPlot("risk")
##   else if (theWidget("lift_radiobutton")$getActive())
##     exportPlot("lift")
##   else if (theWidget("roc_radiobutton")$getActive())
##     exportPlot("roc")
##   else if (theWidget("precision_radiobutton")$getActive())
##     exportPlot("precision")
##   else if (theWidget("sensitivity_radiobutton")$getActive())
##     exportPlot("sensitivity")
##   else
##     infoDialog("No export functionality from the Evaluate tab for",
##                "the selected option is yet available.")
## }

## exportPlot <- function(type="plot", devices=NULL)
## {
##   if (is.null(dev.list()))
##   {
##     warnDialog("There are currently no active graphics devices.",
##                "So there is nothing to export!",
##                "Please Execute (F2) to obtain a plot to export.")
##     return()
##   }

##   # Obtain a filename to save to. Ideally, this would also prompt for
##   # the device to export, and the fontsize, etc.

##   {
##     save.name <- dialog$getFilename()
##     dialog$destroy()
##   }
##   else
##   {
##     dialog$destroy()
##     return()
##   }

##   if (get.extension(save.name) == "") save.name <- sprintf("%s.pdf", save.name)

##   if (file.exists(save.name))
##     if ( ! questionDialog("A Graphics file of the name", save.name,
##                                 "already exists. Do you want to overwrite",
##                                 "this file?"))
##       return()

##   cur <- dev.cur()
##   ext <- get.extension(save.name)
##   if (ext == "pdf")
##     dev.copy(pdf, file=save.name, width=7, height=7)
##   else if (ext == "png")
##     dev.copy(png, file=save.name, width=700, height=700)
##   else if (ext == "jpg")
##     dev.copy(jpeg, file=save.name, width=700, height=700)
##   dev.off()
##   dev.set(cur)

##   infoDialog(sprintf("R Graphics: Device %d (ACTIVE)", cur),
##              "has been exported to", save.name)
## }

getWidgetOrObject <- function(dialog, name)
{
  if (crv$useGtkBuilder)
    return(dialog$getObject(name))
  else
    return(dialog$getWidget(name))
}

getExportSaveName <- function(mtype)
{
}

generateExportPMMLtoC <- function(model.name, save.name, TV)
{
  # 110116 Introduce an encoding to the file saved. Then under
  # MS/Windows the file is saved as UTF-8 rather than SHIFT-JIS. On
  # Linux it is saved as UTF-8 anyhow. However, I note that we seem to
  # be working pretty hard to make everything UTF-8 on MS/Windows. It
  # seems that we are fighting against "nature" here - is there
  # something about encodings and R that we are missing. Maybe that is
  # the key as to why everything works okay on Linux (UTF-8) but we
  # battle with MS/Windows.
  #
  # From cran.r-project.org/doc/manuals/R-data.html: "The hard part is
  # to know what file encoding to use. For use on Windows, it is best
  # to use what Windows calls `Unicode' that is "UTF-16LE". (Even
  # then, Windows applications may expect a Byte Order Mark which the
  # implementation of iconv used by R may or may not add depending on
  # the platform.), Using UTF-8 is a good way to make portable files
  # that will not easily be confused with any other encoding, but even
  # Mac OS X applications (where UTF-8 is the system encoding) may not
  # recognize them, and Windows applications are most unlikely
  # to. Apparently Excel:mac 2004/8 expects .csv files in "macroman"
  # encoding (the encoding used in much earlier versions of Mac OS)."

  #110122 IBI suggest the C code be SJIS for now.

  export.cmd <- paste('con <- file("%s", open="w")', # 110122, encoding="UTF8")',
                      "\ncat(pmmltoc(%s%s%s,",
                      '\n            name="%s",',
                      '\n            includePMML=%s,',
                      '\n            includeMetaData=%s,',
                      '\n            exportClass=%s),',
                      '\n    file=con)',
                      '\nclose(con)', sep="")

  export.cmd <- sprintf(export.cmd,
                        fixWindowsSlash(save.name),
                        ifelse(isWindows() && isJapanese(),
                               paste("paste('<?xml version=\"1.0\"",
                                     "encoding=\"shift_jis\"?>\\n',",
                                     "\n                  "), ""),
                        "toString(eval(parse(text=pmml.cmd)))",
                        ifelse(isWindows() && isJapanese(), ")", ""),
                        model.name,
                        ifelse(attr(save.name, "includePMML"), "TRUE", "FALSE"),
                        ifelse(attr(save.name, "includeMetaData"),
                               sprintf('rattle:::getTextviewContent("%s")', TV),
                               '"\\"Not Included\\""'),
                        ifelse(attr(save.name, "exportClass"), "TRUE", "FALSE"))
  return(export.cmd)
}
