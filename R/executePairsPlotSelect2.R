#' Perform the required operations for displaying a pairs plot.
#' 
#' Time-stamp: <2015-09-17 21:08:50 gjw>
#' 
executePairsPlotSelect2 <- function(dataset, vars, target, targets, stratify, sampling, pmax)
{
  startLog(Rtxt("Display a pairs plot for the selected variables."))

  varsi <- getVariableIndicies(vars)
  
 # v1 <- theWidget("pairs_color_combobox")$getActiveText()
  v1 <- target
  if (is.null(v1) || v1 == " ")
  {
    colorStr<-'' # No color selected.
  }
  else
  {
    colorStr<-sprintf('colour="%s",',v1)
  }

  plot.cmd <- paste0('GGally::ggpairs(', dataset, ',\n',
                     '        columns=c(',
                     paste(varsi, collapse=','), '),\n', 
                     if (colorStr!="") paste0('        ', colorStr, "\n"),
                     '        diag=list(continuous="density",\n',
                     '                  discrete="bar"),\n',
                     '        upper=list(continuous="cor",\n',
                     '                   combo="box",\n',
                     '                   discrete="ratio"),\n',
                     '        lower=list(continuous="points",\n',
                     '                   combo="denstrip",\n',
                     '                   discrete="facetbar"))')
      
  appendLibLog(Rtxt("Use GGally's ggpairs() to do the hard work."), plot.cmd)
  newPlot()
  eval(parse(text=sprintf("suppressMessages(print(%s))", plot.cmd)))
}
