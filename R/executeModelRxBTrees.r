#' Build a Linear model.
#' 
#' Time-stamp: <2017-08-18 12:13:21 Graham Williams>
#'
executeModelRxBTrees <- function()
{
  # Initial setup. 
  
  TV <- "ada_textview"
  VAR <- "crs$ada"    
  NAME <- "Boosted Trees"
  FUNC <- "rxBTrees"
  
  # Formula Creation for the model.
  
  crs$target %>%
    paste("~", paste(crs$input, collapse=" + ")) %>%
    strwrap(crv$log_width, 0, 4) %>%
    paste(collapse="\n") ->
  frml

  # Build the model build command.

  # TODO Need to allow parameters to be set from the GUI.

  build.cmd <- paste0(VAR, " <- ", FUNC, "(\n\n  ", frml, ",\n\n",
                      "  data     = crs$xdf.split[[1]],\n",
                      "  maxDepth = 30,\n",
                      "  cp       = 0.01,\n",
                      "  minSplit = 20",
                      ")")

  # Build the model.

  appendLog(Rtxt("Build a rxBTrees model."),
            build.cmd, sep="")
  start.time <- Sys.time()
  result <- try(eval(parse(text=build.cmd)), silent=TRUE)
  summary.cmd <- "print(summary(crs$ada))"
  
  print.cmd <- paste0("print(", VAR, ")")
  
  # Text view
  resetTextview(TV)
  setTextview(TV,
              sprintf(Rtxt("Boosted Trees built using %s"),
                      FUNC),
              "\n\n",
              collectOutput(print.cmd))  
  return(TRUE)
}
