#' Build a random forest based from xdf dataset.
#' 
#' Time-stamp: <2017-08-18 12:13:50 Graham Williams>
#'
executeModelRxDForest <- function()
{
  # Identify the model specific constants.
  
  TV   <- "rf_textview"
  NAME <- commonName(crv$RXDFOREST)
  PKG  <- "RevoScaleR"
  FUNC <- "rxDForest"
  VAR  <- "crs$rf"
  TYPE <- Rtxt("Classification")
  DESC <- Rtxt("build an xdf based random forest model")
  
  # Check package prerequisites.
  
  if (! packageIsAvailable(PKG, DESC)) return(FALSE)

  # Construct the formula for the model build.

  crs$target %>%
    paste("~", paste(crs$input, collapse=" + ")) %>%
    strwrap(crv$log_width, 0, 4) %>%
    paste(collapse="\n") ->
  frml

  # Variables to be included --- a string of indicies.

  # included <- getIncludedVariables()
  included <- "c(crs$input, crs$target)" # 20110102

  # Some convenience booleans

  sampling   <- not.null(crs$train)
  including  <- not.null(included)
  subsetting <- sampling || including

  # Commands.

  build.cmd <- paste0(VAR, " <- ", FUNC, "(\n\n  ", frml, ",\n\n",
                      "  data       = crs$xdf.split[[1]],\n",
                      "  importance = TRUE",
                      ")")

  print.cmd <- paste0("print(", VAR, ")")

  startLog(NAME)

  # Build the model.

  appendLog(sprintf(Rtxt("Build the %s model."), NAME), build.cmd)
  start.time <- Sys.time()
  result <- try(eval(parse(text=build.cmd)), silent=TRUE)
  time.taken <- Sys.time() - start.time

  # Show the results.

  resetTextview(TV)
  setTextview(TV,
              sprintf(Rtxt("Summary of the %s model for %s (built using '%s'):"),
                      NAME, TYPE, FUNC),
              "\n\n",
              collectOutput(print.cmd))

  # Now that we have a model, make sure the buttons are sensitive.

  showModelRFExists(traditional=TRUE, conditional=FALSE)

  # Finish up.

  reportTimeTaken(TV, time.taken, NAME)

  return(TRUE)
}
