#----------------------------------------------------------------------
#
# MODEL ADA
#

executeModelAda <- function(dataset, formula)
{
  # Initial setup. 
  
  TV  <- "ada_textview"
  VAR <- "crs$ada"

  # Build model.
  
  crs$ada <- buildModelAda(
    formula,
    dataset,
    tv       = theWidget("ada_textview"),
    maxdepth = theWidget("ada_maxdepth_spinbutton")$getValue(),
    minsplit = theWidget("ada_minsplit_spinbutton")$getValue(),
    cp       = theWidget("ada_cp_spinbutton")$getValue(),
    xval     = theWidget("ada_xval_spinbutton")$getValue(),
    ntree    = theWidget("ada_ntree_spinbutton")$getValue())
  
  return(TRUE)
}
