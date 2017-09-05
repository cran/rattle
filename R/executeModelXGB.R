
#----------------------------------------------------------------------
#
# MODEL XGB
#

executeModelXGB <- function(dataset, formula)
{
  # Initial setup. 
  
  TV <- "ada_textview"
  VAR <- "crs$ada"
 
   # Build model
  
  crs$ada <- buildModelXgb(formula,
                    dataset,
                    tv=theWidget("ada_textview"),
                    max_depth=theWidget("ada_maxdepth_spinbutton")$getValue(),
                    eta=theWidget("ada_learningrate_spinbutton")$getValue(),
                    #num_parallel_tree=theWidget("ada_ntree_spinbutton")$getValue(),
                    nthread=theWidget("ada_nthread_spinbutton")$getValue(),
                    nround=theWidget("ada_niter_spinbutton")$getValue(),
                    #metrics=theWidget("ada_metrics_combobox")$getActiveText(),
                    objective=theWidget("ada_objective_combobox")$getActiveText()
                    )
				
  return(TRUE)
}