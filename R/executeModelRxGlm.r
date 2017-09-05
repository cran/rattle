#' Build a Linear model.
#' 
#' Time-stamp: <2017-08-18 12:15:00 Graham Williams>
#'
executeModelRxGlm <- function()
{
  # Initial setup. 
  
  TV <- "glm_textview"
  mtype <- "linear"
  VAR <- "crs$glm"
  # Obtain the family

  if (theWidget("glm_linear_radiobutton")$getActive())
    family <- "Linear"
  else if (theWidget("glm_gaussian_radiobutton")$getActive())
    family <- "Gaussian"
  else if (theWidget("model_linear_poisson_radiobutton")$getActive())
    family <- "Poisson"
  else if (theWidget("glm_logistic_radiobutton")$getActive())
    family <- "Logistic"
  else if (theWidget("model_linear_probit_radiobutton")$getActive())
    family <- "Probit"
  else if (theWidget("glm_multinomial_radiobutton")$getActive())
    family <- "Multinomial"
  
  # Build the formula for the model. 080719 If the user has requested
  # a numeric target and the target is actually a factor, then convert
  # to a numeric, else the algorithms complain.

  if (family %in% c("Linear", "Gaussian", "Poisson")
      && "factor" %in% class(crs$xdf.split[[1]][[crs$target]]))
  {
    for(i in 1:length(crs$xdf.split))
      eval(parse(text = sprintf("crs$xdf.split[[i]] <- rxDataStep(crs$xdf.split[[i]], transforms = list(%s = as.numeric(crs$target)-1),overwrite = TRUE)",crs$target)))
  }

  # Construct the formula for the model build.

  crs$target %>%
    paste("~", paste(crs$input, collapse=" + ")) %>%
    strwrap(crv$log_width, 0, 4) %>%
    paste(collapse="\n") ->
  frml
   
  # List, as a string, the variables to be included. 
  
  included <- "c(crs$input, crs$target)" # 20110102 getIncludedVariables()
  
  # Some convenience booleans.

  sampling  <- not.null(crs$train)
  including <- not.null(included)
  subsetting <- sampling || including
  
  startLog(Rtxt("Regression model"))

  if (family == "Logistic" || family == "Probit")
  {
    # For a categoric variable we usually default to assuming
    # proprtions data, and so we perform logistic regression, which
    # uses a binomial distribution and a logit link function. However,
    # the user could eventually choose a different distriubtion/link
    # pair.
    #
    # If we have a binary response it may be that we might consider
    # using a loglog link rather than a logit link.

    #### Need to confirm from Graham for the weights 
	
    model.cmd <- sprintf(paste0(VAR," <- ", "rxGlm(\n\n  ", frml, ",\n\n",
                                "  data   = crs$xdf.split[[1]],\n",
                                "  family = binomial(link=%s)",
                                ")"),
                         ifelse(family == "Probit", "probit", "logit"))	   

    # In addition to the default summary, add the chi-square test of
    # the difference between the null model and the current model as
    # presented in http://www.ats.ucla.edu/stat/R/dae/probit.htm.

    # To check with Graham about Log likelihood function 
    
    summary.cmd <- "print(summary(crs$glm))"
  }
  else if (family == "Linear")
  {

    # For a numeric target we expect to produce the usual linear
    # model. We could use glm to generate the model using the gaussian
    # distribution and the identity link function. This will produce
    # the same model as lm. But lm is faster (glm is an iterative
    # algorithm) and it also produces the R squared stats, so we use
    # lm.
	
    model.cmd <- paste0(VAR," <- ",sprintf("rxLinMod(formula = frml, data = %s)", 	"crs$xdf.split[[1]]"))	   

    summary.cmd <- "print(summary(crs$glm))"
  }
  else if (family == "Gaussian")
  {
    # Whilst this is a less efficient equivalent of the Linear model
    # using lm, it is identified that some users perceive value in
    # having both lm and glm options for numeric regression. This uses
    # a gaussian distribution and an identity link function.

    model.cmd <- paste0(VAR," <- ", sprintf("rxGlm(formula = frml, data = %s, family=gaussian(identity))", 	"crs$xdf.split[[1]]"))	  

    summary.cmd <- "print(summary(crs$glm))"
  }
  else if (family == "Poisson")
  {
    # 080912 Added

    model.cmd <- paste0(VAR," <- ",sprintf("rxGlm(formula = frml, data = %s, family=gaussian(log))", 	"crs$xdf.split[[1]]"))	  


    summary.cmd <- "print(summary(crs$glm))"
  }
  else if (family == "Multinomial")
  {
    lib.cmd <-  "library(nnet, quietly=TRUE)"
    if (! packageIsAvailable("nnet", Rtxt("build a mulitnomial model"))) return(FALSE)
    appendLog(Rtxt("Build a multinomial model using the nnet package."), lib.cmd)
    eval(parse(text=lib.cmd))

    car.available <- TRUE
    lib.cmd <- "library(car, quietly=TRUE)"
    if (! packageIsAvailable("car", Rtxt("use Anova to evaluate a mulitnomial model")))
      car.available <- FALSE
    else
    {
      appendLog(Rtxt("Summarise multinomial model using Anova from the car package."), lib.cmd)
      eval(parse(text=lib.cmd))
    }
    
    model.cmd <- paste("crs$glm <- ",
                       "multinom",
                       "(", frml, ", data=crs$dataset",
                       if (subsetting) "[",
                       if (sampling) "crs$train",
                       if (subsetting) ",",
                       if (including) included,
                       if (subsetting) "]",
                       ", trace=FALSE, maxit=1000",
                       ")", sep="")

    summary.cmd <- paste("rattle.print.summary.multinom(summary(crs$glm,",
                         "                              Wald.ratios=TRUE))",
                         paste('cat(sprintf("Log likelihood: %.3f (%d df)\n",',
                               'logLik(crs$glm)[1], attr(logLik(crs$glm), "df")))'),
                         paste('if (is.null(crs$glm$na.action)) omitted <- TRUE',
                               'else omitted <- -crs$glm$na.action'),
                         paste('cat(sprintf("Pseudo R-Square: %.8f\n\n",',
                               'cor(apply(crs$glm$fitted.values, 1, ',
                               'function(x) which(x == max(x))),\n',
                               'as.integer(crs$dataset',
                               ifelse(sampling, '[crs$train,]', ''),
                               '[omitted,]$',
                               crs$target, '))))\n', sep=""),
                         "cat('==== ANOVA ====\n')",
                         "print(Anova(crs$glm))",
                         'print("\n")',
                         sep="\n")

  }
  
  # Build the model.

  appendLog(Rtxt("Build a Regression model."),
            model.cmd, sep="")
  start.time <- Sys.time()
  result <- try(eval(parse(text=model.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    if (any(grep("too many (.*) weights", result)))
    {
      find.num.weights <- regexpr('\\([0-9]*\\)', result)
      num.weights <- substr(result, find.num.weights,
                            find.num.weights + attr(find.num.weights, "match.length")-1)
      errorDialog(sprintf(Rtxt("The Multinomial model build has failed,",
                               "with too many weights (%d)",
                               "needing to be calculated.",
                               "Perhaps consider reducing the",
                               "number of categoric variables with",
                               "unique values (if you have",
                               "such variables in your input data)",
                               "or perhaps treating the target",
                               "variable as numeric and perform a",
                               "numeric linear regression."),
                          num.weights))
      setTextview(TV)
    }        
    else if (any(grep("contrasts can be applied only to factors with 2", result)))
    {
      factors <- crs$input[sapply(crs$input, function(x)
                                  is.factor(crs$dataset[[x]]))]
      single <- factors[sapply(factors, function(x)
                               length(levels(crs$dataset[[x]]))==1)]
      one <- length(single)==1
      errorDialog("It appears that", ifelse(one, "a", "some"),
                  "categoric input",
                  ifelse(one, "variable is", "variables are"), "constant.",
                  "The regression model algorithm can not handle such",
                  ifelse(one, "a variable.", "variables."),
                  "You may like to Ignore the",
                  ifelse(one, "variable", "variables"),
                  "through the Data tab:\n\n",
                  paste(single, collapse=", "))
      setTextview(TV)
    }
    else
      errorDialog(Rtxt("The regression model appears to have failed.",
                       "The error message was:"),
                  result, crv$support.msg)
    return(FALSE)
  }
  
  # Summarise the model.

  appendLog(sprintf(Rtxt("Generate a textual view of the %s model."),
                    commonName(crv$GLM)), summary.cmd)
  
  resetTextview(TV)
  setTextview(TV, sprintf(Rtxt("Summary of the %s %s model",
                               "(built using %s):\n"),
                          family, "Regression",
                          ifelse(family == "Linear", "lm",
                                 ifelse(family == "Multinomial", "multinom", "glm"))),
              ifelse(any(is.na(coef(crs$glm))),
                     paste("\n***Note*** Singularities were found in the modeling
and are indicated by an NA in the following table.
This is often the case when variables are linear
combinations of other variables, or the variable
has a constant value.  These variables will be ignored
when using the model to score new data and will not be
included as parameters in the exported scoring routine.\n",
                           sep="\n"), ""),
              collectOutput(summary.cmd))

# 090223 Got it working above - just wrap a print around it?
#
#  if (family == "Multinomial" && car.available)
#  {
#    # Couldn't get this working within the summary.cmd
#    appendTextview(TV, paste("\n\n",
#                             collectOutput("Anova(crs$glm)", use.print=TRUE)),
#                   tvsep=FALSE)
#  }
  
  
  if (sampling) crs$smodel <- union(crs$smodel, crv$GLM)

  # Enable the plot button

  if (family == "Multinomial")
    theWidget("model_linear_plot_button")$setSensitive(FALSE)
  else
    theWidget("model_linear_plot_button")$setSensitive(TRUE)
  
  # Finish up.
  
  time.taken <- Sys.time()-start.time

  reportTimeTaken(TV, time.taken,
                  msg=paste(sprintf(Rtxt("The %s model has been built."),
                    commonName(mtype)),
                    ifelse(any(is.na(coef(crs$glm))),
                           Rtxt("Singularities exist."), "")))
  return(TRUE)
}
