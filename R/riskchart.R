riskchart <- function(pr,
                      ac,
                      ri               = NULL,
                      title            = "Risk Chart",
                      title.size       = 10,
                      subtitle         = NULL,
                      caption          = TRUE,
                      show.legend      = TRUE,
                      optimal          = NULL,
                      optimal.label    = "",
                      chosen	       = NULL,
                      chosen.label     = "",
                      include.baseline = TRUE,
                      dev              = "",
                      filename         = "",
                      show.knots       = NULL,
                      show.lift        = TRUE,
                      show.precision   = TRUE,
                      show.maximal     = TRUE,
                      risk.name        = "Risk",
                      recall.name      = "Recall",
                      precision.name   = "Precision",
                      thresholds       = NULL,
                      legend.horiz     = TRUE)
{

  # Riskcharts are only implemented for binary classificaton.
  
  if (length(unique(na.omit(ac))) > 2)
    stop("Risk charts are for binary classification models only.")
  
  # 121209 Initialise variables otherwise appearing to be unintialised
  # to R CMD check.

  x <- y <- NULL

  # ggplot2 version of the risk chart.

  if (! requireNamespace("ggplot2"))
    stop(Rtxt("riskchart requires the ggplot2 package"))

  data <- evaluateRisk(pr, ac, ri)
  
  stopifnot(c("Caseload", "Recall", "Precision") %in% colnames(data)) 

  # Is Risk included in the supplied data?
  
  include_risk <- "Risk" %in% colnames(data)
  
  score <- as.numeric(row.names(data))
  locateScores <- function(s) 
  { 
    # Convert score to caseload (percentile) for plotting
    idx <- findInterval(s, score)
    idx[idx==0] <- 1
    data$Caseload[idx]
  } 
  scores <- data.frame(ticks=seq(0.1, 0.9, by = 0.1))
  scores$pos <- locateScores(scores$ticks)

  # Calculate AUC for each curve. The calculateAUC is for the whole
  # plot, rather than just under the maximal possible curve. So we
  # need to refine it to be relative to the maximal curve, as
  # suggested by Hari Koesmarno, 9 Aug 2013. I suspect I should always
  # show the AUC wrt the maximal obtainable rather than wrt to the
  # whole plot but for now I'll show the old if show.maximal is
  # selected, as a method of backward compatibility.
  
  recall_auc <- with(data, calculateAUC(Caseload, Recall))
  recall_auc <- round(100*ifelse(show.maximal,
                                 (2*recall_auc)/(2-data$Precision[1]),
                                 recall_auc))
  recall.name <- sprintf("%s (%s%%)", recall.name, recall_auc)

  if (include_risk)
  {
    # Updated by Cameron Chisholm 20170908 to include the optimal risk
    # chart (pink) and corrected AUC for risk.
    #
    # NOTE: Have only tested on thousands of rows - not sure of
    # performance scaling to calculate the actual AUC.
    #
    # 1) Generate the optimal revenue scenario
    #
    # 2) Calculate the relative AUC between the actual and optimal
    #    revenue curves
    #
    # We already have all the data we need as part of the riskchart
    # function so thankfully it's just a matter of playing with the
    # data we already have. The riskchart already provides controls
    # around when to calculate risk data so we can also hijack these
    # controls so we don't try and calculate risk when it isn't
    # wanted.
    #
    # To generate the optimal revenue curve we take the risk data,
    # sort it in descending order by risk, calculate cumulative risk
    # captured, the overall percentage of risk captured, and the
    # percentage of caseload. Once we have this we can draw the pink
    # optimal risk curve.
    #
    # To calculate the AUC of the optimal risk curve we have to sort
    # our risk data in descending order by percentage of
    # caseload. Once we have this we can pass the percentage of
    # caseload and percentage of risk into the Rattle function
    # calculateAUC.
    #
    # Once we have the AUC of the optimal curve we just divide the
    # actual risk AUC (this is already computed in the risk chart
    # function) by the optimal risk AUC to get the relative AUC.
    #
    # Once we have all the data we need it's simply a matter of adding
    # another layer to the ggplot code to plot the optimal risk curve
    # and editing the legend to use the new relative AUC.
    
    risk_auc <- with(data, calculateAUC(Caseload, Risk))

    # Sort the data and make it a 1 column data frame.

    sorted_risk <- ri %>% sort(decreasing=TRUE) %>% as.data.frame()
    names(sorted_risk) <- c("risk")
    
    # Avoid global variable binding NOTE for now.

    c.risk <- percent.risk <- risk <- row.num <- percent.caseload <- NULL

    # Row-wise risk calculations.

    sorted_risk %<>%
      dplyr::mutate(c.risk = cumsum(risk)) %>% 
      dplyr::mutate(percent.risk = c.risk/sum(risk)) %>% 
      # Get the row number so we can calculate the percentage of
      # caseload for each row.
      dplyr::mutate(row.num = as.integer(row.names.data.frame(.))) %>% 
      dplyr::mutate(percent.caseload = row.num/nrow(.))
    
    # Need to reverse order to calculate AUC.

    sorted_risk <- dplyr::arrange(sorted_risk, desc(percent.caseload))

    # Calculate the AUC for the optimal risk case.

    sorted_risk_auc <- calculateAUC(sorted_risk$percent.caseload,
                                    sorted_risk$percent.risk)

    # Add a row for the no data case.

    sorted_risk <- rbind(data.frame(risk             = 0,
                                    c.risk           = 0,
                                    percent.risk     = 0,
                                    row.num          = 0,
                                    percent.caseload = 0),
                         sorted_risk)

    risk.name <- sprintf("%s (%s%%)", risk.name, round(risk_auc * 100))
    
    # Risk auc = area under actual risk/area under optimal risk case.

    risk_auc <- risk_auc/sorted_risk_auc
    
  }
  
  # How to get recall.name etc in the following - by default it is not
  # defined?

  p <- ggplot2::ggplot(data, ggplot2::aes(x=100*Caseload))
  p <- p + ggplot2::scale_colour_manual(breaks=c("Recall", "Risk", "Precision"),
                                        labels=c(recall.name,
                                                 risk.name,
                                                 precision.name),
                                        values=c("blue", "forestgreen", "red"))
  p <- p + ggplot2::scale_linetype_manual(breaks=c("Recall", "Risk", "Precision"),
                                          labels=c(recall.name,
                                                   risk.name,
                                                   precision.name),
                                          values=c(3, 1, 2))

  if (show.maximal)
  {
    # This is the maximal performance possible from any model - a
    # straight line 100% performance at a caseload including only
    # productive cases.
    
    mx <- data.frame(x=c(0, 100*data$Precision[1], 100), y=c(0, 100, 100))
    p <- p + ggplot2::geom_line(data=mx, ggplot2::aes(x, y), colour="grey")

    # This is the maximal risk curve.

    if(include_risk)
    {
      p <- p + ggplot2::geom_line(data=sorted_risk,
                                  ggplot2::aes(x=percent.caseload*100,
                                               y=percent.risk*100),
                                  colour="pink")
    }

  }
  
  p <- p + ggplot2::geom_line(ggplot2::aes(y=100*Recall,
                                           colour="Recall",
                                           linetype="Recall"))
  p <- p + ggplot2::geom_line(ggplot2::aes(y=100*Precision,
                                           colour="Precision",
                                           linetype="Precision"))
  p <- p + ggplot2::geom_text(data=scores, ggplot2::aes(x=100*pos,
                                                        y=102,
                                                        label=ticks), size=2)
  p <- p + ggplot2::annotate("text",
                             x=0,
                             y=105,
                             label="Risk Scores",
                             hjust=0,
                             size=2.5)
  
  p <- p + ggplot2::geom_line(ggplot2::aes(y=100*Caseload))

  # Labels: Handle labels including any supplied title, subtitle and
  # caption.
  
  if (! is.character(caption) & ! is.logical(caption) & ! is.null(caption))
    stop("Caption needs to be a string or TRUE/FALSE")

  if (is.logical(caption))
      caption <- if (caption) genPlotTitleCmd(vector=TRUE) else NULL

  p <- p + ggplot2::labs(title=title,
                         subtitle=subtitle,
                         caption=caption,
                         x="Caseload (%)",
                         y="Performance (%)")

  # Legend:
  
  if (legend.horiz)
    p <- p + ggplot2::theme(legend.title=ggplot2::element_blank(),
                            plot.title=ggplot2::element_text(size=title.size),
                            legend.justification=c(0, 0),
                            legend.position=c(.20, 0.02),
                            legend.direction="horizontal")
  else
    p <- p + ggplot2::theme(legend.title=ggplot2::element_blank(),
                            plot.title=ggplot2::element_text(size=title.size),
                            legend.justification=c(0, 0),
                            legend.position=c(.30, .02))

  # Ensure we have a single legend by giving both the same name.
  
  p <- p + ggplot2::guides(colour=ggplot2::guide_legend(keywidth=3,
                                                        labels=1:3,
                                                        title="Legend"),
                           linetype=ggplot2::guide_legend(keywidth=3,
                                                          labels=1:3,
                                                          title="Legend"))

  # Axis Ticks:
  
  p <- p + ggplot2::scale_x_continuous(breaks=seq(0, 100, 20))
  p <- p + ggplot2::scale_y_continuous(breaks=seq(0, 100, 20))
  
  if (include_risk)
    p <- p + ggplot2::geom_line(ggplot2::aes(y        = 100*Risk,
                                             colour   = "Risk",
                                             linetype = "Risk"))

  if (include.baseline && show.precision)
    p <- p + ggplot2::geom_text(data=data.frame(x=100, y=100*data$Precision[1]+4,
                           text=sprintf("%0.0f%%", 100*data$Precision[1])),
                         ggplot2::aes(x, y, label=text), size=3)

  if (show.lift)
  {
    base_lift <- 100*data$Precision[1]
    lift_seq <- seq(base_lift, 100, base_lift)
    lifts <- data.frame(x=110, y=lift_seq, label=lift_seq/base_lift)
    # 140906 R-Devel CMD check complains label not defined in:
    # p <- p + ggplot2::geom_text(data=lifts,
    #                             ggplot2::aes(x, y, label=label), size=3)
    # So use aes_string
    p <- p + ggplot2::geom_text(data=lifts,
                                ggplot2::aes_string("x", "y", label="label"), size=3)
    p <- p + ggplot2::annotate("text", x=110, y=101.5, label="Lift", size=3)
  }

  if (! is.null(thresholds))
    p <- p + ggplot2::geom_vline(xintercept=100*locateScores(thresholds),
                        linetype="twodash", color="grey")

  p <- p + ggplot2::theme(legend.background=ggplot2::element_rect(fill=NA),
                 legend.key=ggplot2::element_rect(fill=NA, linetype=0))

  attr(p, "recall") <- recall_auc

  return(p)
}
