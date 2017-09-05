#' Display boxplots using ggplot2.
#' 
#' Time-stamp: <2016-09-19 19:28:44 Graham Williams>
#' 
executeBoxPlot2 <- function(dataset, vars, target, targets, stratify, sampling, pmax)
{
  # Check prerequisite packages.
  
  if (!packageIsAvailable("ggplot2", Rtxt("build plots using a grammar of graphics"))) return(FALSE)
  if (!packageIsAvailable("gridExtra", Rtxt("arrange plots on a grid"))) return(FALSE)
  if (!packageIsAvailable("dplyr", Rtxt("mutate the supplied dataset"))) return(FALSE)

  # Report to the Log script.
  
  startLog(Rtxt("Display box plots for the selected variables."))

  # Start a new plot as we could be drawing multiple types of plots.
  
  newPlot()

  for (i in seq_along(vars))
  {
    title.txt <- genPlotTitleCmd(generateTitleText(vars[i],
                                                   target,
                                                   sampling,
                                                   stratify && length(targets)),
                                 vector=TRUE)

    plot.cmd <- stringr::str_c('# Generate a box plot.\n\n',
                               sprintf("p%02d", i), ' <- crs %>%\n',
                               '  with(', dataset, ') %>%\n',
                               if (length(target))
                                 stringr::str_c('  dplyr::mutate(', target,
                                                '=as.factor(', target, ')) %>%\n'),
                               '  ggplot2::ggplot(ggplot2::aes(y=', vars[i], ')) +\n',
                               '  ggplot2::geom_boxplot(ggplot2::aes(x="All"), ',
                               'notch=TRUE, fill="grey") +\n',
                               '  ggplot2::stat_summary(ggplot2::aes(x="All"), ',
                               'fun.y=mean, geom="point", shape=8) +\n',
                               if (length(target))
                                 stringr::str_c('  ggplot2::geom_boxplot(',
                                                'ggplot2::aes(x=', target, ', ',
                                                'fill=', target, '), notch=TRUE) +\n',
                                                '  ggplot2::stat_summary(',
                                                'ggplot2::aes(x=', target, '), ',
                                                'fun.y=mean, geom="point", ',
                                                'shape=8) +\n'),
                               '  ggplot2::xlab("',
                               if (length(target))
                                 stringr::str_c(target, '\\n\\n'),
                               title.txt[2], '") +\n',
                               '  ggplot2::ggtitle("', title.txt[1], '") +\n',
                               '  ggplot2::theme(legend.position="none")')
  
    comment <- paste(Rtxt("Use ggplot2 to generate box plot for"), vars[i])
    appendLibLog(comment, plot.cmd, include.libs=(i==1))
    eval(parse(text=plot.cmd))
  }

  display.cmd <-
    "gridExtra::grid.arrange(" %s+%
    paste(sprintf("p%02d", seq_len(i)), collapse=", ") %s+%
    ")"

  appendLibLog("Display the plots.", display.cmd)
  eval(parse(text=display.cmd))

}

