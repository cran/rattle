executeHistPlot2 <- function(dataset, vars, target, targets, stratify, sampling, pmax)
{
  startLog(Rtxt("Histogram Plots"))

  libs <- loadLibs(c("ggplot2", "ggplot", "dplyr", "select"))

  for (s in seq_along(vars))
  {
    newPlot()

    title.txt <- genPlotTitleCmd(generateTitleText(vars[s],
                                                   target,
                                                   sampling,
                                                   stratify && length(targets)),
                                 vector=TRUE)

    plot.cmd <- paste(sprintf('ar <- range(with(crs, select(%s, %s)))',
                              dataset, vars[s]),
                      sprintf('bw <- (ar[2]-ar[1])/nclass.FD(with(crs, %s$%s))',
                              dataset, vars[s]),
                      sprintf('p  <- ggplot(with(crs, select(%s, %s, %s)), aes(x=%s)) +',
                              dataset, vars[s],
                              ifelse(length(target), target, "NULL"), vars[s]),
                      paste('      geom_histogram(aes(y=..density..),',
                            'binwidth=bw, fill="grey", colour="black") +'),
                      sprintf('      geom_density(%s) +',
                              ifelse(length(target),
                                     sprintf("aes(colour=%s)", target), "")),
                      sprintf('      xlab("%s\\n\\n%s") +', vars[s], title.txt[2]),
                      sprintf('      ggtitle("%s") +',  title.txt[1]),
                      sprintf('      labs(colour="", y="Density")'),
                      paste('print(p)', sep="\n"), sep="\n")
             
    appendLog(paste(Rtxt("Histogram Plot for"), vars[s]), plot.cmd)
    eval(parse(text=plot.cmd))
  }
  unloadLibs(libs)
}
