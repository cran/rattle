\name{savePlotToFile}
\alias{savePlotToFile}
\alias{copyPlotToClipboard}
\alias{printPlot}
\title{
  Save a plot in some way
}
\description{
  
  For the current device, or for the device identified, save the plot
  displayed there in some way. This is either saved to file, copied to
  the clipboard for pasting into other applications, or sent to the
  printer for saving a hard copy.
  
}
\usage{
savePlotToFile(file.name, dev.num=dev.cur())
copyPlotToClipboard(dev.num=dev.cur())
printPlot(dev.num=dev.cur()) 
}
\arguments{
  
  \item{file.name}{Character string naming the file including the file
    name extension which is used to specify the type of file to save.}

  \item{dev.num}{A device number indicating which device to save.}

}
\references{Package home page: \url{https://togaware.com/projects/rattle/}}
\author{\email{Graham.Williams@togaware.com}}
\keyword{hplot}
