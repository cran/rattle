loadTooltips <- function()
{
  if (! packageIsAvailable("XML", "load GUI tooltips"))
  {
    warning("The XML package is not available. Tooltips will not be available.")
    return(FALSE)
  }

  require(XML, quietly=TRUE)

  result <- try(etc <- file.path(path.package(package="rattle")[1], "etc"),
                silent=TRUE)
  if (inherits(result, "try-error"))
    doc <- xmlTreeParse("tooltips.xml", useInternalNodes=TRUE)
  else
    doc <- xmlTreeParse(file.path(etc, "tooltips.xml"), useInternalNodes=TRUE)

  for (tt in getNodeSet(doc, "//tooltip"))
  {
    # 100110 format the tooltip. blank lines are retained, but other
    # line breaks are ignored.
    tip <- gsub("XoX", "\\\n\\\n",
                gsub("\n *", " ",
                     gsub("\n *\n *", "XoX", xmlValue(tt))))
    wd <- theWidget(xmlGetAttr(tt, 'widget'))
    wd["tooltip-text"] <- Rtxt (tip) # 100408 Space after Rtxt is intentional.
#print(tip)
    # 090214 The MS/Windows RGtk2 is compiled with an older GTK, even
    # though a user might have GTK 2.12.8 installed.  Thus,
    # setTooltipText is not avilable and so we use the above setting
    # of the property rather than using the function.
    
    # theWidget(xmlGetAttr(tt, 'widget'))$setTooltipText(xmlValue(tt))
  }
}
