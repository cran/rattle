rattleInfo <- function(all.dependencies=FALSE,
                       include.not.installed=FALSE,
                       include.not.available=FALSE,
                       include.libpath=FALSE)
{
  # Alternatives include:
  # http://mirror.aarnet.edu.au/pub/CRAN/

  # 120221 Brian Ripley seems to be checking for packages using
  # installed.packages() and warning about it being a "very slow way
  # to find information on one or a small number of packages," as
  # stated on the man page, and as I am very aware. He goes on to say:
  # "In addition, many of you are using it to find out if a package is
  # installed, when actually you want to know if it is usable (it
  # might for example be installed for a different architecture or
  # require a later version of R), for which you need to use
  # require()." I have already fixed my usage of installed.packages()
  # within packageIsAvailable(), where there was a better way of
  # checking for an installed package. But here I think it might be
  # appropriate to use it.
  
  iv <- utils::installed.packages()
  av <- available.packages(contriburl=contrib.url("http://cran.r-project.org"))

  cat(sprintf("Rattle: version %s cran %s\n",
              crv$version, av["rattle", "Version"]))

  up <- NULL # List of packages that can be upgraded.

  if (compareVersion(av["rattle", "Version"], crv$version) == 1)
    up <- "rattle"
  
  cat(sprintf("%s\n",
              sub(" version", ": version", version$version.string)))

  cat("\n")
  si <- Sys.info()
  for (i in seq_along(si))
    cat(sprintf("%s%s: %s\n", toupper(substr(names(si)[i], 1, 1)),
                substring(names(si)[i], 2), si[i]))

  cat("\nInstalled Dependencies\n")

  if (all.dependencies)
  {
    cat("  please wait a few seconds whilst all dependencies are searched for...")

    suppressPackageStartupMessages({
      require(pmml, quietly=TRUE)
      require(methods, quietly=TRUE)
      require(colorspace, quietly=TRUE)
      require(cairoDevice, quietly=TRUE)
      require(RGtk2, quietly=TRUE)
      require(utils, quietly=TRUE)
      require(XML, quietly=TRUE)
      require(graph, quietly=TRUE, warn.conflicts=FALSE)

      require(RBGL, quietly=TRUE)
      require(bitops, quietly=TRUE)
      require(grid, quietly=TRUE)

      if (! require(pkgDepTools, quietly=TRUE))
      {
        source("http://bioconductor.org/biocLite.R")
        pkg <- "pkgDepTools"
        biocLite("pkgDepTools")
        cmd <- sprintf("require(%s, quietly=TRUE)", pkg)
        eval(parse(text=cmd))
      }
      if (! require(Rgraphviz, quietly=TRUE))
      {
        source("http://bioconductor.org/biocLite.R")
        biocLite("Rgraphviz")
        require(Rgraphviz, quietly=TRUE)
      }
    })
      
    cran.repos <- "http://cran.r-project.org"
    if (isWindows())
      cran.deps <- pkgDepTools::makeDepGraph(cran.repos, type="win.binary", dosize=TRUE)
    else
      cran.deps <- pkgDepTools::makeDepGraph(cran.repos, type="source", dosize=TRUE)

    deps <- c("rattle", names(graph::acc(cran.deps, "rattle")[[1]]))
    cat("\n")
  }    
  else
    deps <- strsplit(gsub("\\n", " ",
                          paste0(gsub(' \\([^\\)]+\\)', '', iv["rattle", "Depends"]),
                                 ", ",
                                 gsub(' \\([^\\)]+\\)', '', iv["rattle", "Suggests"])
                                 )), ", ")[[1]]

  for (p in deps)
  {
    if (! p %in% rownames(av))
    {
      if (include.not.available) cat(sprintf("%s: not available\n", p))
    }
    else if (! p %in% rownames(iv))
    {
      if (include.not.installed) cat(sprintf("%s: not installed\n", p))
    }
    else
      cat(sprintf("%s: version %s%s%s%s", p, iv[p,"Version"],
                  ifelse(compareVersion(av[p,"Version"], iv[p,"Version"]) == 1,
                         {
                           up <- c(up, p);
                           sprintf(" upgrade available %s", av[p,"Version"])
                         },
                         ""),
                  ifelse(include.libpath, paste("\t", iv[p,"LibPath"]), ""),
                  "\n"))
  }

  cat("\nThat was",
      sum(sapply(deps, function(p) p %in% rownames(iv))),
      "packages.\n")
  
  if (! is.null(up))
  {
    cat(sprintf(paste('\nUpdate the packages with either',
                      'of the following commands:\n\n ',
                      '> install.packages(c("%s"))\n\n ',
                      '> install.packages(rattleInfo(%s%s%s%s%s%s%s))\n\n'),
                paste(strwrap(paste(up, collapse='", "'),
                              width=60, exdent=23), collapse="\n"),
                ifelse(all.dependencies, "all.dependencies=TRUE", ""),
                ifelse(all.dependencies &&
                       (include.not.installed ||
                        include.not.available ||
                        include.libpath), ", ", ""),
                ifelse(include.not.installed, "include.not.installed=TRUE", ""),
                ifelse(include.not.installed &&
                       (include.not.available ||
                        include.libpath), ", ", ""),
                ifelse(include.not.available, "include.not.available=TRUE", ""),
                ifelse(include.not.available &&
                       include.libpath, ", ", ""),
                ifelse(include.libpath, "include.libpath=TRUE", "")))
    if (isWindows() && "rattle" %in% up)
      cat("Detach rattle (and other attached packages) before updating:\n\n ",
          '> detach("rattle")\n\n')
    cat("Alternatively update all installed packages:\n\n ",
        '> update.packages()\n\n')

  }

  invisible(up)

}
