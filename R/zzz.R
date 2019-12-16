# R Data Scientist: Gtk interface to R for Data Science
#
# Time-stamp: <2018-08-25 08:45:31 Graham.Williams@togaware.com>
#
# Copyright (c) 2009-2018 Togaware Pty Ltd

# These could be in rattle.R

crs <- new.env() # Current Rattle State
crv <- new.env() # Current Rattle Variables

if (! exists("RATTLE.DATA")) RATTLE.DATA <- NULL
if (! exists("RATTLE.SCORE.IN")) RATTLE.SCORE.IN <- NULL
if (! exists("RATTLE.SCORE.OUT")) RATTLE.SCORE.OUT <- NULL

on_aboutdialog_response <- function(object, ...)
{
  RGtk2::checkPtrType(object, "GtkWidget")
  w <- RGtk2::.RGtkCall("S_gtk_widget_destroy", object, PACKAGE = "RGtk2")
  return(invisible(w))
}

.onLoad <- function(libname, pkgname)
{
  # print("LOAD")

  # 101009 This is called first when the package is loaded into the
  # library as through the library or require commands. Note that a
  # user may have saved a workspace into .RData, and that seems to
  # note the dependency upon the rattle package, and so when R starts
  # up and looks to load the .RData file, it first loads this
  # package. Thus anything here is evaluated first, and then
  # overridden by things from .RData. Thus, the creation of
  # environments, which used to be in here, should be in .onAttach,
  # which is called when the user explicitly loads the package.
  #
  # Thus, when no .RData exists, LOAD and ATTACH happen when there is
  # a library(rattle). When there is an .RData, LOAD happens when R
  # starts up, followed by loading .RData, and ATTACH happens when
  # there is a library(rattle).
  #
  # 080417 The R manual for .onLoad also says to use .onAttach for
  # startup messages, which makes sense as we want to see them when
  # the user loads the package, not when R (possibly on startup)
  # decides to load the package.
}

.onAttach <- function(libname, pkgname)
{
  # print("ATTACH")

  # 101009 This is executed when the package becomes visible to the
  # user, which is usually following a library(rattle). Note that on
  # restoring an .RData file the package might get loaded on starting
  # up R and thus the .onLoad is executed then, but the ATTACH code
  # won't get loaded until we do library(rattle).
  
  # 090315 Create the crs environment here. It is defined here and
  # then also reset in rattle() so that R CMD check would not complain
  # about knowing nothing of crs (after removing the crs <<-
  # assigments throughout rattle)!

#  crs <<- new.env() # 121212 Moved to top of file so not global

  # 090207 Create the global crv environment for Rattle. Once again,
  # this is a deviation from Chamber's Prime Directive, but is akin to
  # the use of option.  It is defined here so that it is globally
  # known and so that plugins can override options. We generally
  # include here the options that can be overridden by a plugin.
  
#  crv <<- new.env() # 121212 Moved to top of file so not global

  # 100820 GtkBuilder update. Use GtkBuilder as default, except if R
  # version < 2.12.0 on MS/Windows. Here is a collection of tests performed:
  #
  # OS      R	   release RGtk2     rattle GUI	       Gtk+	Date	Test Notes
  #
  # Ubuntu  2.11.1	   2.12.18   2.5.40 GtkBuilder 2.20.1	100822	OK
  # Ubuntu  2.11.1	   2.12.18   2.5.40 GladeXML   2.20.1	100822	OK
  #
  # Ubuntu  2.12.0 r52791  2.12.18   2.5.40 GtkBuilder 2.20.1	100822	OK
  # Ubuntu  2.12.0 r52791  2.12.18   2.5.40 GladeXML   2.20.1	100822	OK
  #
  # Windows 2.11.1	   2.12.18   2.5.40 GladeXML   2.12.9-2	100822	OK
  #
  # Windows 2.12.0 r52771  2.12.18-5 2.5.40 GtkBuilder 2.12.9-2	100822	FAIL (1)
  # Windows 2.12.0 r52771  2.12.18-5 2.5.40 GladeXML   2.12.9-2	100822	FAIL (2)
  #
  # Windows 2.12.0 r52771  2.12.18-5 2.5.40 GtkBuilder 2.16.6	100822	OK   (3)
  # Windows 2.12.0 r52771  2.12.18-5 2.5.40 GladeXML   2.16.6	100822	FAIL (4)
  #
  # Notes
  #
  # (1) This FAIL is expected: we get an unhandled 'requires' tag
  #     which is not supported in Gtk 2.12.9.
  #
  # (2) This FAIL is expected: libglade is not loaded.
  #
  # (3) gtk 2.16.6 eis dated 2010-02-24.
  #
  # (4) This FAIL is expected: libglade is not loaded.
  # 
  # The test version of R 2.12.0 was downloaded from
  #
  # https://cran.r-project.org/bin/windows/base/rtest.html

  crv$appname <- "Rattle"
  crv$projext <- ".rattle"

  # 20170624 Note if Microsoft R Server

  crv$mrs <- packageIsAvailable("RevoScaleR")
  
  # 111204 Fix issue of Mac OS/X not ignoring warnings in the .ui
  # file, so use an alternative one for now until work out permanent
  # fix. 130309 No longer an issue since the ubuntu string is no
  # longer inserted into the .ui file. So use the standard .ui
  # file. 130402 Revert to using rattle_macosx.ui - rattle.ui does not
  # yet work?
  
  crv$rattleUI <- "rattle.ui"
  # if (Sys.info()["sysname"] == "Darwin") crv$rattleUI <- "rattle_macosx.ui"

  crv$log.intro <- paste0("# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.",
                          "\n# It is free (as in libre) open source software.",
                          "\n# It is licensed under the GNU General Public License,",
                          "\n# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.",
                          "\n# Rattle was written by Graham Williams with contributions",
                          "\n# from others as acknowledged in 'library(help=rattle)'.",
                          "\n# Visit https://rattle.togaware.com/ for details.")
  crv$support.msg <- sprintf(Rtxt("If this is a bug please contact %s.\n\n%s"),
                             "support@togaware.com",
                             Rtxt("Please supply the output of rattleInfo()",
                                  "and the steps required to replicate the problem."))
  crv$library.command <- "library(rattle)"

  # 101009 Record version for each so we can see when we might have
  # these restored from a .RData file automatically on startup.

  crv$version <- VERSION
  crs$version <- VERSION

  # Some global constants

  # 160901 XDF Default number if rows for a preview.

  crv$xdf.preview <- 1e4
  
  # Default seed to use

  crv$seed <- 42
  
  # 091130 Use UTF-8 as the default encoding for files. This certainly
  # works okay on GNU/Linux. On Vista I see ISO8859-1 as the default
  # and Acken sees CP932 for Japanese.
  
  crv$csv_encoding <- "UTF-8"

  # 100410 All monofonts come out vertically aligned in Japanese???
  
  crv$textview.font <- "monospace 10" # Japanese vertically aligned - bad periods/commas
  # crv$textview.font <- "Courier New 10" # Okay but not found on MS/Windows
  # crv$textview.font <- "Bitstream Vera Sans Mono 10" # Better?
  # crv$textview.font <- "Andale Mono 10" # Not very nice
  # crv$textview.font <- "Sans Italic 12" # For fun.
  
  crv$show.timestamp <- TRUE
  ## crv$tooltiphack <- FALSE
  crv$close <- "ask"

  crv$sample_csv <- "weather"
  crv$sample_xdf <- "weather"

  # 100402 Record whether the Execute button is currently in
  # action. The problem is that in loading a CSV file if the Execute
  # button is double clicked then it starts twice, before it knows
  # what it is doing, and loads the data twice, then complains about
  # two targets selected.

  crv$executing <- FALSE

  # 101127 I originally turned toolbar text off since on GNU/Linux it
  # was chopping the text. But now on moving to GTK 2.20 it all looks
  # okay again, so include the text.
  
  crv$toolbar.text <- TRUE
  
  # 090525 Always load tooltips - now use Settings option to enable on
  # GNU/Linux. 090622 But on older installations we still get the
  # Invalid property error so for now on Unix do not support tooltips.
  
  # 090601 Add the crv$load.tooltips option, so it can be turned off
  # on the command line before starting rattle, since older GTK
  # version has issue: Invalid property tooltip-text!
  
  crv$load.tooltips <- TRUE
  
#100110 testing for Rtxt
#  if (.Platform$OS.type == "unix")
#    crv$load.tooltips <- FALSE # Not working in general on Linux
  
  crv$verbose <- TRUE # Add sub titles to plots ...

  crv$max.categories <- 10 # Above which target assumed numeric, not categoric
  crv$max.vars.correlation <- 40 # Correlation slows down too much
  crv$export.to.c.available <- FALSE # No export to C implemented yet
  crv$show.warnings <- TRUE # 090207 Show test/train warning.
  crv$project.extensions <- c("rattle", "rstat") # Extensions for projects  
  crv$ident.min.rows <- 300 # Unique factors/ints > than this are idents
  crv$default.train.percentage <- 70 # The default sample percentage value.
  crv$default.sample <- "70/15/15" # The default train/validate/test split.

  crv$log_width <- 70 # The wrap column for the log tab.
  
  # Popup a warning above this many rows in the table being loaded via
  # ODBC

  crv$odbc.large <- 50000
  
  # Log constants

  crv$start.log.comment <- "\n\n# "	# Assume paste with sep=""
  crv$end.log.comment   <- "\n\n"	# Assume paste with sep=""

  # Model defaults
  
  crv$cluster.report.max.obs <- 4000
  crv$scatter.max.vars <- 5
  
  crv$rpart.cp.default        <- 0.010
  crv$rpart.minsplit.default  <- 20
  crv$rpart.minbucket.default <- 7
  crv$rpart.maxdepth.default  <- 30 # Used to be 3, now 30 as default in rpart.

  crv$ada.ntree.default   <- 50

  crv$rf.ntree.default    <- 500
  crv$rf.mtry.default     <- 10
  crv$rf.sampsize.default <- ""

  # Evaluate

  # 121212 Check exists rather than just default to NULL.
  #.RATTLE.DATA <<- NULL
  #.RATTLE.SCORE.IN <<- NULL
  #.RATTLE.SCORE.OUT <<- NULL
  
  # 090309 We set some other environment variables for convenience.

  # 121212 No Longer required since using environments 
  #crv$rattleGUI <- NULL
  #Global_.rattleGUI <<- NULL
  #viewdataGUI <- NULL
  #on_aboutdialog_response <<- NULL
  
  # 090206 How to not display the welcome message if quietly=TRUE? A
  # user has the option to suppressPackageStartupMessages().

  # 091221 The Rtxt does not seem to work from the rattle.R file, so
  # do it here again.
  
  COPYRIGHT <- sprintf(Rtxt("Copyright (c) 2006-2018 %s."), "Togaware Pty Ltd")

  msg <- paste(Rtxt("Rattle: A free graphical interface",
                    "for data science with R."), "\n",
               Rtxt("Version"), " ", VERSION, " ",
               COPYRIGHT, "\n",
#LICENSE
               Rtxt("Type 'rattle()' to shake, rattle, and roll your data."),
               "\n",
               sep="")
    
  if ("rattle" %in% getOption("defaultPackages"))
    rattle()
  else
    packageStartupMessage(msg, appendLF=FALSE)
}
