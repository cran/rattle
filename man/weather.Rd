\name{weather}
\docType{data}
\alias{weather}
\title{Sample dataset of daily weather observations from Canberra
  airport in Australia.}
\description{

  One year of daily weather observations collected from the Canberra
  airport in Australia was obtained from the Australian Commonwealth
  Bureau of Meteorology and processed to create this sample dataset for
  illustrating data mining using R and Rattle.
  
  The data has been processed to provide a target variable
  \code{RainTomorrow} (whether there is rain on the following day -
  No/Yes) and a risk variable \code{RISK_MM} (how much rain recorded in
  millimetres). Various transformations were performed on the source
  data. The dataset is quite small and is useful only for repeatable
  demonstration of various data science operations. 

  The source dataset is Copyright by the Australian Commonwealth Bureau
  of Meteorology and is provided as part of the rattle package with
  permission.

}
\usage{weather}
\format{

  The \code{weather} dataset is a data frame containing one year of
  daily observations from a single weather station (Canberra).

  \describe{

    \item{\code{Date}}{The date of observation (a Date object).}

    \item{\code{Location}}{The common name of the location of the
    weather station.}

    \item{\code{MinTemp}}{The minimum temperature in degrees celsius. }

    \item{\code{MaxTemp}}{The maximum temperature in degrees celsius. }

    \item{\code{Rainfall}}{The amount of rainfall recorded for the day in mm. }
    
    \item{\code{Evaporation}}{The so-called Class A pan evaporation (mm)
    in the 24 hours to 9am.}

    \item{\code{Sunshine}}{The number of hours of bright sunshine in the day.}
    
    \item{\code{WindGustDir}}{The direction of the strongest wind gust
    in the 24 hours to midnight.}

    \item{\code{WindGustSpeed}}{The speed (km/h) of the strongest wind
    gust in the 24 hours to midnight.}

    \item{\code{Temp9am}}{ Temperature (degrees C) at 9am. }

    \item{\code{RelHumid9am}}{ Relative humidity (percent) at 9am. }

    \item{\code{Cloud9am}}{ Fraction of sky obscured by cloud at
      9am. This is measured in "oktas", which are a unit of eigths. It
      records how many eigths of the sky are obscured by cloud. A 0
      measure indicates completely clear sky whilst an 8 indicates that
      it is completely overcast.  }

    \item{\code{WindSpeed9am}}{
      Wind speed (km/hr) averaged over 10 minutes prior to 9am.
    }

    \item{\code{Pressure9am}}{
      Atmospheric pressure (hpa) reduced to mean sea level at 9am.
    }

    \item{\code{Temp3pm}}{ Temperature (degrees C) at 3pm. }

    \item{\code{RelHumid3pm}}{ Relative humidity (percent) at 3pm. }

    \item{\code{Cloud3pm}}{

      Fraction of sky obscured by cloud (in "oktas": eighths) at
      3pm. See Cload9am for a description of the values.

    }

    \item{\code{WindSpeed3pm}}{
      Wind speed (km/hr) averaged over 10 minutes prior to 3pm.
    }

    \item{\code{Pressure3pm}}{
      Atmospheric pressure (hpa) reduced to mean sea level at 3pm.
    }

    \item{\code{ChangeTemp}}{
      Change in temperature.
    }

    \item{\code{ChangeTempDir}}{
      Direction of change in temperature.
    }

    \item{\code{ChangeTempMag}}{
      Magnitude of change in temperature.
    }

    \item{\code{ChangeWindDirect}}{
      Direction of wind change.
    }

    \item{\code{MaxWindPeriod}}{
      Period of maximum wind.
    }

    \item{\code{RainToday}}{
      Integer: 1 if precipitation (mm) in the 24 hours to 9am exceeds
      1mm, otherwise 0.
    }

    \item{\code{TempRange}}{

      Difference between minimum and maximum temperatures (degrees C) in
      the 24 hours to 9am.

    }

    \item{\code{PressureChange}}{
      Change in pressure.
    }
    \item{\code{RISK_MM}}{
      The amount of rain. A kind of measure of the "risk".
    }

    \item{\code{RainTomorrow}}{

      The target variable. Did it rain tomorrow?

    }
  }
}
\source{

  The daily observations are available from
  \url{http://www.bom.gov.au/climate/data}.  Copyright Commonwealth of
  Australia 2010, Bureau of Meteorology.

  Definitions adapted from
  \url{http://www.bom.gov.au/climate/dwo/IDCJDW0000.shtml}
  
}

\references{

  Package home page: \url{http://rattle.togaware.com}. Data source:
  \url{http://www.bom.gov.au/climate/dwo/} and
  \url{http://www.bom.gov.au/climate/data}.

}

\author{\email{Graham.Williams@togaware.com}}

\seealso{

  \code{rattle.data::weatherAUS}, \code{\link{audit}}.

}

\keyword{datasets}
