\name{weatherAUS}
\docType{data}
\alias{weatherAUS}
\alias{locationsAUS}
\title{Daily weather observations from multiple Australian weather stations.}
\description{

  Daily weather observations from multiple locations around Australia,
  obtained from the Australian Commonwealth Bureau of Meteorology and
  processed to create this realtively large sample dataset for
  illustrating analytics, data mining, and data science using R and
  Rattle.

  The data has been processed to provide a target variable
  \code{RainTomorrow} (whether there is rain on the following day -
  No/Yes) and a risk variable \code{RISK_MM} (how much rain recorded in
  millimeters). Various transformations are performed on the data.

  The \code{weatherAUS} dataset is regularly updated an updates of this
  package usually correspond to updates to this dataset. The data is
  updated from the Bureau of Meteorology web site.

  The \code{locationsAUS} dataset records the location of each weather
  station.

  The source dataset comes from the Australian Commonwealth Bureau of
  Meteorology. The Bureau provided permission to use the data with the
  Bureau of Meteorology acknowledged as the source of the data, as per
  email from Cathy Toby (C.Toby@bom.gov.au) of the Climate Information
  Services of the National Climate Centre, 17 Dec 2008.

}
\usage{weatherAUS}
\format{

  The \code{weatherAUS} dataset is a data frame containing over 140,000
  daily observations from over 45 Australian weather stations.

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

  Observations were drawn from numerous weather stations.  The daily
  observations are available from www.bom.gov.au.  Copyright
  Commonwealth of Australia 2010, Bureau of Meteorology.

  Definitions adapted from www.bom.gov.au.

}

\references{

  Package home page: \url{https://togaware.com/projects/rattle//}. Data source:
  www.bom.gov.au.

}

\author{\email{Graham.Williams@togaware.com}}

\seealso{

  \code{\link{weather}}, \code{\link{audit}}.

}

\keyword{datasets}
