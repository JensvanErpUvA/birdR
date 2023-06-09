% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/31_detecting_negative_observation_bias.R
\name{nob}
\alias{nob}
\title{nob}
\usage{
nob(x, m, unit = "hour", period = NULL)
}
\arguments{
\item{x}{track data with minimum the geometry, timestamp_start and timestamp_end}

\item{m}{masking intensity at original temporal resolution}

\item{unit}{character varying giving the aggregation unit, default hours. See floor_date/round_date for all the possible units}

\item{period}{default NULL, optional start and end time, by default the time period is based on start and end date of the track data}
}
\value{
returns the tracks with era5 annotation
}
\description{
annotate the trajectory with environmental data
}
\details{

}
\examples{



}
