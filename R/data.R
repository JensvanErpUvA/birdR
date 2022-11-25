#' era
#'
#' era5 data for the Netherlands
#'
#'
#' @format ## `era`
#' \describe{
#'   \item{date_time}{date and hour of the day}
#'   \item{lat, lon}{latitude and longitude in EPSG:4326}
#'   \item{U10m, V10m}{U and V component of era5 dataset}
#' }
#' @source <https://cds.climate.copernicus.eu/#!/search?text=ERA5&type=dataset>
"era" #TODO add correct source link

#' landmask
#'
#' landmask exported from the luchterduinen robin database
#'
#'
#' @format ## `landmask`
#' \describe{
#'   \item{timestamp}{UTC time}
#'   \item{landmask}{landmask value}
#' }
"landmask"

#' tracks_all
#'
#' trajectories
#'
#'
#' @format ## `tracks_all`
#' \describe{
#'   \item{id}{unique_identifier}
#'   \item{timestamp_start}{UTC timestamp corresponding to the first location of a bird-track}
#'   \item{timestamp_end}{UTC timestamp corresponding to the last location of a bird-track}
#'   \item{trajectory_time}{offset seconds from timestamp_start for each location in a bird-track}
#'   \item{trajectory}{linestring geometry corresponding to a bird-track}
#' }
"tracks_all"
#NOTE for package we cannot store the whole tracks_all dataset if we want to publish in CRAN. We could store a sample and make a data package for the complete dataset which we can store and archive in github and figshare/zenodo.

#' turbines
#'
#' turbines of all windparks from the Netherlands
#'
#'
#' @format ## `turbines`
#' \describe{
#'   \item{OBJECTID}{unique identifier}
#'   \item{NAAM}{windpark name}
#'   \item{geometry}{point geometry corresponding to a turbine location}
#' }
"turbines"

