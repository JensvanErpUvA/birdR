#' @importFrom sf
#'
#'
#' annotation
#' @name annotation
#' @description annotate the trajectory with environmental data
#'
#' @param TRACKS sf object with tracks
#' @param ERA path/file/API ERA5 dataset with annotation variables,
#' @param VAR vector with ERA5 variables to be annotated
#' @param unit the annotation unit (hours)
#'
#' @return returns the tracks with era5 annotation
#'
#' @details
#'
#' @export
#'
#' @examples
#'
#'
annotation <- function(TRACKS=tracks_step1,
                       ERA=ERA5_database,
                       VAR=c("U10m","V10m"),
                       unit="hours"
                       ){
  ## Load in the wind data
  # if PATH name
  if(is.character(ERA)){ # TODO double check if correct
  ERA <- fread(ERA)
  }
  # if API
  # TODO API statement might also be character input - To Be Checked

  ## Make date_time into POSIXct:
  ERA[,date_time:=ymd_hms(date_time, tz="UTC")]

  # use start and end time
  if(!'duration' %in% colnames(TRACKS)){
  TRACKS[,nearest_hour:=round_date((timestamp_start + (timestamp_end - timestamp_start)),unit=unit)]
  } else {
  ## To add weather conditions for each track, calculate the rounded hour of the half-way point of each track first
  TRACKS[,nearest_hour:=round_date((timestamp_start+0.5*duration),unit=unit)]
  }
  ## Create exception for last hour in the dataset is the weather data holds the same
  TRACKS[!(nearest_hour %in% ERA$date_time), nearest_hour:=ERA$date_time[nrow(ERA)]]

  ## Add wind conditions (in this case U and V component at 10m ASL) to the tracks based on their timestamp
  TRACKS <- TRACKS[ERA[,c("date_time",VAR)], on=.(nearest_hour=date_time)]
  ## Remove duplicate rows caused by hours without tracks (but with wind data available)
  TRACKS <- TRACKS[!is.na(id),]
  TRACKS[,"nearest_hour":=NULL]



  return(x)
}
