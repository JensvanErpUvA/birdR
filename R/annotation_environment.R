#' @importFrom data.table fread
#' @importFrom lubridate round_date ymd_hms
#'
#' @title annotation
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
weather <- function(x=tracks_step1,
                    era5=era,
                    var=c("U10m","V10m"),
                    unit="hours"
                    ){
  ## Load in the wind data
  # if PATH name
  if(is.character(era5)){ # TODO double check if correct
    era5 <- fread(era5)
  }
  # if API
  # TODO API statement might also be character input - To Be Checked

  ## Make date_time into POSIXct:
  era5[,date_time:=ymd_hms(date_time, tz="UTC")]

  # use start and end time
  if(!'duration' %in% colnames(x)){
  x[,nearest_hour:=round_date((timestamp_start + (timestamp_end - timestamp_start)),unit=unit)]
  } else {
  ## To add weather conditions for each track, calculate the rounded hour of the half-way point of each track first
  x[,nearest_hour:=round_date((timestamp_start+0.5*duration),unit=unit)]
  }
  ## Create exception for last hour in the dataset is the weather data holds the same
  x[!(nearest_hour %in% era5$date_time), nearest_hour:=era5$date_time[nrow(era5)]]

  ## Add wind conditions (in this case U and V component at 10m ASL) to the tracks based on their timestamp
  var <- c("date_time",var)
  x <- x[era5[, ..var], on=.(nearest_hour=date_time)]
  ## Remove duplicate rows caused by hours without tracks (but with wind data available)
  x <- x[!is.na(id),]
  x[,"nearest_hour":=NULL]

  return(x)
}



