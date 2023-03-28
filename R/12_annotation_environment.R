#' @importFrom data.table fread
#' @importFrom lubridate round_date ymd_hms
#'
#' @title add_env
#' @name add_env
#' @description annotate a track dataset with environmental data
#'
#' @param x data.frame or data.table object with tracks
#' @param env data.frame or data.table with environmental data, or path to dataset
#' @param variables character vector with environmental variables to be annotated. the first index should always be the time
#' @param unit the annotation unit (hours)
#'
#' @return returns the track dataset with environmental variables annotated
#'
#' @details
#'
#' @export
#'
#' @examples
#'
#'
add_env <- function(x=tracks_step1,
                    env=era,
                    variables=c("date_time","U10m","V10m"),
                    unit="hours"){
  ## Track data
  ## if not data.table
  if (class(x)[1]!="data.table") {
    x <- as.data.table(x)
  }

  ## Environmental data
  ## if path
  if(is.character(env)){ # TODO double check if correct
    env <- fread(env)
  }
  ## if not data.table
  if (class(env)[1] != "data.table") {
    env <- as.data.table(env)
  }
  ## if API
  # TODO API statement might also be character input - To Be Checked
  ## if time not POSIXct
  if (class(env[,get(variables[1])])[1] != "POSIXct"){
    env[,get(variables[1]):=ymd_hms(get(variables[1]), tz="UTC")]
  }

  ## Get nearest time of each track, based on timestamp near mid point of track
  # Use duration if available
  if('duration' %in% colnames(x)){
    x[,nearest_time:=round_date((timestamp_start + 0.5*duration),unit=unit)]
  } else { ## Otherwise use start and end time
    x[,nearest_time:=round_date((timestamp_start + 0.5*(timestamp_end - timestamp_start)),unit=unit)]
  }
  ## Create exception for last timestamp in the dataset if the env data doesn't hold it
  ## e.g., if time rounds up to next day while env data is available until last hour of previous day
  x[!(nearest_time %in% env[,get(variables[1])]), nearest_time:=env[,get(variables[1])[nrow(env)]]]

  ## Annotate environmental conditions to the tracks based on overlap in time
  x <- merge(x,env[, ..variables],by.x="nearest_time",by.y=variables[1])
  ## Remove nearest time
  x[,"nearest_time":=NULL]

  return(x)
}



