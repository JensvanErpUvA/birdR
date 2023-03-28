#'@importFrom data.table setnames
#'@importFrom geosphere bearingRhumb distHaversine
#'@importFrom sf st_coordinates st_length st_transform
#'
#'@title get_displacement
#' @name get_displacement
#' @description Calculate track displacement (from start to end of track) using the Haversine formula.
#' The Haversine formula assumes a spherical earth, so a slight error on actual (max ~0.5%) is expected
#' The Earth's arithmetic mean radius is taken, as distHaversine uses nomrnal equatorial Earth radius by default which introduces larger errors
#'
#' @param trajectory track trajectory as st_LINESTRING in wgs84 projection (lon/lat)
#' @param n number of points in the track
#'
#' @return Returns the track displacement in metres
#'
get_displacement <- function(trajectory, n){ # NOTE this functions is not exported and only used internally
  p1 <- st_coordinates(trajectory)[1,1:2]
  p2 <- st_coordinates(trajectory)[n,1:2]

  return(distHaversine(p1,p2,r=6371001))
}

#'@title get_direction
#' @name get_direction
#' @description Calculate track direction (from start to end of track) along a rhumb line
#'
#' @param trajectory track trajectory as st_LINESTRING in wgs84 projection (lon/lat)
#' @param n number of points in the track
#'
#' @return Returns the track direction in radians from North
#'
get_direction <- function(trajectory, n){ # NOTE this function is not exported and only used internally
  p1 <- st_coordinates(trajectory)[1,1:2]
  p2 <- st_coordinates(trajectory)[n,1:2]

  return(bearingRhumb(p1, p2)/180*pi)
}

#'@title get_airspeed
#' @name get_airspeed
#' @description Calculates average airspeed using u- and v- windcomponents, average groundspeed and average track direction
#'
#' @param groundspeed groundspeed in metres per second
#' @param direction direction in radians from North
#' @param wind_u wind u-component in metres per second
#' @param wind_v wind v-component in metres per second
#'
#' @return Returns average airspeed in meter per second
#'
#' @export
#'
get_airspeed <- function(groundspeed,
                         direction,
                         wind_u,
                         wind_v){
  ## Calculate displacement on the x and y axis from the average groundspeed and direction of each track
  disp_x <- groundspeed*sin(direction)
  disp_y <- groundspeed*cos(direction)

  ## Get discplacement relative to air by subtracting wind components
  air_x <- disp_x-wind_u
  air_y <- disp_y-wind_v

  ## Calculate total airspeed
  airspeed <- sqrt((air_x^2) + (air_y^2))
  return(airspeed)
}

# TODO This function should be moved to uvaRR
#' @title fix_trajectorytime
#' @name fix_trajectorytime
#' @description Converts the RR trajectory time, which is given as long character, to a vector of integers
#'
#' @param tracks dataset of tracks (data.table) with trajectory_time column.
#'
#'
#'
#' @return Returns the track data.table with converted trajectory_time
#'
#' @details
#'
#' @export
#'
#' @examples
fix_trajectorytime <- function(tracks){
  ## In RR system track durations per plot are given as a long character string, pull apart and make into list of numbers

  tracks[,trajectory_time:=lapply(
    strsplit(
      substr(trajectory_time,2,nchar(trajectory_time)-1),
      split = ','),
    as.double)
  ]
  return(tracks)
}

#' @title movement
#' @name movement
#' @description Annotate metrics to a track data.table. The function requires trajectory, trajectory_time, timestamp_start and timestamp_end.
#' Metrics are in meters (lengt/displacement), seconds (duration), meters per second (groundspeed/dot), and radians North (direction).
#'
#' @param x data.table with tracks
#' @param metrics character vector with movement metrics to add to tracks
#' @param crs local coordinate reference system as epsg
#'
#'
#'
#' @return Returns annotated data.table with movement information.
#'
#' @details
#'
#' @export
#'
#' @examples
#'
#'
movement <- function(x,
                     metrics=c('n','duration', 'length', 'groundspeed', 'displacement', 'direction','dot'),
                     crs=loc_epsg){
  ## List all possible variables to compute
  metrics_all <- c('n','duration', 'length', 'groundspeed', 'displacement', 'direction','dot')

  ## Number of points in the track
  if(length(metrics[grepl('n|displacement|direction|dot',metrics)]) > 0){
    x[,n := sapply(trajectory_time, length)]
    print('Computed number of points')
  }

  ## Duration (seconds)
  if(length(metrics[grepl('duration|groundspeed|dot',metrics)]) > 0){
    x[,duration := as.numeric(timestamp_end-timestamp_start)]
    print('Computed duration')
  }

  ## Length (metres)
  if(length(metrics[grepl('length|groundspeed',metrics)]) > 0){
    x[,length := as.numeric(st_length(st_transform(trajectory, crs=crs)))]
    print('Computed length')
  }

  ## Groundspeed (metres per second)
  if(length(metrics[grepl('groundspeed',metrics)]) > 0){
    x[,groundspeed := length/duration]
    print('Computed groundspeed')
  }

  ## Displacement (metres)
  if(length(metrics[grepl('displacement|dot',metrics)]) > 0){
    x[,displacement := mapply(get_displacement, trajectory, n)]
    print('Computed displacement')
  }

  ## Direction (radians North)
  if(length(metrics[grepl('direction',metrics)]) > 0){
    x[,direction := mapply(get_direction, trajectory, n)]
    print('Computed direction')
  }

  ## Displacement over time (dot, metres per second)
  if(length(metrics[grepl('dot',metrics)]) == 1){
    x[,dot:=displacement/duration]
  }

  ## Variables that are not requested but were needed for calculations are removed again
  exclude <- metrics_all[!metrics_all %in% metrics]
  ##Ssee here if not working https://stackoverflow.com/questions/9202413/how-do-you-delete-a-column-by-name-in-data-table
  if(length(exclude) > 0){
    x[, c(exclude):=NULL]  ## Remove columns that are used for calculations but should not be in output
  }
  return(x)
}


