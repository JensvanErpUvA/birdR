#'@importFrom data.table setnames
#'@importFrom fossil deg.dist
#'@importFrom geosphere bearingRhumb
#'@importFrom sf st_coordinates st_length st_transform
#'
#'@title get_displacement
#' @name get_displacement
#' @description calculate displacement using a vector of track geometries and a vector with the number of locations in a track
#'
#' @param trajectory trajectory column
#' @param n number of points
#'
#'
#'
get_displacement <- function(trajectory, n){ # NOTE this functions is not exported and only used internally
  longs <- st_coordinates(trajectory)[c(1,n),1]
  lats <- st_coordinates(trajectory)[c(1,n),2]

  return(deg.dist(longs[1], lats[1], longs[2], lats[2])*1000)
}

#'@title get_direction
#' @name get_direction
#' @description calculate direction using a vector of track geometries and a vector with the number of locations in a track
#'
#' @param trajectory trajectory column
#' @param n number of points
#'
#'
#'
get_direction <- function(trajectory, n){ # NOTE this function is not exported and only used internally
  longs <- st_coordinates(trajectory)[c(1,n),1]
  lats<- st_coordinates(trajectory)[c(1,n),2]

  return(bearingRhumb(c(longs[1], lats[1]), c(longs[2], lats[2]))/180*pi)
}

#'@title get_airspeed
#' @name get_airspeed
#' @description calculate airspeed using u- and v-component, average groundspeed and direction
#'
#' @param groundspeed groundspeed
#' @param direction direction
#' @param wind_u wind u / wind speed
#' @param wind_v wind v / wind direction
#'
#' @return airspeed in meter per second
#'
#' @export
#'
get_airspeed <- function(groundspeed,
                         direction,
                         wind_u,
                         wind_v){
  ## Calculate total displacement on the x and y axis from the average groundspeed and direction of each track
  disp_x <- groundspeed*sin(direction)
  disp_y <- groundspeed*cos(direction)

  air_x <- disp_x-wind_u
  air_y <- disp_y-wind_v

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
#' Metrics are in meters (lengt/displacement), seconds (duration), meters per second (groundspeed/dot), and degreees North (direction).
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
  TRACKS <- x
  VAR <- metrics
  ## List all possible variables to compute
  vars_all <- c('n','duration', 'length', 'groundspeed', 'displacement', 'direction','dot')

  ## Number of points in the track
  if(length(VAR[grepl('n|displacement|direction|dot',VAR)]) > 0){
    TRACKS[,n := sapply(trajectory_time, length)]
    print('Computed number of points')
  }

  ## Duration (seconds)
  if(length(VAR[grepl('duration|groundspeed|dot',VAR)]) > 0){
    TRACKS[,duration := as.numeric(timestamp_end-timestamp_start)]
    print('Computed duration')
  }

  ## Length (metres)
  if(length(VAR[grepl('length|groundspeed',VAR)]) > 0){
    TRACKS[,length := as.numeric(st_length(st_transform(trajectory, crs=crs)))]
    print('Computed length')
  }

  ## Groundspeed (metres per second)
  if(length(VAR[grepl('groundspeed',VAR)]) > 0){
    TRACKS[,groundspeed := length/duration]
    print('Computed groundspeed')
  }

  ## Displacement (metres)
  if(length(VAR[grepl('displacement|dot',VAR)]) > 0){
    TRACKS[,displacement := mapply(get_displacement, trajectory, n)]
    print('Computed displacement')
  }

  ## Direction (degrees North)
  if(length(VAR[grepl('direction',VAR)]) > 0){
    TRACKS[,direction := mapply(get_direction, trajectory, n)]
    print('Computed direction')
  }

  ## Displacement over time (dot, metres per second)
  if(length(VAR[grepl('dot',VAR)]) == 1){
    TRACKS[,dot:=displacement/duration]
  }

  # Variables that we do not want to include are removed again
  exclude <- vars_all[!vars_all %in% VAR] # see here if not working https://stackoverflow.com/questions/9202413/how-do-you-delete-a-column-by-name-in-data-table
  if(length(exclude) > 0){
    TRACKS[, c(exclude):=NULL]  # remove columns that are used for calculations but should not be in output
    print(paste0('Removed columns: ',exclude, collapse=' '))
  } else {
    print(paste0('Removed columns: ',0, collapse=' '))
  }
  return(TRACKS)
}


