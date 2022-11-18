
#' displacement
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


#' direction
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


#' airspeed
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


#' movement
#' @name movement
#' @description annotate the trajectory with environmental data
#'
#' @param TRACKS sf object with tracks
#' @param VAR vector with movement metrics to add to tracks
#' @param crs
#'
#'
#'
#' @return returns annotated trajectories with movement information
#'
#' @details
#'
#' @export
#'
#' @examples
#'
#'
movement <- function(TRACKS,
                     VAR=c('n','duration', 'length', 'groundspeed', 'displacement', 'direction','dot'), # NOTE maybe timestamp_start and timestamp_end can be added as well
                     crs=loc_epsg){
  # all possible variables to compute
  vars_all <- c('n','duration', 'length', 'groundspeed', 'displacement', 'direction','dot')

  ## In RR system track durations per plot are given as a long character string, pull apart and make into list of numbers

  ## THIS IS ROBIN SPECIFIC SO SHOULD ONLY BE PERFORMED FOR ROBIN
  TRACKS[,traj_time:=lapply(
    strsplit(
      substr(trajectory_time,2,nchar(trajectory_time)-1),
      split = ','),
    as.double)
  ]


  TRACKS[,"trajectory_time":=NULL]
  setnames(TRACKS,"traj_time","trajectory_time")


  ## Number of points in the track
  TRACKS[,n := sapply(trajectory_time, length)]

  ## trajectory timestamp_start, timestamp_end
  # NOTE if timestamp_end and timestamp_start does not exist return error and ask to create variables? Alternative

  ## Duration (seconds)
  TRACKS[,duration := as.numeric(timestamp_end-timestamp_start)]
  print('computed duration')
  ## Length (metres)
  TRACKS[,length := as.numeric(st_length(st_transform(trajectory, crs=crs)))]
  print('computed length')

  ## Groundspeed (metres per second)
  if(length(VAR[grepl('groundspeed',VAR)]) > 0){
    TRACKS[,groundspeed := length/duration]
    print('computed groundspeed')
  }

  # displacement
  if(length(VAR[grepl('displacement | dot',VAR)]) > 0){
  TRACKS[,displacement := mapply(get_displacement, trajectory, n)]
  print('computed desplacement')
  }

  # direction
  if(length(VAR[grepl('direction',VAR)]) > 0){
  TRACKS[,direction := mapply(get_direction, trajectory, n)]
  print('computed direction')
  }

  if(length(VAR[grepl('dot',VAR)]) == 1){
  TRACKS[,dot:=displacement/duration]
  }

  # Variables that we do not want to include are removed again
  exclude <- vars_all[!vars_all %in% VAR]   # see here if not working https://stackoverflow.com/questions/9202413/how-do-you-delete-a-column-by-name-in-data-table
  TRACKS[, c(exclude):=NULL]  # remove columns that are used for calculations but should not be in output
  print(paste0('removed columns: ',exclude, collapse=' '))

  return(TRACKS)
}


