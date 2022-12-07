#' @title get_time_inter
#' @name get_time_inter
#' @description time interval between consecutive points
#'
#' @param trajectory_time
#'
#' @return calculate the interval in seconds between consecutive locations in a trajectory
#'
#'
#' @details
#'
#'
get_dt <- function(trajectory_time){
  n_plots <- length(trajectory_time)
  time_inter <- NA
  time_inter[2:n_plots] <- trajectory_time[2:n_plots]-trajectory_time[1:(n_plots-1)]
  return(time_inter)
}


#' @title check_time_error
#' @name check_time_error
#' @description check time error
#'
#' @param time_inter time interval
#' @param min_time_inter minimum time interval
#'
#' @return
#'
#'
#'
#'
check_time_error <- function(time_inter, min_time_inter){
  return(length(which(unlist(time_inter)<=min_time_inter))>0)
}


#' @title first_last_removed
#' @name first_last_removed
#' @description Check if the first or last point was removed
#'
#' @param good_points good points of the trajectory
#' @param n number of points of a trajectory
#'
#' @return
#'
#'
#'
first_last_removed <- function(good_points, n){
  return(!(1 %in% good_points & n %in% good_points))
}

#' @title get_good_points
#' @name get_good_points
#' @description return the good points
#'
#' @param time_inter time interval
#' @param min_time_inter minimum time interval
#'
#' @return
#'
#'
#'
get_good_points <- function(time_inter, min_time_inter){
  ## First find bad points
  error_points <- which(unlist(time_inter) < min_time_inter)
  ## These are the indices of the second point of the two creating the time interval which falls below the threshold
  ## Collect the indices of the first points
  error_points <- sort(unique(c(error_points-1,error_points)))

  ## Gather all indices
  all_points <- 1:length(unlist(time_inter))

  ## Now  return the indices minus the errors
  return(all_points[-error_points])
}


#' @title fix_traj
#' @name fix_traj
#' @description function to correct the trajectory by retaining only the good points
#'
#' @param trajectory trajectory geometry
#' @param good_points good points of the trajectory
#'
#' @return
#'
#'
#'
fix_traj <- function(trajectory, good_points){
  crs <- st_crs(trajectory)
  new_traj <- st_sfc(st_linestring(st_coordinates(trajectory)[good_points,c("X","Y","Z","M")]), crs=crs)
  return(new_traj)
}

#' @title fix_vec
#' @name fix_vec
#' @description function to correct a vector by retaining only the good points
#'
#' @param vector vector of values
#' @param good_points good points of the trajectory / vector
#'
#' @return
#'
#'
#'
fix_vec <- function(vector, good_points){
  new_vector <- vector[good_points]
  return(list(new_vector))
}



#' @title fix_time_end
#' @name fix_time_end
#' @description function to correct the end time corresponding to a trajectory
#'
#' @param timestamp_start start time of the trajectory
#' @param trajectory_time trajectory times in seconds
#'
#' @return
#'
#'
fix_time_end <- function(timestamp_start, trajectory_time){
  new_time_end <- timestamp_start + trajectory_time[length(trajectory_time)]
  return(new_time_end)
}

#' @title fix_duration
#' @name fix_duration
#' @description function to correct the duration of a trajectory
#'
#' @param timestamp_start start time of the trajectory
#' @param timestamp_end end time of the trajectory
#'
#' @return
#'
#'
fix_duration <- function(timestamp_start, timestamp_end){
  duration <- floor(as.numeric(difftime(timestamp_end,timestamp_start, units = "secs")))
  return(duration)
}

#' @title fix_str_distance
#' @name fix_str_distance
#' @description function to correct the displacement of a trajectory
#'
#' @param trajectory trajectory geometry
#' @param good_points good points of the trajectory
#'
#' @return
#'
#'
fix_str_distance <- function(trajectory, good_points){
  longs <- st_coordinates(trajectory)[c(1,length(good_points)),1]
  lats<- st_coordinates(trajectory)[c(1,length(good_points)),2]

  displacement <- deg.dist(longs[1], lats[1], longs[2], lats[2])*1000
  return(displacement)
}

#' @title fix_direction
#' @name fix_direction
#' @description function to correct the direction of a trajectory
#'
#' @param trajectory trajectory geometry
#' @param good_points good points of the trajectory
#'
#' @return
#'
#'
fix_direction <- function(trajectory, good_points){
  longs <- st_coordinates(trajectory)[c(1,length(good_points)),1]
  lats<- st_coordinates(trajectory)[c(1,length(good_points)),2]

  direction <- bearingRhumb(c(longs[1], lats[1]), c(longs[2], lats[2]))/180*pi
  return(direction)
}



#' @title fix_geom
#' @name fix_geom
#' @description correct geometries
#'
#' @param x tracks
#' @param dt_min minimum time interval
#'
#' @return track table including only the tracks with erroneous points
#'
#' @export
#'
#' @details
#' Erroneous points exist within some tracks, identifiable by a very short time-interval
#' between those points and the next/previous point. Identify these intervals and remove
#' both points involved with that interval, as there is no way to tell which of these two
#' is faulty.
#'
fix_geom <- function(x, dt_min) {
  data <- x
  data[,trajectory_time_interval:=lapply(trajectory_time, get_dt)]
  print('calculate trajectory time interval')

  # calculate which points have an error based on dt_min
  data[,point_error:=sapply(trajectory_time_interval,
                            check_time_error,
                            min_time_inter=dt_min)]
  print('identify trajectories with wrong points')

  data[point_error==T, good_points:=lapply(trajectory_time_interval,
                                                   get_good_points,
                                                   min_time_inter=dt_min)]
  print('identify the correct points in the trajectories')

  ## Check if the first or last point was removed
  data[point_error==T, first_last_error:= mapply(first_last_removed,
                                                         good_points,
                                                         n)]
  print('identify if the first or last point is removed')
  error <- data[point_error==T,]

  ## correct them
  data[point_error==T, trajectory := mapply(fix_traj, trajectory, good_points)]
  print('trajectory corrected')

  # # recompute metrics movement and airspeed
  # # is this still necessary?
  # VAR=c('n','duration', 'length', 'groundspeed', 'displacement', 'direction','dot') # NOTE maybe timestamp_start and timestamp_end can be added as well
  # data_error <- movement(data[point_error==T,],VAR=colnames(data)[colnames(data) %in% VAR], crs=crs)
  #
  # # rbind data
  # data <- rbind(data[point_error==F,],data_error)
  # # remove columns that are used for calculations but should not be in output
  # data[, c(first_last_error,good_points):=NULL] # NOTE remove also point error?

  ## Fix trajectory timestamps and time interavl
  data[point_error==T, trajectory_time := mapply(fix_vec, trajectory_time, good_points)]
  print('trajectory time corrected')
  data[point_error==T, trajectory_time_interval := mapply(fix_vec, trajectory_time_interval, good_points)]
  print('trajectory time interval corrected')
  data[point_error==T & first_last_error==T, timestamp_end := mapply(fix_time_end, timestamp_start, trajectory_time)]
  print('timestamp_end corrected')
  data[point_error==T & first_last_error==T, duration := mapply(fix_duration, timestamp_start, timestamp_end)]
  print('duration corrected')
  data[point_error==T & first_last_error==T, displacement := mapply(fix_str_distance, trajectory, good_points)]
  print('displacement corrected')
  data[point_error==T & first_last_error==T, direction := mapply(fix_direction, trajectory, good_points)]
  print('direction corrected')

  ## Lastly correct some properties that will be changed regardless
  data[point_error==T, n := lengths(good_points)]
  print("n corrected")
  data[point_error==T, length := as.numeric(st_length(st_transform(trajectory, crs=loc_epsg)))]
  print("length corrected")
  data[point_error==T, groundspeed := length/duration]
  print("groundspeed corrected")
  data[point_error==T, airspeed := mapply(get_airspeed, groundspeed, direction, U10m, V10m)]
  print("airspeed corrected")

  # # remove columns that are used for calculations but should not be in output
  #data[, c(first_last_error,good_points):=NULL] # NOTE remove also point error?
  return(data) # NOTE: check if this really returns the error tracks and that the other track dataset is updated.
  }



