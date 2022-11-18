
#2.2 Improving the quality of bird tracks
Erroneous  points exist within some tracks, identifiable by a very short time-interval between those points and the next/previous point. Identify these intervals and remove both points involved with that interval, as there is no way to tell which of these two is faulty

Again here maybe split function between identifying a bad trajectory and fixing it?
  Could also be merged: skip returning the true/false and immediatly fix
#############################################
FUNCTION
Input:
  - track time per point, as seconds passed since the first point
- time interval threshold
Output:
  - TRUE/FALSE of erroneous points
Example: rr_check_error <- function(time_inter, min_time_inter){<code> return(is_erroneous)}
```{r check_trajectories}
## First get the time interval between consequetive points
## Note the time interval between point 1 and point 2 is stored at the second location in the vector (1st is NA)
get_time_inter <- function(trajectory_time){
  n_plots <- length(trajectory_time)
  time_inter <- NA
  time_inter[2:n_plots] <- trajectory_time[2:n_plots]-trajectory_time[1:(n_plots-1)]
  return(time_inter)
}
tracks_step2[,time_inter:=lapply(trajectory_time, get_time_inter)]

## Now check which tracks have an error
check_time_error <- function(time_inter, min_time_inter){
  return(length(which(unlist(time_inter)<=min_time_inter))>0)
}

tracks_step2[,point_error:=sapply(time_inter,
                                  check_time_error,
                                  min_time_inter=min_time_inter)]
```

FUNCTION END
#############################################

Now fix the trajectory, as discussed this can be incorporated with the previous function into one function. There are also a lot of posibilities for what to fix: only the trajectoy or all track parameters?
  #############################################
FUNCTION
Input:
  - trajectory
- track time intervals
Output:
  - good trajectory
Example: rr_fix_error <- function(time_inter, min_time_inter){<code> return(is_erroneous)}
```{r fix_trajectories}
## Initialize and run function to get good plots in track
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
tracks_step2[point_error==T, good_points:=lapply(time_inter,
                                                 get_good_points,
                                                 min_time_inter=min_time_inter)]

## Check if the first or last point was removed
first_last_removed <- function(good_points, nr_of_points){
  return(!(1 %in% good_points & nr_of_points %in% good_points))
}

tracks_step2[point_error==T, first_last_error:= mapply(first_last_removed,
                                                       good_points,
                                                       nr_of_points)]

## Make a function to correct the trajectory
fix_traj <- function(trajectory, good_points){
  new_traj <- st_sfc(st_linestring(st_coordinates(trajectory)[good_points,c("X","Y","Z","M")]), crs=wgs84_epsg)
  return(new_traj)
}
## Correct them
tracks_step2[point_error==T, trajectory := mapply(fix_traj, trajectory, good_points)]
```
FUNCTION END
#############################################
