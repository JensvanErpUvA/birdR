#' @importFrom data.table fread
#' @importFrom lubridate floor_date
#' @importFrom mgcv gam predict.gam
#'
#' @title nob_prep
#' @name nob_prep
#' @description annotate the trajectory with environmental data
#'
#' @param x track data with minimum the geometry, timestamp_start and timestamp_end
#' @param m masking intensity at original temporal resolution
#' @param unit character varying giving the aggregation unit, default hours. See floor_date/round_date for all the possible units
#' @param period default NULL, optional start and end time, by default the time period is based on start and end date of the track data
#'
#' @return returns the tracks with era5 annotation
#'
#' @details
#'
#'
#' @examples
#'
#'
nob_prep <- function(x, m, unit='hour', period=NULL){

  # NOTE because a period needs to be set. Not sure if this start and end date is needed cause it can be derived from the track table?
  if(!is.null(period)){
  data_hours <- seq(period[1], period[2], by=unit)
  } else {
  data_hours <- seq(floor_date(min(x$timestamp_start), unit),
                    floor_date(max(x$timestamp_start), unit), by=unit)
  }

  ## create an hourly masking average
  NOB <- m[,mean(landmask),by=round_date(timestamp,unit=unit)]
  NOB <- setnames(NOB,c("V1","round_date"),c("landmask","timestamp"))

  ## If we used a temporal subset of the data (for exploration), remove other hours
  NOB <- NOB[timestamp %in% data_hours,]

  ## Use hours in which no filter data is available as tag for offline hours
  offline_hours <- data_hours[which(!(data_hours %in% NOB$timestamp))]

  ## By calculating the rounded hour of the first, middle, and end timestamp of each track,
  ## we are nearly certain to cover all hours a track occurs (a true track nearly never last over 3 hours)
  ## Note: this DOESN'T work if we let the user set the temporal interval!

  x[,round_hour:=round_date(timestamp_start+0.5*duration,unit=unit)]
  x[,round_hour_start:=round_date(timestamp_start,unit=unit)]
  x[,round_hour_end:=round_date(timestamp_end,unit=unit)]

  ## Create an hourly dataset by looping over the hourly data, and counting
  ## the number of tracks occurring per hour. Also include information on whether the radar was functioning
  ## Lastly add the relevant variable affecting negative bias

  ## Initiate the hourly dataset
  aggr_stats <- data.table(timestamp = ymd_hms(),
                             trackcount = integer(),
                             landmask = double(),
                             offline = logical())

## Loop over the hours in this period (note, ALL hours, we also want to loop over moments no tracks are recorded)
for (i in 1:length(data_hours)){

  cur_hour <- data_hours[i]

  ## Check if radar was offline during this hour
  if (cur_hour %in% offline_hours) {
    off <- TRUE
  } else {
    off <- FALSE
  }

  ## Extract the tracks occuring in this hour from the track database
  n_tracks <- nrow(x[round_hour == cur_hour |
                     round_hour_start == cur_hour |
                     round_hour_end == cur_hour,])

  ## Retrieve masking data
  if (cur_hour %in% NOB$timestamp) {
    landmask <- NOB[timestamp == cur_hour,landmask]
  } else {
    landmask <- NA
  }
  ## Now bind all the data as a new row to the aggr_stats
  aggr_stats <- rbind(aggr_stats, list(cur_hour,n_tracks,landmask,off))
}

 aggr_stats$unit <- unit

return(aggr_stats)
}



#' @title nob_gam
#' @name nob_gam
#' @description negative observation bias based on mask
#'
#' @param aggr_stats track data with minimum the geometry, timestamp_start and timestamp_end
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
nob_gam <- function(aggr_stats){

  ## To model the relation between the bias variable and the hourly track count we doe a GAM analysis.
  ## Remove the hours the radar was offline
  gam_stats <- aggr_stats[offline==FALSE,]

  ## Set K low (default=9), we want to identify the larger pattern
  gam_tempbias <- mgcv::gam(trackcount ~ s(landmask, k=5),
                            data=gam_stats)

  ## Visualize the modelled relation on top of the data
  ## Create response data based on the model
  dummy_mask <- seq(min(gam_stats[,landmask]),max(gam_stats[,landmask]),length.out = 100)
  response_mask <- as.data.table(mgcv::predict.gam(gam_tempbias,
                                                   data.frame(landmask = dummy_mask),
                                                   type = "response",
                                                   se.fit = T))

  ## If no clear cut-off is distinguishable, it often helps to visualize the first and
  ## second derivative of the response to vind specific turningpoints in the response (onset of decline, max decline)
  d1_response <- c(NA,response_mask$fit[2:100]-response_mask$fit[1:99])
  d2_response <- c(NA,d1_response[2:100]-d1_response[1:99])

  # ADD SELECTION TO PLOT
  sel <- which(d1_response[3:100]==min(d1_response[3:100]))+1 # note why + 1?
  bias_threshold <- dummy_mask[sel]
  dum <- min(sel)

  g <- ggplot()+
    geom_point(data=aggr_stats[offline==FALSE,], aes(x=landmask,y=trackcount),alpha = 1)+
    geom_line(aes(dummy_mask, response_mask$fit), colour="blue", lwd=1)+
    geom_line(aes(dummy_mask, (d1_response*5)), colour="red",lwd=1)+
    geom_line(aes(dummy_mask, (d2_response*50)), colour="purple",lwd=1)+
    geom_point(aes(dum,bias_threshold), colour='red') +
    xlim(c(0,0.6))+
    theme_minimal()+
    labs(y = paste0(" Number of birds per ", gsub('s','',unique(aggr_stats$unit))),
         x = paste0(" Average 'landmask' value (0-1) per ",gsub('s','',unique(aggr_stats$unit)))) +
    theme(axis.title = element_text(size = 14,face = "bold"),
          axis.text = element_text(size = 10,face = "bold"),
          axis.text.y.right = element_text(colour="red"),
          axis.ticks.y.right = element_line(colour="red"),
          axis.title.y.right = element_text(colour="red"))
  print(plotly::ggplotly(g))

  aggr_stats$bias_threshold <- bias_threshold

  return(aggr_stats)
}


#' @title nob
#' @name nob
#' @description annotate the trajectory with environmental data
#'
#' @param x track data with minimum the geometry, timestamp_start and timestamp_end
#' @param m masking intensity at original temporal resolution
#' @param unit character varying giving the aggregation unit, default hours. See floor_date/round_date for all the possible units
#' @param period default NULL, optional start and end time, by default the time period is based on start and end date of the track data
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
#'
nob <- function(x, m, unit='hour', period=NULL){
# prepare NOB dataset at specific unit interval
NOB <- nob_prep(x=x, m=m, unit=unit, period=period)
# run gam and calculate where the prediction intersects the x-axis
NOB <- nob_gam(NOB)
# specify the hours where the masking threshold is reached
threshold_hours <- NOB[landmask > unique(NOB$bias_threshold), timestamp]

# NOTE: tag the threshold (not sure if you would want to remove them immediately)
x[,masked:= (round_hour %in% threshold_hours &
             round_hour_start %in% threshold_hours &
             round_hour_end %in% threshold_hours)]

return(x)
}



