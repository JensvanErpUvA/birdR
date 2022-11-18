#############################################
#' annotation
#' @name get_dot_percs
#' @description visualize percentiles
#'
#' @param displacement sf object with tracks
#' @param duration path/file/API ERA5 dataset with annotation variables,
#' @param probs vector with ERA5 variables to be annotated
#' @param plot boolean (TRUE/FALSE) to plot the
#'
#' @return returns dot perc vals
#'
#' @details
#'
#' @export
#'
#' @examples
#'
#'
get_dot_percs <- function(displacement,
                          duration,
                          probs=seq(0.002,0.02,0.002),
                          plot=T
                          ){
  dot <- displacement/duration
  dot_perc_vals <- quantile(dot,probs=probs)
  if(plot){
    # clutter track implementation
    clutter_track_l <- lapply(1:length(dot_perc_vals), function(x) track_step1[dot <= x[i] ] )
    clutter_track_l <- lapply(length(clutter_track_l):2, function(x) clutter_track_l[[i]][!(id %in% clutter_track_l[[i-1]][,id])])

    lapply(1:length(clutter_track_l), function(x){
      if (nrow(x)>5000){x <- sample_n(x,5000)}
      ggplot()+
        geom_sf(data=st_zm(x[,trajectory]))+
        geom_sf(data=turbines[near_aoi==TRUE,geometry], lwd = 2, colour="red")+
        labs(y = "Latitude", x = "Longitude",
             title = "1st percentile of tracks")+
        theme_minimal()
    })

  }
  return(dot_perc_vals)
}

