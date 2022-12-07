#' @title get_dot_percs
#' @name get_dot_percs
#' @description visualize percentiles
#'
#' @param x sf object with tracks
#' @param probs vector of probability bins
#' @param context boolean (TRUE/FALSE) to plot the tracks of each probability bin
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
get_dot_percs <- function(x,
                          probs=seq(0.002,0.02,0.002),
                          context=NULL
                          ){

  #dot <- displacement/duration
  # TODO check if we need to add the possibility to calculate dot in here from displacement and duration. Instead of providing a full table x calculate duration and displacement from time and trajectory
  dot_perc_vals <- quantile(x$dot,probs=probs)

  if(!is.null(context)){
    # clutter track implementation
    clutter_track_l <- lapply(1:length(dot_perc_vals), function(i) {
      if((i - 1) == 0){x[dot >= 0 & dot <= dot_perc_vals[i]]}
      x[dot >= dot_perc_vals[i-1] & dot <= dot_perc_vals[i]]
    })
    names(clutter_track_l) <- names(dot_perc_vals)
    lapply(1:length(clutter_track_l), function(x){
      if (nrow(clutter_track_l[[x]])>5000){clutter_track_l[[x]] <- sample_n(clutter_track_l[[x]],5000)}
      g <- ggplot(area_of_inclusion,lwd = 2)+
        geom_sf(aes(color='aoi'))+
        geom_sf(data=st_zm(clutter_track_l[[x]][,trajectory]))+
        labs(y = "Latitude", x = "Longitude",color='',
             title = paste0(names(clutter_track_l)[x], " percentile of tracks"))+
        theme_minimal()
      print(g)
    })

  }
  return(dot_perc_vals)
}

