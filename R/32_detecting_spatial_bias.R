#'@importFrom terra crs rast ext vect project res as.data.frame mask distance
#'@importFrom plotly ggplotly

#' @title rasterize_basic
#'
#' @description count number of points per grid cell
#'
#' @param data spatvector object (points)
#' @param resolution resolution
#' @param fun summary function
#' @param crs coordinate reference system
#' @param e minimum x value of extent in lonlat
#' @param aoi area of inclusion
#'
#' @return a raster with counts per grid cell
#'
#'
#' @examples
#'
#'
#' \dontrun{
#' r <- grid_count(data, resolution=500)
#' }
#'
rasterize_basic <- function(data=NULL, resolution=100, crs=NULL, fun='weighted',e=NULL){

  # make sure it is a spatraster
  if(class(data)[1] != 'SpatVector'){
    data <- vect(data)
  }

  input_crs <- terra::crs(data)

  if(is.null(e)){
    # CREATE A RASTER WITH EXTENT OF THE DATASET
    r <- terra::rast(terra::ext(data),crs=input_crs)
  }
  if(class(e)[1] == 'SpatExtent'){
    r <- rast(e,crs=input_crs)
  }
  if(class(e) == 'numeric'){
    r <- rast(crs=input_crs)
    terra::ext(r) <- e
  }

  # convert to local projection to measure resolution in meters
  if(!is.null(crs)){
  r <- terra::project(r,crs)
  terra::res(r) <- resolution
  # reproject back to original projection
  r <- terra::project(r,input_crs)
  }


  if(geomtype(data) == 'points'){
    R <- rasterize(data,r,fun=fun)
  } else {
    if(geomtype(data) == 'lines'){
      numcells <- 1:(dim(r)[1] * dim(r)[2])
      values(r) <- numcells

      # CREATE DATAFRAME WITH COORDINATED OF RASTER
      r_df <- terra::as.data.frame(r,xy=TRUE)

      # COUNT PER CELL HOW MANY TRACKS INTERSECT
      # ALL TRACKS
      # if(fun=='length'){
      #   p <- terra::extract(r,data,xy=TRUE) %>%
      #     dplyr::select(c(lyr.1,x,y)) %>%
      #     plyr::count() %>%
      #     left_join(x=r_df,by= c('lyr.1','x','y'))
      # }
        p <- as.data.table(extract(r, data, xy=TRUE))
        if(fun=='weighted'){
          p[,cell_num:=1/length(lyr.1), by=ID]
          p[,freq:=sum(cell_num), by=lyr.1]
        }
        if(fun=='absolute'){
          p[,cell_num:=1, by=ID]
          p[,freq:=sum(cell_num), by=lyr.1]
        }
        p[,c("ID","cell_num"):=NULL]
        p <- unique(p, by="lyr.1") %>%
          left_join(x=r_df, by=c('lyr.1','x','y'))

      p[which(is.na(p$freq)),'freq'] <- 0
      R <- r
      values(R) <- p$freq
      NAflag(R) <- 0
    }
  }

  return(R)
}

#' @title rasterizer
#'
#' @description count number of points per grid cell
#'
#' @param data spatvector object (points)
#' @param resolution resolution
#' @param var variable to loop over
#' @param crs coordinate reference system
#' @param e minimum x value of extent
#' @param aoi area of inclusion
#'
#' @return a raster or list of rasters with counts per grid cell
#'
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' r <- grid_count(data, resolution=500)
#' }
#'
rasterizer <- function(data=NULL, resolution=5000,var=NULL, crs=NULL, fun='weighted', e=NULL,aoi=NULL){
  if(is.null(var)){
    R <- rasterize_basic(data=data,resolution=resolution,crs=crs,fun=fun,e=e)
  } else {
    VALS <- unique(as.vector(unlist(values(data[,var]))))
    R <- lapply(VALS, function(x) {
      A <- subset(data, as.vector(unlist(values(data[,var]))) == x)
      print(paste0('nrow: ',nrow(A)))
      R <- rasterize_basic(data=A,resolution=resolution,fun=fun,e=e,crs=crs)
      print(paste0('sum: ',sum(values(R),na.rm=TRUE)))
      print(paste0('variable:', x))
      return(R)
    })
    names(R) <- VALS
  }
  set.names(R,"tracks")

  print(rasterize_plot(R,aoi=aoi))

  return(R)
}




#' @title rasterize_plot
#'
#' @description plot raster
#'
#' @param x spatraster object
#' @param aoi area of inclusion
#'
#' @return a plot of the raster and the area of inclusion
#'
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#' }
#'
rasterize_plot <- function(x, aoi=NULL){
  # ## First visual inspection. Convert the data to a data.frame for plotting. Use the spatial mask set in step 2.1.1 to remove "dead" cells
  if(!is.null(aoi)){
    fig_rast_data <- terra::as.data.frame(terra::mask(x,vect(aoi)),xy=TRUE)
    setnames(fig_rast_data, c("x","y"),c("lon","lat"))
  } else {
    fig_rast_data <- terra::as.data.frame(x,xy=TRUE)
    setnames(fig_rast_data, c("x","y"),c("lon","lat"))
  }

  # basic plot
  g <- ggplot() +
    geom_tile(data=fig_rast_data, aes(x=lon,y=lat,fill=tracks)) +
    scale_fill_gradient(low="grey", high="darkblue", name="Number\n of tracks") +
    labs(x="Longitude", y="Latitude", title=area_name) +
    theme_minimal() +
    theme(title = element_text(size=15, face="bold"),
          axis.title = element_text(size=15, face="bold"),
          axis.text = element_text(size=12, face="bold"))

  # exclusion geometry exists
  if(!is.null(aoi)){
    g <- g + geom_sf(data=aoi, fill=NA, colour="black", lwd = 0.3)

    if(!is.null(attr(aoi,'exclusion_geom'))){
      g <- g + geom_sf(data=st_intersection(st_as_sfc(st_bbox(aoi)),attr(aoi,'exclusion_geom')$geometry), colour="black", lwd = 0.1)
    }
    # location geometry exists
    if(!is.null(attr(aoi,'location'))){
      g <- g + geom_sf(data=st_as_sf( data.frame(geom=attr(aoi,'location')),wkt='geom',crs=attr(aoi,'location_crs')), colour="darkred", shape=16, lwd = 0.7)
    }
  }
  return(g)
}

#' @title rastgam
#'
#' @description count number of points per grid cell
#'
#' @param x spatraster generated by track_density_rast
#' @param radar radar location
#' @param distance default mid mad
#'
#' @return a raster with counts per grid cell
#'
#'
#' @examples
#'
#'
#' \dontrun{
#'
#' }
#'
rastgam <- function(x,
                    radar,
                    distance=c(mid,mad)){
  ## Create a layer of distance from the cell to the radar
  distance_raster_name <- 'distance'
  dist_rast <- terra::distance(x$tracks,vect(radar))
  set.names(dist_rast,distance_raster_name)

  if(length(names(x)[names(x) %in% distance_raster_name]) < 1 ){
    add(x) <- dist_rast
  }

  rast_data <- as.data.table(terra::as.data.frame(x,xy=TRUE))
  setnames(rast_data, c("x","y"), c("lon","lat"))

  gam_stats <- rast_data[distance>=min(distance) & distance<=max(distance),]
  gam_distbias <- gam(tracks ~ s(distance),
                      data=gam_stats)

  dummy_dist <- seq(min(distance),max(distance),length.out = 1000)
  response_dist <- as.data.table(predict.gam(gam_distbias,
                                             data.frame(distance = dummy_dist),
                                             type = "response",
                                             se.fit = T))
  ## TODO get the confidence interval of the gam.prediction

  g <- ggplot() +
    geom_point(data = gam_stats, aes(x=distance,y=tracks))+
    xlim(min(distance),max(distance)) +
    geom_ribbon(aes(x=dummy_dist,ymin=(response_dist$fit-10*response_dist$se.fit),
                    ymax=(response_dist$fit+10*response_dist$se.fit)), colour = NA, fill = "purple", alpha = 0.4)+
    geom_line(aes(x=dummy_dist,y=response_dist$fit), colour = "purple", lwd=1)+
    theme_minimal()+
    theme(legend.text = element_text(size=15, face="bold"),
          legend.title = element_text(size=15, face="bold"),
          axis.title = element_text(size=20, face="bold"),
          axis.text = element_text(size=15, face="bold"))

  res <- list(gam_distbias, rast_data, g)
  names(res) <- c('GAM','DATA','PLOT')
  return(res)
}


#' @title crosses_aoi
#'
#' @description count number of points per grid cell
#'
#' @param trajectory trajectory geometry
#' @param raster raster
#'
#' @return a raster with counts per grid cell
#'
#'
#' @examples
#'
#'
#' \dontrun{
#' r <- rastgam(data, resolution=500)
#' }
#'
crosses_aoi <- function(trajectory, raster) {
  ## convert track sf's to SpatVector
  traj_vect <- vect(st_zm(trajectory))

  ## Get the cells with which the trajectory overlaps
  overlap <- extract(raster, traj_vect, xy=TRUE)

  return(any(overlap$inclusion_cell==1))
}


#' @title distb
#'
#' @description correction for distance bias based on track raster
#'
#' @param x track dataset
#' @param track_raster spatraster generated by track_density_rast
#' @param radar radar location
#' @param distance default mid mad
#' @param aoi area of interest
#'
#' @return a raster with counts per grid cell
#'
#'
#' @examples
#'
#'
#' \dontrun{
#' x <- tracks[sample(1:nrow(tracks),10000),]
#' y <- track_density_rast(tracks$trajectory,resolution=100)
#' radar <- sf::st_sfc(sf::st_point(c(4.185345,  52.427827)),crs=4326)
#' distance <- c(1000,2500)
#' distb(x,y, radar, distance,aoi=NULL)
#'
#' }
#'
distb <- function(x,y,aoi=NULL,radar=NULL,distance=NULL){
  track_raster <- y
  crs_input <-terra::crs(track_raster)

  if(!is.null(aoi)){
    if(is.null(radar)){
      radar <- st_as_sf( data.frame(geom=attr(aoi,'location')),wkt='geom',crs=attr(aoi,'location_crs'))
    }
    if(is.null(distance)){
      distance <- sort(attr(aoi,'distance'))
    }
  }

  rastgam_output <- rastgam(x=track_raster,radar=radar, distance=distance)
  gam_distbias <- rastgam_output[[1]]
  rast_data <- rastgam_output[[2]]
  plot <- rastgam_output[[3]]

  rast_data[distance>=min(distance) & distance<=max(distance) & !is.na(tracks) , #
            threshold_tracks:=predict.gam(gam_distbias,distance=rast_data$distance,type="response")]
  rast_data[,inclusion_cell:=tracks>=threshold_tracks]
  rast_data[is.na(threshold_tracks),inclusion_cell:=T]

  # fig_rast_data <- as.data.table(as.data.frame(mask(rast(rast_data, type="xyz", crs=crs_input),vect(area_of_inclusion)),xy=TRUE))
  # setnames(fig_rast_data, c("x","y"), c("lon","lat"))

  r <- rast(rast_data, type="xyz", crs=crs_input)

  r[r$inclusion_cell==0] <- NA
  if(is.null(aoi)){
    if(!is.null(distance)){
      r[r$distance < min(distance)] <- NA
      r[r$distance > max(distance)] <- NA
    }
  }

  g <- rasterize_plot(r,aoi=aoi)

  #plotly::ggplotly(g)
  print(g)

  setnames(rast_data, c("lon","lat"), c("x","y"))
  track_raster <- rast(rast_data, type="xyz", crs=crs_input)

  x[,crosses_raster:=sapply(trajectory,crosses_aoi,raster=track_raster)]

  return(plot)
}






#' # NOTE older version
#' #' @title track_density_rast
#' #'
#' #' @description count number of points or trajectories per grid cell
#' #'
#' #' @param data spatvector object (points)
#' #' @param resolution resolution
#' #' @param crs summary function
#' #' @param e minimum x value of extent in lonlat
#' #' @param aoi aoi
#' #'
#' #' @return a raster with counts per grid cell
#' #'
#' #'
#' #' @examples
#' #'
#' #'
#' #' \dontrun{
#' #' r <- track_density_rast(data, resolution=500)
#' #' }
#' #'
#' track_density_rast <- function(data=trajectories,crs=loc_proj, e=NULL,resolution=100,aoi=NULL) {
#'
#'   # make sure it is a spatraster
#'   if(class(data)[1] != 'SpatVector'){
#'     data <- vect(data)
#'   }
#'
#'   input_crs <- terra::crs(data)
#'
#'   if(is.null(e)){
#'     # CREATE A RASTER WITH EXTENT OF THE DATASET
#'     r <- terra::rast(terra::ext(data),crs=input_crs)
#'   }
#'   if(class(e)[1] == 'SpatExtent'){
#'     r <- rast(e,crs=input_crs)
#'   }
#'   if(class(e) == 'numeric'){
#'     r <- rast(crs=input_crs)
#'     terra::ext(r) <- e
#'   }
#'
#'   # convert to local projection to measure resolution in meters
#'   if(!is.null(crs)){
#'     r <- terra::project(r,crs)
#'     terra::res(r) <- resolution
#'     # reproject back to original projection
#'     r <- terra::project(r,input_crs)
#'   }
#'
#'   ## fill in number of raster cells
#'   numcells <- 1:(dim(r)[1] * dim(r)[2])
#'   values(r) <- numcells
#'
#'   ## create dataframe with coordinates of extent raster
#'   r_df <- terra::as.data.frame(r, xy=TRUE)
#'
#'   ## count the number of tracks intersecting with each cell
#'   r_count <- as.data.table(extract(r, data, xy=TRUE))
#'   r_count[,cell_num:=1/length(lyr.1), by=ID]
#'   r_count[,freq:=sum(cell_num), by=lyr.1]
#'   r_count[,c("ID","cell_num"):=NULL]
#'   r_count <- unique(r_count, by="lyr.1") %>%
#'     left_join(x=r_df, by="lyr.1")
#'
#'   ## if there are zero tracks -> make cell 0
#'   r_count[which(is.na(r_count$freq)),"freq"] <- 0
#'   values(r) <- r_count$freq
#'   NAflag(r) <- 0
#'   set.names(r,"tracks")
#'
#'   # plot data
#'   print(rasterize_plot(r,aoi=aoi))
#'
#'   #project(r, wgs84_proj)
#'   ## Return the raster
#'   return(r)
#' }


