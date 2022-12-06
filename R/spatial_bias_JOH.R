#' @title rasterize_basic
#'
#' @description count number of points per grid cell
#'
#' @param data spatvector object (points)
#' @param resolution resolution
#' @param fun summary function
#' @param e minimum x value of extent in lonlat
#'
#' @return a raster with counts per grid cell
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
rasterize_basic <- function(data=NULL, resolution=100, crs=loc_proj, fun='length',e=NULL){

  # make sure it is a spatraster
  if(class(data)[1] != 'SpatVector'){
    data <- vect(data)
  }

  input_crs <- terra::crs(data)

  if(is.null(e)){
    # CREATE A RASTER WITH EXTENT OF THE DATASET
    r <- terra::rast(terra::ext(data),crs=input_crs)
  } else {
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
      if(fun=='length'){
        p <- terra::extract(r,data,xy=TRUE) %>%
          dplyr::select(c(lyr.1,x,y)) %>%
          plyr::count() %>%
          left_join(x=r_df,by= c('lyr.1','x','y'))
      }
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
#' @param e minimum x value of extent
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
rasterizer <- function(data=NULL, resolution=5000,var=NULL, fun='length', e=NULL){
  if(is.null(var)){
    R <- rasterize_basic(data=data,resolution=resolution,fun=fun,e=e)
  } else {
    VALS <- unique(as.vector(unlist(values(data[,var]))))
    R <- lapply(VALS, function(x) {
      A <- subset(data, as.vector(unlist(values(data[,var]))) == x)
      print(paste0('nrow: ',nrow(A)))
      R <- rasterize_basic(data=A,resolution=resolution,fun=fun,e=e)
      print(paste0('sum: ',sum(values(R),na.rm=TRUE)))
      print(paste0('variable:', x))
      return(R)
    })
    names(R) <- VALS
  }
  set.names(R,"tracks")

  return(R)
}



