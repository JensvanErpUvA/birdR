#' @importFrom sf st_buffer st_sfc st_point st_transform st_polygon st_difference st_union st_crs st_coordinates
#' @importFrom ggplot2 geom_sf labs ggtitle theme_minimal
#'
#'
#' @title create_location
#' @name create_location
#' @description calculate a point geometry
#'
#' @param location coordinate, vector of radar coordinates, or spatial point. Input coordinates in WGS84.
#' @param crs metric crs to perform calculations in
#'
#' @return a polygon with inclusion ring in coordinate reference system of the input
#'
#' @details
#'
#'
create_location <- function(location,crs, verbose=T){
if(class(location)[1] == 'numeric'){ # create radar location
  location <- sf::st_sfc(sf::st_point(location),crs=4326)
  if(verbose){
  message(paste0('coordinates are converted to sfc_POINT EPSG:4326.\nwarning: If input coordinates are not 4326, please input a geometry with the correct CRS as location-argument. \ne.g. st_sfc(st_point(location),crs=',loc_epsg,')'))
  }
  }

#### check whether a location is EPSG 4326 and convert to local to make calculations in a metric system ####
input_crs <- st_crs(location)$input
if(!grepl(crs,input_crs)){ # input_crs == 'EPSG:4326'
  location <- sf::st_transform(location, crs=crs)
  if(verbose){
  message(paste0('coordinates are reprojected to local projection system for further calculations: EPSG:' , crs))
  }
}
return(list(location,input_crs))
}

#' @title inclusion_ring
#' @name inclusion_ring
#' @description Return the inclusion ring defined for the radar location and a minimum and maximum distance
#'
#' @param location coordinate, vector of radar coordinates, or spatial point. Input coordinates in WGS84.
#' @param distance vector of minimum and maximum distance in meters to include
#' @param crs metric crs to perform calculations in
#'
#' @return a polygon with inclusion ring in coordinate reference system of the input
#'
#' @details
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' distance <- c(2500,1000) # distance in meters
#' crs <- 23095 # metric CRS in which calculations are performed
#'
#' # location as vector
#'
#' location <- c(4.185345, 52.42783)
#' ring <- inclusion_ring(location, distance,crs)
#' ggplot(ring) + geom_sf() + geom_sf(data=st_sfc(st_point(location),crs=4326))
#'
#' # location as geometry
#'
#' # EPSG:4326
#' location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)
#' ring <- inclusion_ring(location, distance, crs)
#' ggplot(ring) + geom_sf() + geom_sf(data=location)
#'
#' # EPSG:23095
#' location <- st_sfc(st_point(c(444696.9, 5809144)),crs=crs)
#' ring <- inclusion_ring(location, distance, crs)
#' ggplot(ring) + geom_sf() + geom_sf(data=location)
#' }
#'
inclusion_ring <- function(location,
                           distance,
                           crs,
                           verbose=F){ # CALCULATION IN LOCAL CRS
  #### radar location ####
  loc <- create_location(location,crs=crs,verbose=verbose)
  location <- loc[[1]]
  input_crs <- loc[[2]]
  input_loc <- st_as_text(st_transform(location,crs=input_crs))

  if(length(distance)==1){
    area_of_inclusion <- sf::st_buffer(location, dist=distance[1])
  }

  if(length(distance)==2){
    area_of_inclusion <- sf::st_difference(sf::st_buffer(location, dist=distance[1]),
                                           sf::st_buffer(location, dist=distance[2]))
  }

  if(input_crs == 'EPSG:4326'){
    area_of_inclusion <- sf::st_transform(area_of_inclusion,4326)
  }
  if(verbose){
    message('inclusion ring created')
  }

  attr(area_of_inclusion, 'location') <- input_loc
  attr(area_of_inclusion, 'location_crs') <- input_crs
  attr(area_of_inclusion, 'distance') <- distance

  return(area_of_inclusion)
}

#' @title exclusion_angle
#' @name exclusion_angle
#' @description Return the inclusion ring defined for the radar location and a minimum and maximum distance
#'
#' @param location coordinate, vector of radar coordinates, or spatial point. Input coordinates in WGS84.
#' @param angles vector of minimum and maximum angles in degrees to exclude
#' @param distmax proj4string to transform to for calculations in a metric system
#' @param crs crs to perform calculations in
#'
#' @return a polygon with inclusion donut
#'
#' @details
#' With the argument exclusion_angle for which we expect structural interference.
#' Here we have two types of interference:
#' 1. The turbine the radar is situated on blocks one side of the radar window
#' 2. The area overlapping with the vertical radar beam, which mostly has mixed
#' tracks we did not include in this study. Therefore this area is not relevant here
#' We remove the blocked corners by creating a triangle using the radar coordinates,
#' the corner angles, and  maximum distance.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' angles <- c(287,30)
#' distmax <- 2500 # distance in meters
#' crs <- 23095 # metric CRS in which calculations are performed
#'
#' # location as vector
#'
#' location <- c(4.185345, 52.42783)
#' blocked_triangle <- exclusion_angle(location, angles, distmax, crs)
#' ggplot(blocked_triangle) + geom_sf() + geom_sf(data=st_sfc(st_point(location),crs=4326))
#'
#' angles <- list(c(287,30),c(115,135))
#' blocked_triangles <- lapply(angles,function(x) exclusion_angle(location,angles=x,distmax,crs))
#' ggplot() +
#' geom_sf(data=blocked_triangles[[1]]) +
#' geom_sf(data=blocked_triangles[[2]]) +
#' geom_sf(data=st_sfc(st_point(location),crs=4326))
#' }
#'
exclusion_angle <- function(location=c(r_lon, r_lat),
                            angles=b_corners[1,],
                            distmax=mad,
                            crs=loc_epsg,
                            verbose=F){ # CALCULATION IN LOCAL CRS
  #### radar location ####
  loc <- create_location(location,crs,verbose=verbose)
  location <- loc[[1]]
  input_crs <- loc[[2]]

  ## Calculate a length of the triangle legs much larger than mad
  triangle_leg <- 5 * distmax

  ## Loop over the blocked corners
  ## Angles between which the radar is blocked by its turbine (in [Rad])
  triangle_angles <- c(angles/180*pi, angles/180*pi)

  ## Get base point of triangle from radar coordinates (local projection)
  triangle_base <- sf::st_transform(location, crs=crs)
  ## Make a triangle using the radar coordinates, gap angles and leg length
  triangle_top <- triangle_base + c(sin(triangle_angles[2])*triangle_leg, cos(triangle_angles[2])*triangle_leg)
  triangle_side <- triangle_base + c(sin(triangle_angles[1])*triangle_leg, cos(triangle_angles[1])*triangle_leg)
  triangle_coordinates <- rbind(c(st_coordinates(triangle_base)[[1]], st_coordinates(triangle_base)[[2]]),
                                c(st_coordinates(triangle_top)[[1]], st_coordinates(triangle_top)[[2]]),
                                c(st_coordinates(triangle_side)[[1]], st_coordinates(triangle_side)[[2]]),
                                c(st_coordinates(triangle_base)[[1]], st_coordinates(triangle_base)[[2]]))

  ## Create a geometry of the triangle corners
  blocked_triangle <-  sf::st_sfc(sf::st_polygon(list(triangle_coordinates)), crs=crs)

  if(input_crs == 'EPSG:4326'){
    blocked_triangle <- sf::st_transform(blocked_triangle,4326)
  }

  if(verbose){
    message('exclusion angles created')
    g  <- ggplot(blocked_triangle) + geom_sf() + geom_sf(data=location,aes(col='red'))
    print(g)
  }

  return(blocked_triangle)
}

#' @title exclusion_location
#' @name exclusion_location
#' @description Return the inclusion ring defined for the radar location and a minimum and maximum distance
#'
#' @param x coordinate, vector of radar coordinates, or spatial point. Input coordinates in WGS84.
#' @param y vector of minimum and maximum angles in degrees to exclude
#' @param buffer proj4string to transform to for calculations in a metric system
#' @param crs crs to perform calculations in
#'
#' @return a polygon with inclusion donut
#'
#' @details
#'
#' @export
#'
#' @examples
#'
#' turbines
#' x<- turbines
#' buffer <- 100 # distance in meter
#' crs <- 23095 # metric CRS in which calculations are performed
#'
#' p <- exclusion_geom(x,buffer,crs,verbose=T)
#'
#' x <- st_buffer(x$geometry,200)
#' p <- exclusion_geom(x,buffer,crs,verbose=T)
#'
exclusion_geom <- function(x=turbines,
                           buffer=NULL,
                           crs=loc_epsg,
                           verbose=F){ # CALCULATION IN LOCAL CRS

  input_crs <- st_crs(x)$input
  ## Add a buffer around each point
  if(grepl('sfc',class(x)[1])){
    input_x <- x
  }
  if(class(x)[1] == 'data.table'){
    input_x <- x$geometry
    x <- input_x
  }

  if(!is.null(buffer)){
    x <- sf::st_buffer(x=st_transform(input_x,crs=crs),dist=buffer)
  }

  if(grepl(crs,input_crs)){
    x <- st_transform(x,input_crs)
  }

  x <- sf::st_union(x) %>% sf::st_combine()

  if(verbose){
    message('(buffered) areas to exclude created')
    g  <- ggplot(x) + geom_sf() + geom_sf(data=input_x,aes(col='geom'))
    print(g)
  }

  return(x)
}

#' @title roi
#' @name roi
#' @description Return the area of interest. Three filters (inclusion_donut, exclusion_angle, exclusion_location) can be specified.
#'
#' @param location coordinate, vector of radar coordinates, or spatial point. Input coordinates in WGS84.
#' @param inclusion_ring vector of minimum and maximum distance in meters to include
#' @param exclusion_angle vector, list or data.frame of angle-pairs to exclude from analysis
#' @param exclusion_location sf-object with locations which can reliably be excluded
#' @param exclusion_location_buffer buffer in unit of crs?
#' @param crs crs to perform calculations in. If different from location coordinate, the location coordinate will reprojected
#'
#' @return a polygon with inclusion zones
#'
#' @details
#' With the argument exclusion_angle for which we expect structural interference.
#' Here we have two types of interference:
#' 1. The turbine the radar is situated on blocks one side of the radar window
#' 2. The area overlapping with the vertical radar beam, which mostly has mixed
#' tracks we did not include in this study. Therefore this area is not relevant here
#' We remove the blocked corners by creating a triangle using the radar coordinates,
#' the corner angles, and  maximum distance.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # inclusion ring
#'
#' # location as vector
#' location <- c(4.185345, 52.42783)
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095)
#'
#' # location as geom
#' # 4326
#' location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095)
#' # 3035
#' location <- st_sfc(st_point(c(3925960, 3273397)),crs=3035)
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095)
#' ## distance
#'
#' # one distance value: no exclusion of area near radar
#' location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)
#' area_of_inclusion <-  roi(location=location,
#'                          distance=2500,
#'                          crs=23095)
#' # two distance values: no exclusion of area near radar
#' location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095)
#'
#' ## exclusion angles
#' location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)
#'
#' # exclusion angles can be a list, vector or matrix
#'
#' # list
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095,
#'                          excl_angles=list(c(287,30),c(115,135)))
#'
#' # vector
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095,
#'                          excl_angles=c(287,30,115,135))
#'
#' # matrix
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095,
#'                          excl_angles=matrix(c(287,30,115,135),nrow=2,ncol=2,byrow=T))
#'
#' # do not exclude areas near the radar
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500),
#'                          crs=23095,
#'                          excl_angles=matrix(c(287,30,115,135),nrow=2,ncol=2,byrow=T))
#'
#' ## exclusion areas
#'
#' # without buffer points will not be excluded
#' location <- st_sfc(st_point(c(4.185345, 52.42783)),crs=4326)
#'
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095,
#'                          excl_angles=c(287,30,115,135),
#'                          excl_geom=turbines)
#'
#' # buffer outside the function
#' polygons <- st_buffer(st_transform(turbines$geometry,23095),100)
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095,
#'                          excl_angles=c(287,30,115,135),
#'                          excl_geom=polygons)
#' # buffer within the function
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095,
#'                          excl_angles=c(287,30,115,135),
#'                          excl_geom=turbines,
#'                          excl_buffer=100
#' )
#'
#' # vary the buffer distance based on distance from radar
#' dist <- st_distance(location,turbines$geometry)
#' weight <- as.vector(dist/max(dist) * 100)
#'
#' area_of_inclusion <-  roi(location=location,
#'                          distance=c(2500,1000),
#'                          crs=23095,
#'                          excl_angles=c(287,30,115,135),
#'                          excl_geom=turbines,
#'                          excl_buffer=weight
#' )
#'
#' # apply a buffer around multiple locations by using lists
#' area_of_inclusion <-  roi(location=location,
#'                          distance=2500,
#'                          crs=23095,
#'                          excl_angles=c(287,30,115,135),
#'                          excl_geom=list(turbines,location),
#'                          excl_buffer=list(100,1000)
#' )
#'
#' # apply a buffer around multiple locations by using lists and varying buffer weight within a single dataset
#' dist <- st_distance(location,turbines$geometry)
#' weight <- as.vector(dist/max(dist) * 100)
#' area_of_inclusion <-  roi(location=location,
#'                          distance=2500,
#'                          crs=23095,
#'                          excl_angles=c(287,30,115,135),
#'                          excl_geom=list(turbines,location),
#'                          excl_buffer=list(weight,1000)
#')
#'}
roi <- function(location=NULL,
                distance=NULL,
                excl_angles=NULL,
                excl_geom=NULL,
                excl_buffer=NULL,
                crs=loc_epsg,
                verbose=T){

  #### inclusion ring ####
  if(!is.null(distance)){
    area_of_inclusion <- inclusion_ring(location=location,
                                        distance=distance,
                                        crs=crs,
                                        verbose=verbose)
    attr_location <- attr(area_of_inclusion, 'location')
    attr_location_crs <- attr(area_of_inclusion, 'location_crs')
    attr_distance <- attr(area_of_inclusion, 'distance')

    g  <- ggplot(area_of_inclusion) + geom_sf() + ggtitle('inclusion ring')
    print(g)
  } else {
    area_of_inclusion <- NULL
  }

  #### exclusion angles ####
  ## if list rbind to data frame
  if(!is.null(excl_angles)){
    if(is.list(excl_angles)){
      excl_angles <- as.matrix(do.call('rbind.data.frame',excl_angles))
      colnames(excl_angles) <- NULL
    }
    if(is.vector(excl_angles)){
      excl_angles <- as.matrix(data.frame(excl_angles[seq(1,length(excl_angles),2)],excl_angles[seq(2,length(excl_angles),2)]))
      colnames(excl_angles) <- NULL
    }

    for(i in 1:nrow(excl_angles)){
    blocked_triangle <- exclusion_angle(location=location,
                                        angles=excl_angles[i,],
                                        distmax=max(distance),
                                        crs=crs,
                                        verbose=verbose)
    ## Substract the triangle from the donut
    area_of_inclusion <- st_difference(area_of_inclusion, blocked_triangle)

    g  <- ggplot(area_of_inclusion) + geom_sf() + ggtitle('exclusion angles')
    print(g)
    }
    attr(area_of_inclusion,'exclusion_angles') <- as.vector(t(excl_angles))
    attr_exclusion_angles <- as.vector(t(excl_angles))
  }

  #### exclusion geometry ####
  if(!is.null(excl_geom)){
    gg <- excl_geom
    bb <- excl_buffer
    if(class(excl_geom)[1]!='list'){
      excl_geom <- list(excl_geom)
    }
    if(class(excl_buffer)[1]!='list'){
      excl_buffer <- list(excl_buffer)
    }

    for(i in 1:length(excl_geom)){
    buffered_geom <- exclusion_geom(x=excl_geom[[i]],
                                    buffer=excl_buffer[[i]],
                                    crs=crs,
                                    verbose=verbose)
    crs_input <- st_crs(area_of_inclusion)$input
    crs_excl <- st_crs(buffered_geom)$input
    if(crs_input != crs_excl){
      buffered_geom <- st_transform(buffered_geom, crs_input)
    }

    ## Substract the turbine areas
    area_of_inclusion <- sf::st_difference(area_of_inclusion, buffered_geom)

    g  <- ggplot(area_of_inclusion) + geom_sf() + ggtitle('exclusion geom')
    print(g)
    }
    attr(area_of_inclusion,'exclusion_geom') <- gg
    attr(area_of_inclusion,'exclusion_buffer') <- bb

  }

  if(!is.null(area_of_inclusion)){
  location <- create_location(location,crs=crs,verbose=F)
  g  <- ggplot(area_of_inclusion) +
    geom_sf() +
    geom_sf(data=location[[1]],aes(col='radar')) +
    ggtitle('area of inclusion') +
    labs(x="Longitude", y="Latitude",colour='') +
    theme_minimal()


  print(g)
  }

  if(!is.null(distance)){
  attr(area_of_inclusion,'location')<-attr_location
  attr(area_of_inclusion,'location_crs')<-attr_location_crs
  attr(area_of_inclusion,'distance')<-attr_distance
  }
  if(!is.null(excl_angles)){
  attr(area_of_inclusion,'exclusion_angles')<-attr_exclusion_angles
  }

  return(area_of_inclusion)
}
