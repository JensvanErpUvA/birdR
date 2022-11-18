#' @importFrom sf
#'
#' inclusion_ring_calc
#' @name inclusion_ring
#' @description Return the inclusion ring defined for the radar location and a minimum and maximum distance
#'
#' @param location coordinate, vector of radar coordinates, or spatial point. Input coordinates in WGS84.
#' @param distance vector of minimum and maximum distance in meters to include
#' @param crs metric crs to perform calculations in
#'
#' @return a polygon with inclusion donut
#'
#' @details
#'
#' @export
#'
#'
inclusion_ring <- function(location,  # CALCULATION IN LOCAL CRS
                           distance,
                           crs){
  area_of_inclusion <- sf::st_difference(sf::st_buffer(sf::st_transform(location, crs=crs), dist=distance[1]),
                                         sf::st_buffer(sf::st_transform(location, crs=crs), dist=distance[2]))
  return(area_of_inclusion)
}

#' exclusion_angle
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
#'
exclusion_angle <- function(location=radar_loc,
                            angles=b_corners,
                            distmax=mad,
                            crs=loc_epsg){ # CALCULATION IN LOCAL CRS
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

  return(blocked_triangle)
}

#' exclusion_location
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
#'
exclusion_location <- function(x=area_of_inclusion,
                               y=turbines,
                               buffer=turb_exclusion_rad,
                               crs=loc_epsg){ # CALCULATION IN LOCAL CRS

  ## Add a buffer around each turbine
  y_buff <- sf::st_buffer(st_transform(st_sfc(st_multipoint(st_coordinates(y[,geometry])),crs=wgs84_epsg), crs=crs),
                          dist=buffer)

  ## Substract the turbine areas
  x <- sf::st_difference(x, y_buff)

  ## Transform the AoI  to lon/lat coordinates
  x <- sf::st_transform(x, crs=wgs84_epsg)

  return(x)
}

#' roi
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
#'
roi <- function(location=c(r_lon, r_lat),
                inclusion_ring=c(mad,mid),
                exclusion_angle=b_corners,
                exclusion_location=turbines,
                exclusion_location_buffer=turb_exclusion_rad,
                crs_loc){

  #### radar location ####
  if(class(location)[1] == 'numeric'){ # create radar location
  radar_loc <- st_sfc(st_point(location),crs=4326)
  }

  #### inclusion ring ####
  if(!is.null(inclusion_ring)){
    area_of_inclusion <- inclusion_ring_calc(location=radar_loc,
                                              inclusion_ring=inclusion_ring,
                                              crs=crs_loc)
  }

  # EXCLUSION ANGLES
  ## if list rbind to data frame
  if(is.list(exclusion_angle)){
    exclusion_angle <- do.call('rbind.data.frame',exclusion_angle)
  }
  if(is.vector(exclusion_angle)){
    exclusion_angle <- data.frame(exclusion_angle[1],exclusion_angle[2])
  }

  for(i in 1:nrow(exclusion_angle)){
  blocked_triangle <- exclusion_angle_calc(exclusion_angle[i,],distmax=max(inclusion_ring))
  ## Substractthe triangle from the donut
  area_of_inclusion <- st_difference(area_of_inclusion, blocked_triangle)
  }

  if(!is.null(exclusion_location)){
    area_of_inclusion <- exclusion_location_calc(x=area_of_inclusion,
                                                 y=turbines,
                                                 buffer=exclusion_location_buffer)
    names(area_of_inclusion) <- c('spatial_filter','exclusion_locations')

  } else {
    area_of_inclusion <- list(area_of_inclusion)
    names(area_of_inclusion) <- 'spatial_filter'
    }

  return(area_of_inclusion)
}
