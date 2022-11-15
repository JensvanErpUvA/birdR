#' @importFrom
#'
#' Spatial filter
#' FUNCTION
#'
#' Input:
#' - Radar lat and lon (2 numerics in a 1x2 vector)
#' - Minimum and maximum distance (2numerics in a 1x2 vector, minimum default = 0)
#' - Blocked corners (2 numerics per corner in a ?x2 vector, default)
#' - Local epsg
#' Output:
#'   - sf-feature of area of inclusion (polygon or multi-polygon)
#' Ideally the function should be able to work using only the radar coordinates and the maximum distance
#' Example: rr_get_aoi <- function(r_lat, r_lon, mid=0, mad, b_corners=NA, loc_epsg){<code> return(aoi)}
#'
#' First we set a minimum and maximum distance around the radar
#'
#'
#' @param location coordinate, vector of radar coordinates, or spatial point (WGS84)
#' @param inclusion_donut vector of minimum and maximum distance in meters to include
#' @param exclusion_angle vector, list or data.frame of angle-pairs to exclude from analysis
#' @param crs proj4string of location
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
filter_spatial <- function(location=c(r_lon, r_lat),
                           inclusion_donut=c(mad,mid),
                           exclusion_angle=b_corners,
                           crs=wgs84_epsg, # default
                           crs_loc){ #

  # RADAR LOCATION
  if(class(location)[1] == 'numeric'){ # create radar location
  radar_loc <- st_sfc(st_point(location),crs=crs)
  }

  # INCLUSION DONUT
  if(!is.null(inclusion_donut)){
    area_of_inclusion <- inclusion_donut_calc(location=radar_loc,
                                              inclusion_donut=inclusion_donut,
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
  blocked_triangle <- exclusion_angle_calc(exclusion_angle,distmax=max(inclusion_donut))
  ## Substractthe triangle from the donut
  area_of_inclusion <- st_difference(area_of_inclusion, blocked_triangle)
  }

  return(area_of_inclusion)
}


inclusion_donut_calc <- function(location,  # CALCULATION IN LOCAL CRS
                                 inclusion_donut,
                                 crs=loc_epsg){
  area_of_inclusion <- st_difference(st_buffer(st_transform(radar_loc, crs=csr), dist=inclusion_donut[1]),
                                     st_buffer(st_transform(radar_loc, crs=csr), dist=inclusion_donut[2]))
  return(area_of_inclusion)
}


exclusion_angle_calc <- function(angles=b_corners,
                                 distmax=max(inclusion_donut)){ # CALCULATION IN LOCAL CRS
  ## Calculate a length of the triangle legs much larger than mad
  triangle_leg <- 5 * inclusion_donut[2]

  ## Loop over the blocked corners
    ## Angles between which the radar is blocked by its turbine (in [Rad])
    triangle_angles <- c(b_corners[i,1]/180*pi, b_corners[i,2]/180*pi)

    ## Get base point of triangle from radar coordinates (local projection)
    triangle_base <- st_transform(radar_loc, crs=loc_epsg)
    ## Make a triangle using the radar coordinates, gap angles and leg length
    triangle_top <- triangle_base + c(sin(triangle_angles[2])*triangle_leg, cos(triangle_angles[2])*triangle_leg)
    triangle_side <- triangle_base + c(sin(triangle_angles[1])*triangle_leg, cos(triangle_angles[1])*triangle_leg)
    triangle_coordinates <- rbind(c(st_coordinates(triangle_base)[[1]], st_coordinates(triangle_base)[[2]]),
                                  c(st_coordinates(triangle_top)[[1]], st_coordinates(triangle_top)[[2]]),
                                  c(st_coordinates(triangle_side)[[1]], st_coordinates(triangle_side)[[2]]),
                                  c(st_coordinates(triangle_base)[[1]], st_coordinates(triangle_base)[[2]]))

    ## Create a geometry of the triangle corners
    blocked_triangle <-  st_sfc(st_polygon(list(triangle_coordinates)), crs=loc_epsg)

  return(blocked_triangle)
}


