#' @importFrom sf
#'
#' aoi
#'
#' @param location coordinate, vector of radar coordinates, or spatial point. Input coordinates in WGS84.
#' @param inclusion_donut vector of minimum and maximum distance in meters to include
#' @param exclusion_angle vector, list or data.frame of angle-pairs to exclude from analysis
#' @param exclusion_location sf-object with locations which can reliably be excluded
#' @param exclusion_location_buffer buffer in unit of crs?
#' @param crs proj4string to transform to for calculations in a metric system
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
aoi <- function(location=c(r_lon, r_lat),
                           inclusion_donut=c(mad,mid),
                           exclusion_angle=b_corners,
                           exclusion_location=turbines,
                           exclusion_location_buffer=turb_exclusion_rad,
                           crs_loc){ #

  # RADAR LOCATION
  if(class(location)[1] == 'numeric'){ # create radar location
  radar_loc <- st_sfc(st_point(location),crs=4326)
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


inclusion_donut_calc <- function(location,  # CALCULATION IN LOCAL CRS
                                 inclusion_donut,
                                 crs=loc_epsg){
  area_of_inclusion <- st_difference(st_buffer(st_transform(location, crs=crs), dist=inclusion_donut[1]),
                                     st_buffer(st_transform(location, crs=crs), dist=inclusion_donut[2]))
  return(area_of_inclusion)
}


exclusion_angle_calc <- function(location=radar_loc,
                                 angles=b_corners,
                                 distmax=max(inclusion_donut)){ # CALCULATION IN LOCAL CRS
  ## Calculate a length of the triangle legs much larger than mad
  triangle_leg <- 5 * inclusion_donut[2]

  ## Loop over the blocked corners
    ## Angles between which the radar is blocked by its turbine (in [Rad])
    triangle_angles <- c(b_corners[i,1]/180*pi, b_corners[i,2]/180*pi)

    ## Get base point of triangle from radar coordinates (local projection)
    triangle_base <- st_transform(location, crs=loc_epsg)
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


# MAYBE ALSO THIS ?
exclusion_location_calc <- function(x=area_of_inclusion,
                                    y=turbines,
                                    buffer=turb_exclusion_rad){ # CALCULATION IN LOCAL CRS
## Add a buffer around each turbine
y_buff <- st_buffer(st_transform(st_sfc(st_multipoint(st_coordinates(y[,geometry])),crs=wgs84_epsg), crs=loc_epsg),
                           dist=buffer)

## Substract the turbine areas
x <- st_difference(x, y_buff)

## Add a tag to the turbines for whether they are in the proximity of the radar (bbox of AoI) mostly for plotting
aoi_bbox <- st_as_sfc(st_bbox(x), crs=loc_epsg) # exclude? can be added in markdown script
y[,near_aoi:=sapply(st_transform(y[,geometry],crs=loc_epsg),st_within,y=aoi_bbox, sparse=FALSE)]

## Transform the AoI  to lon/lat coordinates
x <- st_transform(x, crs=wgs84_epsg)

return(list(x,y))
}

# NOTE Input:
# Radar lat and lon (2 numerics in a 1x2 vector)
# - Minimum and maximum distance (2 numerics in a 1x2 vector, minimum default = 0)
# - Blocked corners (2 numerics per corner in a ?x2 vector, default)
# - Local epsg
# Output:
#   - sf-feature of area of inclusion (polygon or multi-polygon)
# Ideally the function should be able to work using only the radar coordinates and the maximum distance
# Example: rr_get_aoi <- function(r_lat, r_lon, mid=0, mad, b_corners=NA, loc_epsg){<code> return(aoi)}
#
# First we set a minimum and maximum distance around the radar

