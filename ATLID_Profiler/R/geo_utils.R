###
### Purpose : Geospatial helper functions used to locate the ATLID orbit point
###           closest to a target longitude/latitude and to select a
###           along-track window of a given width (km) centered on it.
### Example : Dependency for start.R, no need to source directly.
###

#' Great-circle distance (km) between two points, or a point and vectors of
#' points, using the haversine formula.
#'
#' @param lon1,lat1 Longitude/latitude of point A (degrees). Recycled against lon2/lat2.
#' @param lon2,lat2 Longitude/latitude of point(s) B (degrees).
#' @return Numeric vector of distances in kilometers.
haversine_km <- function(lon1, lat1, lon2, lat2) {
  R <- 6371.0088 # Mean Earth radius (km)
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  a <- pmin(pmax(a, 0), 1)
  2 * R * asin(sqrt(a))
}

#' Find the along-track index of an ATLID orbit that is closest to a target point.
#'
#' @param lon,lat Vectors of along-track longitude/latitude for one granule.
#' @param target_lon,target_lat Target point (degrees).
#' @return List with `index` (integer) and `distance_km` (numeric) of the closest point.
find_closest_point <- function(lon, lat, target_lon, target_lat) {
  dist_km <- haversine_km(target_lon, target_lat, lon, lat)
  idx <- which.min(dist_km)
  list(index = idx, distance_km = dist_km[idx])
}

#' Select the along-track indices that make up a window of `window_km`
#' kilometers centered on `center_index`, based on cumulative along-track
#' great-circle distance.
#'
#' @param lon,lat Vectors of along-track longitude/latitude for one granule.
#' @param center_index Integer index to center the window on.
#' @param window_km Total width of the window (km); half on each side.
#' @return Integer vector of indices (in original order) covering the window.
select_along_track_window <- function(lon, lat, center_index, window_km) {
  n <- length(lon)
  half_km <- window_km / 2

  ### Step distances between consecutive along-track points
  step_km <- haversine_km(lon[-n], lat[-n], lon[-1], lat[-1])

  ### Walk backwards from the center accumulating distance until half_km is reached
  lo <- center_index
  acc <- 0
  while (lo > 1 && acc < half_km) {
    acc <- acc + step_km[lo - 1]
    lo <- lo - 1
  }

  ### Walk forwards from the center accumulating distance until half_km is reached
  hi <- center_index
  acc <- 0
  while (hi < n && acc < half_km) {
    acc <- acc + step_km[hi]
    hi <- hi + 1
  }

  seq.int(lo, hi)
}
