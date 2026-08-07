###
### Purpose : Quality filtering, unit conversion and classification recoding
###           for ATLID aerosol extinction and target classification profiles.
###           Filtering logic follows the original PACE-EarthCARE Fusion
###           comparison code (06.plot_profiles.R), stripped of any SPEXone
###           dependency.
### Example : Dependency for start.R, no need to source directly.
###

### Target Classification codes considered "aerosol" (kept in extinction profile)
aerosol_tc_codes <- c(10, 11, 12, 13, 14, 15, 21, 25, 26, 27, 101, 102, 104, 105)
### Target Classification codes for liquid cloud (used to blank whole cloud-contaminated columns)
liquid_cloud_tc_codes <- c(1, 2)

#' Reverse column order (top-of-atmosphere -> surface becomes surface -> top)
#' and coerce to a plain numeric matrix.
flip_levels <- function(mat) {
  mat <- mat[, ncol(mat):1, drop = FALSE]
  matrix(as.numeric(unlist(mat)), nrow = nrow(mat), ncol = ncol(mat))
}

#' Align two along-track x level matrices that may differ by one level
#' (observed occasionally between ATLID products/baselines).
align_levels <- function(mat, n_levels) {
  if (ncol(mat) == n_levels) return(mat)
  if (ncol(mat) == n_levels - 1) return(cbind(mat[, 1], mat))
  mat[, seq_len(n_levels)]
}

#' Build the aerosol extinction profile (unitless optical depth per layer)
#' for a windowed section of one ATLID orbit, filtered using the collocated
#' Target Classification product.
#'
#' @param ebd_vars List returned by read_atlid_granule() for the EBD product.
#' @param tc_vars  List returned by read_atlid_granule() for the TC product.
#' @param idx      Integer vector of along-track indices (the 100 km window).
#' @return List with `x` (along-track index), `y` (height, km, low->high),
#'         `z` (unitless extinction, filtered) and `lon`,`lat`,`tim` for the window.
process_extinction_profile <- function(ebd_vars, tc_vars, idx) {
  n_levels <- ncol(ebd_vars$vars[[atlid_hgtname]])

  extinction <- ebd_vars$vars[[varname_extinction]][idx, , drop = FALSE]
  quality    <- ebd_vars$vars[[varname_quality_status]][idx, , drop = FALSE]
  height     <- ebd_vars$vars[[atlid_hgtname]][idx, , drop = FALSE]
  classification <- align_levels(tc_vars$vars[[varname_classification]][idx, , drop = FALSE], n_levels)

  ### Missing value flag used by EarthCARE products
  extinction[extinction > 9.969210e+35] <- NA
  ### Filter Likely_Bad / Bad / Very_Bad quality pixels
  extinction[quality %in% c(2, 3, 4)] <- NA
  ### Keep only aerosol-classified pixels
  extinction[!(classification %in% aerosol_tc_codes)] <- NA
  ### Blank the full column if it contains any liquid cloud pixel (warm or supercooled)
  cloud_rows <- which(apply(classification, 1, function(row) any(row %in% liquid_cloud_tc_codes)))
  if (length(cloud_rows) > 0) extinction[cloud_rows, ] <- NA

  ### Extinction (m^-1) -> unitless optical depth per layer, using layer thickness (m)
  layer_thickness <- cbind(height[, 1] + 496, height[, seq_len(n_levels - 1)]) - height
  extinction <- extinction * layer_thickness
  extinction[extinction < 0] <- NA  # Negative optical depth is unphysical
  extinction[extinction > 1] <- NA  # Extreme-value guard (rare artefacts)

  list(
    x = seq_along(idx),
    y = flip_levels(height) / 1000,
    z = flip_levels(extinction),
    lon = ebd_vars$lon[idx],
    lat = ebd_vars$lat[idx],
    tim = ebd_vars$tim[idx]
  )
}

#' Recode Target Classification integer codes into contiguous 1000+ codes
#' used by the plotting color palette (see plot.R).
recode_classification <- function(mat) {
  map <- c(
    "-3" = 1, "-2" = 2, "-1" = 3, "0" = 4, "1" = 5, "2" = 6, "3" = 7,
    "10" = 8, "11" = 9, "12" = 10, "13" = 11, "14" = 12, "15" = 13,
    "20" = 14, "21" = 15, "22" = 16, "25" = 17, "26" = 18, "27" = 19,
    "101" = 20, "102" = 21, "104" = 22, "105" = 23, "106" = 24, "107" = 25
  )
  out <- mat
  for (code in names(map)) out[mat == as.numeric(code)] <- map[[code]] + 1000
  out
}

#' Build the Target Classification profile for a windowed section of one
#' ATLID orbit.
#'
#' @param tc_vars List returned by read_atlid_granule() for the TC product.
#' @param idx     Integer vector of along-track indices (the 100 km window).
#' @return List with `x`, `y` (km), `z` (recoded classification codes), `lon`,`lat`,`tim`.
process_classification_profile <- function(tc_vars, idx) {
  classification <- tc_vars$vars[[varname_classification]][idx, , drop = FALSE]
  height         <- tc_vars$vars[[atlid_hgtname]][idx, , drop = FALSE]

  list(
    x = seq_along(idx),
    y = flip_levels(height) / 1000,
    z = flip_levels(recode_classification(classification)),
    lon = tc_vars$lon[idx],
    lat = tc_vars$lat[idx],
    tim = tc_vars$tim[idx]
  )
}
