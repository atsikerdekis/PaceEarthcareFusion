###
### Purpose : Open one ATLID granule (EBD or TC) and extract geolocation and
###           the science variables needed to build a profile.
### Example : Dependency for start.R, no need to source directly.
###

#' Unzip (if needed) and open an ATLID granule, returning geolocation and
#' requested science variables.
#'
#' @param filepath Path to the granule file (.ZIP or .h5).
#' @param varnames Character vector of HDF5 variable paths to read (2D or 3D).
#' @return List with `lon`, `lat`, `tim` (POSIXct) and `vars` (named list of matrices/vectors).
read_atlid_granule <- function(filepath, varnames) {
  h5_path <- filepath
  unzipped <- FALSE

  if (grepl("\\.ZIP$", filepath, ignore.case = TRUE)) {
    unzip(zipfile = filepath, exdir = path_temp)
    h5_path <- file.path(path_temp, gsub("\\.ZIP$", ".h5", basename(filepath), ignore.case = TRUE))
    unzipped <- TRUE
  }

  file_nc <- ncdf4::nc_open(h5_path)
  on.exit({
    ncdf4::nc_close(file_nc)
    if (unzipped) {
      unlink(h5_path)
      unlink(gsub("\\.h5$", ".HDR", h5_path, ignore.case = TRUE))
    }
  }, add = TRUE)

  lon <- ncdf4::ncvar_get(file_nc, atlid_lonname)
  lat <- ncdf4::ncvar_get(file_nc, atlid_latname)
  tim <- as.POSIXct(as.numeric(ncdf4::ncvar_get(file_nc, atlid_timname)), origin = "2000-01-01", tz = "UTC")

  vars <- vector("list", length(varnames))
  names(vars) <- varnames
  for (vn in varnames) {
    raw <- ncdf4::ncvar_get(file_nc, vn)
    ### Along-track profile variables come back as [levels, along_track]; transpose to [along_track, levels]
    if (!is.null(dim(raw)) && length(dim(raw)) == 2) raw <- t(raw)
    vars[[vn]] <- raw
  }

  list(lon = lon, lat = lat, tim = tim, vars = vars)
}
