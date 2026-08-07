###
### Purpose : ATLID Profiler - control script.
###           Given a longitude, latitude and date range, downloads EarthCARE
###           ATLID Level-2a granules, finds the closest orbit point(s) to the
###           target location for each day, and saves a 100 km wide profile
###           PNG for both the aerosol extinction (medium resolution) and the
###           target classification products.
### Usage   : Rscript start.R <lon> <lat> <sDate:YYYYMMDD> <eDate:YYYYMMDD>
### Example : Rscript start.R 167.84 -15.39 20260307 20260317
###           (Ambae/Aoba volcano eruption, Vanuatu, March 2026)
###

### START CLEAN
rm(list = ls())

#####################
### PARSE CLI ARGS ###
#####################
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
  stop("Usage: Rscript start.R <longitude> <latitude> <start_date:YYYYMMDD> <end_date:YYYYMMDD>\n",
       "Example: Rscript start.R 167.84 -15.39 20260307 20260317")
}
target_lon <- as.numeric(args[1])
target_lat <- as.numeric(args[2])
sDate <- as.Date(args[3], format = "%Y%m%d")
eDate <- as.Date(args[4], format = "%Y%m%d")
if (is.na(target_lon) || is.na(target_lat)) stop("Longitude and latitude must be numeric.")
if (is.na(sDate) || is.na(eDate)) stop("Dates must be in YYYYMMDD format.")

####################
### INITIALIZE   ###
####################
suppressMessages(suppressWarnings(library("ncdf4")))
suppressMessages(suppressWarnings(library("oce")))
suppressMessages(suppressWarnings(library("fields")))

Sys.setenv(ATLID_PROFILER_HOME = normalizePath(getwd()))
source("R/config.R")
source("R/geo_utils.R")
source("R/download.R")
source("R/read.R")
source("R/process.R")
source("R/plot.R")

seqDate <- seq.Date(from = sDate, to = eDate, by = "1 day")

message(sprintf(
  "### ATLID Profiler | target=(%.4f, %.4f) | %s to %s | version=%s | window=%dkm",
  target_lon, target_lat, format(sDate), format(eDate), atlid_version, window_km
))

################################
### LOOP OVER EACH DAY       ###
################################
for (mydate in as.character(seqDate)) {
  message(sprintf("\n### Processing %s ###", mydate))
  stime <- Sys.time()

  ### Download ATLID EBD (extinction) and TC (classification) granules for this day
  download_atlid_product(mydate, "EBD")
  download_atlid_product(mydate, "TC")

  ebd_files <- list_atlid_files(mydate, "EBD")
  tc_files  <- list_atlid_files(mydate, "TC")

  if (length(ebd_files) == 0) {
    message("### No ATLID EBD granules available for ", mydate, ", skipping.")
    next
  }

  ebd_ids <- vapply(ebd_files, atlid_orbit_frame_id, character(1))
  tc_ids  <- vapply(tc_files,  atlid_orbit_frame_id, character(1))

  ### Process each EBD granule, matched to its TC granule by orbit+frame id
  for (i in seq_along(ebd_files)) {
    tc_match <- which(tc_ids == ebd_ids[i])
    if (length(tc_match) == 0) {
      message("### No matching TC granule for orbit/frame ", ebd_ids[i], ", skipping.")
      next
    }

    ### Read geolocation only first (cheap) to check proximity to the target
    ebd_geo <- read_atlid_granule(ebd_files[i], character(0))
    closest <- find_closest_point(ebd_geo$lon, ebd_geo$lat, target_lon, target_lat)
    if (closest$distance_km > max_search_km) next

    message(sprintf(
      "### Orbit/frame %s: closest point %.1f km away at %s UTC",
      ebd_ids[i], closest$distance_km, format(ebd_geo$tim[closest$index], "%H:%M:%S")
    ))

    idx_window <- select_along_track_window(ebd_geo$lon, ebd_geo$lat, closest$index, window_km)
    closest_index_in_window <- which(idx_window == closest$index)

    meta <- list(
      target_lon = target_lon, target_lat = target_lat, date = mydate,
      orbit_frame_id = ebd_ids[i], distance_km = closest$distance_km,
      closest_index = closest_index_in_window
    )

    ### Extinction profile (needs both EBD and TC variables)
    ebd_vars <- read_atlid_granule(ebd_files[i], c(varname_extinction, varname_quality_status, atlid_hgtname))
    tc_vars  <- read_atlid_granule(tc_files[tc_match[1]], c(varname_classification, atlid_hgtname))

    extinction_profile <- process_extinction_profile(ebd_vars, tc_vars, idx_window)
    plot_extinction_profile(extinction_profile, meta)

    ### Target classification profile
    classification_profile <- process_classification_profile(tc_vars, idx_window)
    plot_classification_profile(classification_profile, meta)
  }

  etime <- Sys.time()
  message(sprintf("### Finished %s (%.1f minutes)", mydate, as.numeric(difftime(etime, stime, units = "mins"))))
}

message("\n### Done. Profile PNGs saved under: ", path_output)
