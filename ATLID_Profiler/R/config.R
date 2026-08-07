###
### Purpose : Central configuration for ATLID Profiler.
###           All paths, product identifiers, and tunable constants live here.
### Example : Dependency for start.R, no need to source directly.
### Note    : Run start.R from within the ATLID_Profiler/ directory (or set
###           ATLID_PROFILER_HOME) so relative paths below resolve correctly.
###

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

##################
### PATHS      ###
##################
path_base   <- Sys.getenv("ATLID_PROFILER_HOME", getwd())
path_data   <- file.path(path_base, "data")   # Downloaded ATLID granules are stored here
path_temp   <- file.path(path_base, "temp")   # Scratch space for unzipped granules
path_output <- file.path(path_base, "output") # Profile PNGs are written here

### OADS downloader (https://earth.esa.int/eogateway/tools/oads-download)
### Update these (or set the environment variables) to point to your local oads_download.py install.
path_oads_python <- Sys.getenv("ATLID_OADS_PYTHON", "python")
path_oads_script <- Sys.getenv("ATLID_OADS_SCRIPT", file.path(path_base, "..", "oads-download-main", "oads_download.py"))

##########################
### ATLID DATA PRODUCT ###
##########################
atlid_version <- Sys.getenv("ATLID_VERSION", "EXBA") # Baseline/version of ATLID products to request, e.g. "EXAG", "EXBA"
if (!grepl("^[A-Z]{4}$", atlid_version)) {
  stop("atlid_version ('", atlid_version, "') must be a 4-letter ATLID baseline code, e.g. 'EXAG' or 'EXBA'.")
}
### The 2-letter baseline suffix expected by the OADS downloader (e.g. 'BA' from 'EXBA')
atlid_version_suffix <- substr(atlid_version, 3, 4)

### Product codes as used by the OADS downloader / ESA file naming convention
atlid_products <- list(
  EBD = list(code = "AEBD", dir_code = "EBD", label = "Aerosol Extinction (Medium Resolution)"),
  TC  = list(code = "ATC",  dir_code = "TC_", label = "Target Classification")
)

##############################
### PROFILE WINDOW SETTING ###
##############################
window_km     <- 100 # Total along-track window width centered on the closest orbit point (km)
max_search_km <- 500 # Skip a granule if its closest approach to the target is farther than this (km)
max_height_km <- 30   # Upper altitude limit for profile plots (km)

##############################
### VARIABLE DEFINITIONS   ###
##############################
atlid_lonname <- "ScienceData/longitude"
atlid_latname <- "ScienceData/latitude"
atlid_timname <- "ScienceData/time"
atlid_hgtname <- "ScienceData/height"

varname_extinction     <- "ScienceData/particle_extinction_coefficient_355nm_medium_resolution"
varname_quality_status <- "ScienceData/quality_status"
varname_classification <- "ScienceData/classification"

dpi <- 300

dir.create(path_temp,   showWarnings = FALSE, recursive = TRUE)
dir.create(path_output, showWarnings = FALSE, recursive = TRUE)
