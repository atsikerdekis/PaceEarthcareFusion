###
### Created : Thanos Tsikerdekis (KNMI) | Aug 2024 |
### Contact : thanos.tsikerdekis@knmi.nl
### Purpose : Initialization variables
### Example : Dependency for script 00.start.R, no need to change or run it individually.
### Envirom : ...
###

message("--> Initialization...")

#################
### LIBRARIES ###
#################
library("ncdf4")
library("maps")
library("mapdata")
library("mapproj")
library("maptools")
library("sp")
library("dplyr")
library("oce")
library("ocedata")
library("fields")
library("data.table")
data("coastlineWorldFine")

############
### PATH ###
############
#path_base   <- "/Users/tsikerde/Workspace/AIRSENSE/"         # Laptop
#path_NCO    <- "/Users/tsikerde/opt/anaconda3/envs/nco/bin/" # Laptop
#path_NCCOPY <- "/Users/tsikerde/opt/anaconda3/envs/nco/bin/" # Laptop
#path_MAGICK <- "/opt/homebrew/bin/"                          # Laptop
#path_WGET   <- "/Users/tsikerde/opt/anaconda3/bin/"          # Laptop
#path_data   <- paste0(path_base,"data/")                     # Laptop
path_NCO    <- "~/miniforge3/envs/PEF/bin/"                   # KMMI Desktop
path_NCCOPY <- "~/miniforge3/envs/PEF/bin/"                   # KNMI Desktop
path_MAGICK <- "~/miniforge3/envs/PEF/bin/"                   # KNMI Desktop
path_WGET   <- "~/miniforge3/envs/PEF/bin/"                   # KNMI Desktop
path_base   <- "/usr/people/tsikerde/Workspace/AIRSENSE/PEF/" # KNMI Desktop
path_code   <- paste0(path_base,"code/")
path_call   <- paste0(path_base,"/code/call/")
path_plot   <- paste0(path_base,"plot/")
path_data   <- paste0("/nobackup/users/tsikerde/AIRSENSE/PEF/")
path_temp   <- "/nobackup/users/tsikerde/AIRSENSE/PEF/temp/"
path_data_earthcare <- "/net/pc230016/nobackup_1/users/zadelhof/EarthCARE_DATA/L2/"

#################
### FUNCTIONS ###
#################
source(paste0(path_call,"Spatiotemporal_Collocator.R"))
source(paste0(path_call,"print_progress.R"))
source(paste0(path_call,"ScatterPLOT3.R"))
get_AngstromExponent    <- function(aodW1, aodW2, wave1, wave2) { - log( aodW1 / aodW2 ) / log( wave1 / wave2 ) }
get_AerosolOpticalDepth <- function(aodOLD, waveOLD, waveNEW, angstrom) { aodNEW <- ( (waveNEW/waveOLD) ^ (-angstrom) ) * aodOLD; aodNEW }
compress_image          <- function(file_in, file_out) {system(paste0(path_MAGICK,"convert ",file_in," +dither -colors 256 ",file_out))}

#################
### VARIABLES ###
#################
dpi <- 300
