###
### Created : Thanos Tsikerdekis (KNMI) | Aug 2024 |
### Contact : thanos.tsikerdekis@knmi.nl
### Purpose : Read SPEXone data
### Example : Dependency for script 00.start.R, no need to change or run it individually.
### Envirom : ...
###

#################
### INIT PACE ###
#################
path_in          <- paste0(path_data,"PACE/SPEXone-L2-DL20250127/",gsub("-","",mydate),"/")
filenames        <- list.files(path=path_in, pattern="PACE_SPEXONE.*") # PACE_SPEXONE.20240618T215818.V2.L2.AER_OCEAN_REMOTAP.nc
spex_vardataname <- c("geophysical_data/aot")
spex_wavname     <- c("geophysical_data/wave_optic_prop")
spex_qualityflag <- c("diagnostic_data/quality_flag")

####################
### READ SPEXONE ###
####################
spex_lon <- spex_lat <- spex_tim <- spex_vardata <- spex_orbit_lon <- spex_orbit_lat <- NULL
time_start <- Sys.time()
message(paste0("\n### Reading PACE ..."))
for (f in 1:length(filenames)) { # length(filenames)
  ### Monitor progress
  print_progress(i=f, N=length(filenames), time_start=time_start, time_units="secs")
  
  ### SPEXone data
  file.nc  <- nc_open(paste0(path_in,filenames[f]))
  spex_lon <- c(spex_lon, ncvar_get(file.nc, "geolocation_data/longitude"))
  spex_lat <- c(spex_lat, ncvar_get(file.nc, "geolocation_data/latitude"))
  temp_tim <- as.POSIXct(as.character(ncvar_get(file.nc,"geolocation_data/utc_date")), format="%Y%m%d", tz="UTC") + c(ncvar_get(file.nc,"geolocation_data/fracday")) * 86400
  spex_tim <- c(spex_tim, as.character(temp_tim))
  spex_wav <- ncvar_get(file.nc, spex_wavname)
  
  ### SPEXone "orbit" lon and lat
  spex_orbit_lon <- c( spex_orbit_lon, apply(ncvar_get(file.nc, "geolocation_data/longitude"),2,mean) )
  spex_orbit_lat <- c( spex_orbit_lat, apply(ncvar_get(file.nc, "geolocation_data/latitude"),2,mean) )
  
  ### Read AOD for all wavelengths
  temp_var <- ncvar_get(file.nc, spex_vardataname) # aot(bins_along_track, bins_across_track, number_of_bands_for_optic)
  
  ### Read Quality Flag
  spex_qf <- ncvar_get(file.nc, spex_qualityflag) # int quality_flag(bins_along_track, bins_across_track) 
  spex_qf[which(spex_qf != 0)]=NA                 # quality_flag:long_name = "Quality of retrieved pixels (0: good)" ;
  
  ### Apply Quality Flag
  for (w in 1:dim(temp_var)[1]) { temp_var[w,,] <- temp_var[w,,] + spex_qf }
  
  
  dim(temp_var) <- c( dim(temp_var)[1], dim(temp_var)[2]*dim(temp_var)[3] )
  temp_var      <- aperm(temp_var, c(2,1))
  spex_vardata  <- rbind(spex_vardata, temp_var)
  
  nc_close(file.nc)
}

### Clean
ID <- which(!is.na(spex_vardata)[,which(spex_wav==355)])
spex_lon <- spex_lon[ID]
spex_lat <- spex_lat[ID]
spex_tim <- spex_tim[ID]
spex_tim <- as.POSIXct(spex_tim, tz="UTC")
spex_vardata        <- spex_vardata[ID,]
spex_vardata        <- data.frame(spex_vardata)
names(spex_vardata) <- c(paste0("spex_AOD",spex_wav))
spex_geodata        <- data.frame(ID=1:length(spex_lon), lon=spex_lon, lat=spex_lat, tim=spex_tim)


### TEST 
if (1==2) {
  
  plot(spex_orbit_lon, spex_orbit_lat, type="l")
  lines(atlid_lon, atlid_lat,col="red")
  
  # Required Libraries
  library(sf)
  
  # Create Spatial Lines for Both Sets of Points
  spex_line <- st_linestring(cbind(spex_orbit_lon, spex_orbit_lat))
  atlid_line <- st_linestring(cbind(atlid_lon, atlid_lat))
  
  # Create Spatial Objects
  spex_sf <- st_sfc(spex_line, crs = 4326)
  atlid_sf <- st_sfc(atlid_line, crs = 4326)
  
  # Find the Intersection
  intersection <- st_intersection(spex_sf, atlid_sf)
  
  # Check and Print the Intersection Points
  if (length(intersection) > 0) {
    print(st_coordinates(intersection))
  } else {
    cat("No intersection found")
  }
  
  xmin <- data.frame(st_coordinates(intersection))[1,"X"] - 1
  xmax <- data.frame(st_coordinates(intersection))[1,"X"] + 1
  ymin <- data.frame(st_coordinates(intersection))[1,"Y"] - 1
  ymax <- data.frame(st_coordinates(intersection))[1,"Y"] + 1
  plot(spex_orbit_lon, spex_orbit_lat, type="l", xlim=c(xmin,xmax), ylim=c(ymin,ymax))
  lines(atlid_lon, atlid_lat,col="red")
}


