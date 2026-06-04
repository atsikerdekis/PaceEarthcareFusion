#########################
### READ ATLID COORDS ###
#########################
atlid_coord <- read.table(paste0("/nobackup/users/tsikerde/AIRSENSE/PEF/data/ATLID/Coordinates/Coordinates_EarthCARE_",YYYY,MM,DD,".csv"), sep=",", head=T)
spex_coord  <- read.table(paste0("/nobackup/users/tsikerde/AIRSENSE/PEF/data/ATLID/Coordinates/Coordinates_PACE_",YYYY,MM,DD,".csv"), sep=",", head=T)
atlid_coord$Datetime <- as.POSIXct(atlid_coord$Datetime, tz="GMT")
spex_coord$Datetime <- as.POSIXct(spex_coord$Datetime, tz="GMT")

print(dim(atlid_coord))
print(dim(spex_coord))

collocated_coord <- NULL
lat_step <- 10
seq_latitude <- seq(-90,80,lat_step)
time_start <- Sys.time()

if (1==2) {
for (lat in 1:length(seq_latitude)) {
  print_progress(i=lat, N=length(seq_latitude), time_start=time_start, time_units="secs")
  ID <- which(atlid_coord$Latitude >= lat & atlid_coord$Latitude < lat+lat_step)
  atlid_coord_temp <- atlid_coord[ID,]
  ID <- which(spex_coord$Latitude >= lat & spex_coord$Latitude < lat+lat_step)
  spex_coord_temp <- spex_coord[ID,]

  temp <- spatiotemporal_collocation(
    origID1=as.numeric(rownames(atlid_coord_temp)),
    lon1=atlid_coord_temp$Longitude,
    lat1=atlid_coord_temp$Latitude,
    tim1=atlid_coord_temp$Datetime,
    origID2=as.numeric(rownames(spex_coord_temp)),
    lon2=spex_coord_temp$Longitude,
    lat2=spex_coord_temp$Latitude,
    tim2=spex_coord_temp$Datetime,
    dist=dist,
    tdif=tdif)

  collocated_coord <- rbind(collocated_coord, temp)
}
}

collocated_coord <- spatiotemporal_collocation(
    origID1=as.numeric(rownames(atlid_coord)),
    lon1=atlid_coord$Longitude,
    lat1=atlid_coord$Latitude,
    tim1=atlid_coord$Datetime,
    origID2=as.numeric(rownames(spex_coord)),
    lon2=spex_coord$Longitude,
    lat2=spex_coord$Latitude,
    tim2=spex_coord$Datetime,
    dist=dist,
    tdif=tdif)

print(dim(collocated_coord))
print(head(collocated_coord))
print(tail(collocated_coord))
print(unique(collocated_coord$TIM1))

selected_time <- unique(as.POSIXct(collocated_coord$TIM1, tz="GMT"))
selected_time <- selected_time[which(abs(diff(selected_time))>30)]
print(selected_time)

######################
### DOWNLOAD ATLID ###
######################
#system(paste0(path_oads_python,"python ",path_oads,"oads_download.py AEBD:",substr(atlid_version,3,4)," -st ",YYYY,MM,DD," -et ",YYYYp1,MMp1,DDp1," --no_unzip"))
#system(paste0(path_oads_python,"python ",path_oads,"oads_download.py ATC:",substr(atlid_version,3,4)," -st ",YYYY,MM,DD," -et ",YYYYp1,MMp1,DDp1," --no_unzip"))
  
system(paste0(path_oads_python,"python ",path_oads,"oads_download.py AEBD:",substr(atlid_version,3,4)," -t ",paste0(format(as.POSIXct(unique(selected_time),tz="GMT"), "%Y%m%d%H%M%S"), collapse=" ")," --no_unzip"))
system(paste0(path_oads_python,"python ",path_oads,"oads_download.py ATC:",substr(atlid_version,3,4)," -t ",paste0(format(as.POSIXct(unique(selected_time),tz="GMT"), "%Y%m%d%H%M%S"), collapse=" ")," --no_unzip"))

#stop("End of script?")
