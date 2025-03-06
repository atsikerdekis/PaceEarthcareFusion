###############
### PREPARE ###
###############
version     <- "_SPEX-QF-0.3"
mydata      <- col_vardata_all$AER
mydata$date <- as.Date(mydata$TIM1)
ID          <- which(mydata$atlid_AOD355 < 3)
mydata      <- mydata[ID,]

##############################
### PLOT SCATTERPLOT PLOTS ###
##############################
### (1) UNIQUE SPEX (origID2)
### (2) MIN DIST
mydata_plot <- mydata %>%
  group_by(origID2) %>%
  slice_min(DIST, with_ties = FALSE)
myplot <- ScatterPLOT3(dataX=mydata_plot$spex_AOD355,
                       dataY=mydata_plot$atlid_AOD355, 
                       titleX=bquote(SPEXone~AOD[355]), 
                       titleY=bquote(ATLID~AOD[355]), 
                       smin=0, smax=2.5) # smax=3
filename <- paste0("AIRSENSE/PEF/plot/ATLIDvsSPEXone_SCATTERPLOT_",gsub("-","",sDate),"-",gsub("-","",eDate),"_PRODUCT-AER","_VAR-AOD355","_DIST",dist,"_TDIF",tdif,"_unique-SPEX_min-DIST",version,".png")
ggsave(plot=myplot, width=5, height=5, dpi=dpi, filename=filename)

##############################
### PLOT SCATTERPLOT PLOTS ###
##############################
unloadNamespace("dplyr")
library("dplyr")
# Group by 'orbit_ID' and 'date' and count the number of points
mydata_counts <- mydata_plot %>%
  group_by(orbit_ID, date) %>%
  summarise(
    count = dplyr::n(),
    mean_LON1 = mean(LON1, na.rm = TRUE),
    mean_LAT1 = mean(LAT1, na.rm = TRUE)
  )
# Normalize 'count' to a 0-1 range
min_count <- min(mydata_counts$count)
max_count <- max(mydata_counts$count)
mydata_counts$normalized_count <- (mydata_counts$count - min_count) / (max_count - min_count)
mydata_counts$normalized_count <- 1 + (mydata_counts$normalized_count * (8 - 2)) # Scale the normalized count to a specific range
filename <- paste0("AIRSENSE/PEF/plot/ATLIDvsSPEXone_MAP_",gsub("-","",sDate),"-",gsub("-","",eDate),"_PRODUCT-AER","_VAR-AOD355","_DIST",dist,"_TDIF",tdif,"_unique-SPEX_min-DIST",version,".png")
png(filename=filename, width=5.5*dpi, height=3*dpi)
par(mar=c(0,0,0,0), family="Century Gothic")
mapPlot(coastlineWorldFine, grid=FALSE, projection="+proj=robin", axes=F, col="grey95", drawBox=F)
mapPoints(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, col=rgb(1,0,0,0.8), pch=19, cex=mydata_counts$normalized_count)
mapPoints(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, col=rgb(0,0,1,1), pch=1, cex=mydata_counts$normalized_count, lwd=2)
mapText(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, labels=mydata_counts$count, cex=mydata_counts$normalized_count/3)
title(main=paste0("\n\n\nNumber of collocated points (",sum(mydata_counts$count),")\nPeriod: ",gsub("-","",sDate),"-",gsub("-","",eDate),", Distance=",dist,", Temporal Difference=",tdif))
mapGrid(col="grey20", lty=1, lwd=0.5, dlongitude=15, dlatitude=15)
dev.off()

##############################################################################################################
##############################################################################################################
##############################################################################################################

###############
### PREPARE ###
###############
version     <- "SouthPAcific"
mydata      <- col_vardata_all$AER
mydata$date <- as.Date(mydata$TIM1)
ID          <- which(mydata$atlid_AOD355 < 3 & mydata$LON1 < -90 & mydata$LAT1 < 0)
mydata      <- mydata[ID,]

##############################
### PLOT SCATTERPLOT PLOTS ###
##############################
### (1) UNIQUE SPEX (origID2)
### (2) MIN DIST
mydata_plot <- mydata %>%
  group_by(origID2) %>%
  slice_min(DIST, with_ties = FALSE)
myplot <- ScatterPLOT3(dataX=mydata_plot$spex_AOD355,
                       dataY=mydata_plot$atlid_AOD355, 
                       titleX=bquote(SPEXone~AOD[355]), 
                       titleY=bquote(ATLID~AOD[355]), 
                       smin=0, smax=2.5) # smax=3
filename <- paste0("AIRSENSE/PEF/plot/ATLIDvsSPEXone_SCATTERPLOT_",gsub("-","",sDate),"-",gsub("-","",eDate),"_PRODUCT-AER","_VAR-AOD355","_DIST",dist,"_TDIF",tdif,"_unique-SPEX_min-DIST",version,".png")
ggsave(plot=myplot, width=5, height=5, dpi=dpi, filename=filename)

##############################
### PLOT SCATTERPLOT PLOTS ###
##############################
unloadNamespace("dplyr")
library("dplyr")
# Group by 'orbit_ID' and 'date' and count the number of points
mydata_counts <- mydata_plot %>%
  group_by(orbit_ID, date) %>%
  summarise(
    count = dplyr::n(),
    mean_LON1 = mean(LON1, na.rm = TRUE),
    mean_LAT1 = mean(LAT1, na.rm = TRUE)
  )
# Normalize 'count' to a 0-1 range
min_count <- min(mydata_counts$count)
max_count <- max(mydata_counts$count)
mydata_counts$normalized_count <- (mydata_counts$count - min_count) / (max_count - min_count)
mydata_counts$normalized_count <- 1 + (mydata_counts$normalized_count * (8 - 2)) # Scale the normalized count to a specific range
filename <- paste0("AIRSENSE/PEF/plot/ATLIDvsSPEXone_MAP_",gsub("-","",sDate),"-",gsub("-","",eDate),"_PRODUCT-AER","_VAR-AOD355","_DIST",dist,"_TDIF",tdif,"_unique-SPEX_min-DIST",version,".png")
png(filename=filename, width=5.5*dpi, height=3*dpi)
par(mar=c(0,0,0,0), family="Century Gothic")
mapPlot(coastlineWorldFine, grid=FALSE, projection="+proj=robin", axes=F, col="grey95", drawBox=F)
mapPoints(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, col=rgb(1,0,0,0.8), pch=19, cex=mydata_counts$normalized_count)
mapPoints(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, col=rgb(0,0,1,1), pch=1, cex=mydata_counts$normalized_count, lwd=2)
mapText(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, labels=mydata_counts$count, cex=mydata_counts$normalized_count/3)
title(main=paste0("\n\n\nNumber of collocated points (",sum(mydata_counts$count),")\nPeriod: ",gsub("-","",sDate),"-",gsub("-","",eDate),", Distance=",dist,", Temporal Difference=",tdif))
mapGrid(col="grey20", lty=1, lwd=0.5, dlongitude=15, dlatitude=15)
dev.off()





##############################################################################################################
##############################################################################################################
##############################################################################################################

###############
### PREPARE ###
###############
version     <- "SouthAmerica"
mydata      <- col_vardata_all$AER
mydata$date <- as.Date(mydata$TIM1)
ID          <- which(mydata$atlid_AOD355 < 3 & mydata$LON1 >= -80 & mydata$LON1 <= -35 & mydata$LAT1 >= -30 & mydata$LAT1 <= 0)
mydata      <- mydata[ID,]

##############################
### PLOT SCATTERPLOT PLOTS ###
##############################
### (1) UNIQUE SPEX (origID2)
### (2) MIN DIST
mydata_plot <- mydata %>%
  group_by(origID2) %>%
  slice_min(DIST, with_ties = FALSE)
myplot <- ScatterPLOT3(dataX=mydata_plot$spex_AOD355,
                       dataY=mydata_plot$atlid_AOD355, 
                       titleX=bquote(SPEXone~AOD[355]), 
                       titleY=bquote(ATLID~AOD[355]), 
                       smin=0, smax=2.5) # smax=3
filename <- paste0("AIRSENSE/PEF/plot/ATLIDvsSPEXone_SCATTERPLOT_",gsub("-","",sDate),"-",gsub("-","",eDate),"_PRODUCT-AER","_VAR-AOD355","_DIST",dist,"_TDIF",tdif,"_unique-SPEX_min-DIST",version,".png")
ggsave(plot=myplot, width=5, height=5, dpi=dpi, filename=filename)

##############################
### PLOT SCATTERPLOT PLOTS ###
##############################
unloadNamespace("dplyr")
library("dplyr")
# Group by 'orbit_ID' and 'date' and count the number of points
mydata_counts <- mydata_plot %>%
  group_by(orbit_ID, date) %>%
  summarise(
    count = dplyr::n(),
    mean_LON1 = mean(LON1, na.rm = TRUE),
    mean_LAT1 = mean(LAT1, na.rm = TRUE)
  )
# Normalize 'count' to a 0-1 range
min_count <- min(mydata_counts$count)
max_count <- max(mydata_counts$count)
mydata_counts$normalized_count <- (mydata_counts$count - min_count) / (max_count - min_count)
mydata_counts$normalized_count <- 1 + (mydata_counts$normalized_count * (8 - 2)) # Scale the normalized count to a specific range
filename <- paste0("AIRSENSE/PEF/plot/ATLIDvsSPEXone_MAP_",gsub("-","",sDate),"-",gsub("-","",eDate),"_PRODUCT-AER","_VAR-AOD355","_DIST",dist,"_TDIF",tdif,"_unique-SPEX_min-DIST",version,".png")
png(filename=filename, width=5.5*dpi, height=3*dpi)
par(mar=c(0,0,0,0), family="Century Gothic")
mapPlot(coastlineWorldFine, grid=FALSE, projection="+proj=robin", axes=F, col="grey95", drawBox=F)
mapPoints(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, col=rgb(1,0,0,0.8), pch=19, cex=mydata_counts$normalized_count)
mapPoints(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, col=rgb(0,0,1,1), pch=1, cex=mydata_counts$normalized_count, lwd=2)
mapText(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, labels=mydata_counts$count, cex=mydata_counts$normalized_count/3)
title(main=paste0("\n\n\nNumber of collocated points (",sum(mydata_counts$count),")\nPeriod: ",gsub("-","",sDate),"-",gsub("-","",eDate),", Distance=",dist,", Temporal Difference=",tdif))
mapGrid(col="grey20", lty=1, lwd=0.5, dlongitude=15, dlatitude=15)
dev.off()






##############################################################################################################
##############################################################################################################
##############################################################################################################

###############
### PREPARE ###
###############
version     <- "SouthAtlantic"
mydata      <- col_vardata_all$AER
mydata$date <- as.Date(mydata$TIM1)
ID          <- which(mydata$atlid_AOD355 < 3 & mydata$LON1 >= -34 & mydata$LON1 <= 15 & mydata$LAT1 >= -30 & mydata$LAT1 <= 15)
mydata      <- mydata[ID,]

##############################
### PLOT SCATTERPLOT PLOTS ###
##############################
### (1) UNIQUE SPEX (origID2)
### (2) MIN DIST
mydata_plot <- mydata %>%
  group_by(origID2) %>%
  slice_min(DIST, with_ties = FALSE)
myplot <- ScatterPLOT3(dataX=mydata_plot$spex_AOD355,
                       dataY=mydata_plot$atlid_AOD355, 
                       titleX=bquote(SPEXone~AOD[355]), 
                       titleY=bquote(ATLID~AOD[355]), 
                       smin=0, smax=2.5) # smax=3
filename <- paste0("AIRSENSE/PEF/plot/ATLIDvsSPEXone_SCATTERPLOT_",gsub("-","",sDate),"-",gsub("-","",eDate),"_PRODUCT-AER","_VAR-AOD355","_DIST",dist,"_TDIF",tdif,"_unique-SPEX_min-DIST",version,".png")
ggsave(plot=myplot, width=5, height=5, dpi=dpi, filename=filename)

##############################
### PLOT SCATTERPLOT PLOTS ###
##############################
unloadNamespace("dplyr")
library("dplyr")
# Group by 'orbit_ID' and 'date' and count the number of points
mydata_counts <- mydata_plot %>%
  group_by(orbit_ID, date) %>%
  summarise(
    count = dplyr::n(),
    mean_LON1 = mean(LON1, na.rm = TRUE),
    mean_LAT1 = mean(LAT1, na.rm = TRUE)
  )
# Normalize 'count' to a 0-1 range
min_count <- min(mydata_counts$count)
max_count <- max(mydata_counts$count)
mydata_counts$normalized_count <- (mydata_counts$count - min_count) / (max_count - min_count)
mydata_counts$normalized_count <- 1 + (mydata_counts$normalized_count * (8 - 2)) # Scale the normalized count to a specific range
filename <- paste0("AIRSENSE/PEF/plot/ATLIDvsSPEXone_MAP_",gsub("-","",sDate),"-",gsub("-","",eDate),"_PRODUCT-AER","_VAR-AOD355","_DIST",dist,"_TDIF",tdif,"_unique-SPEX_min-DIST",version,".png")
png(filename=filename, width=5.5*dpi, height=3*dpi)
par(mar=c(0,0,0,0), family="Century Gothic")
mapPlot(coastlineWorldFine, grid=FALSE, projection="+proj=robin", axes=F, col="grey95", drawBox=F)
mapPoints(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, col=rgb(1,0,0,0.8), pch=19, cex=mydata_counts$normalized_count)
mapPoints(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, col=rgb(0,0,1,1), pch=1, cex=mydata_counts$normalized_count, lwd=2)
mapText(longitude=mydata_counts$mean_LON1, latitude=mydata_counts$mean_LAT1, labels=mydata_counts$count, cex=mydata_counts$normalized_count/3)
title(main=paste0("\n\n\nNumber of collocated points (",sum(mydata_counts$count),")\nPeriod: ",gsub("-","",sDate),"-",gsub("-","",eDate),", Distance=",dist,", Temporal Difference=",tdif))
mapGrid(col="grey20", lty=1, lwd=0.5, dlongitude=15, dlatitude=15)
dev.off()
