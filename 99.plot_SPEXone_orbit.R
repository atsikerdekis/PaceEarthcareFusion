rm(list=ls())
dev.off()

library("ncdf4")
library("fields")
library("maps")
library("mapdata")
library("mapproj")
library("maptools")
library("oce")
data("coastlineWorldFine")
data("coastlineWorld")
path_MAGICK <- "/opt/homebrew/bin/"
compress_image <- function(file_in, file_out) {system(paste0(path_MAGICK,"convert ",file_in," +dither -colors 256 ",file_out))}

#############
### INPUT ###
#############
path_in  <- "AIRSENSE/PEF/data/PACE/SPEXone-L2-V20240716/20240618/"
path_out <- "AIRSENSE/PEF/plot/"
filenames <- list.files(path=path_in, pattern="PACE_SPEXONE.*.L2.AER_OCEAN_REMOTAP.nc") # PACE_SPEXONE.20240618T215818.V2.L2.AER_OCEAN_REMOTAP.nc

### Color pallete
field_breaks <- c(0.00,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.50,0.60,0.80,1.00)
field_pallete_starting_alpha=30
field_pallete <- colormap( 
  col=colorRampPalette(c("#E1C6DD","#85C0F9","#EBCB3B","#F5793A","#A95AA1","#382119"))(length(field_breaks)-1),
  breaks=field_breaks,
  missingColor="#FFFFFF00")
# Create a sequence of alpha values from 1 to 0
alpha_values <- log( seq(field_pallete_starting_alpha, 100, length.out = length(field_pallete$col)) )
# Normalize the alpha values to be between 0 and 1
alpha_values <- alpha_values / max(alpha_values)
# Convert alpha values to hexadecimal
hex_alpha_values <- sprintf("%02X", round(alpha_values * 255))
# Apply hex alpha
field_pallete$col <- paste0(field_pallete$col, hex_alpha_values)

#################
### READ DATA ###
#################
for (f in 36:36) { # length(filenames)
  message(paste0(f,"/",length(filenames)))
  ############
  ### PLOT ###
  ############
  dpi <- 300
  file_out <- paste0(path_out,gsub(pattern=".nc", replacement=".png", x=filenames[f]))
  png(file_out, width=4*3*dpi+4*0.75*dpi, height=0.25*dpi+6*dpi+3.75*dpi, res=dpi)
  myl <- layout(matrix(c(1:16,17,17,18,18,19,19,20,20),3,8,byrow=T), widths=c(3,0.75,3,0.75,3,0.75,3,0.75), heights=c(0.25,6,3.75));layout.show(myl)
  
  par(mai=c(0,0,0.05,0));plot.new();text(0.5,0.5, bquote(AOD[550]), col="grey20", cex=1.8, family="Century Gothic", srt=0)
  plot.new()
  par(mai=c(0,0,0.05,0));plot.new();text(0.5,0.5, bquote(AOD[550]~Uncertainty), col="grey20", cex=1.8, family="Century Gothic", srt=0)
  plot.new()
  par(mai=c(0,0,0.05,0));plot.new();text(0.5,0.5, bquote(AOD[550]~Relative~Uncertainty), col="grey20", cex=1.8, family="Century Gothic", srt=0)
  plot.new()
  par(mai=c(0,0,0.05,0));plot.new();text(0.5,0.5, bquote(AOD[550]~First~Guess), col="grey20", cex=1.8, family="Century Gothic", srt=0)
  plot.new()
  
  file.nc <- nc_open(paste0(path_in,filenames[f]))
  olon <- ncvar_get(file.nc, "geolocation_data/longitude")
  olat <- ncvar_get(file.nc, "geolocation_data/latitude")
  ovar <- ncvar_get(file.nc, "geophysical_data/aot550")
  nc_close(file.nc)
  par(mai=c(0.4,0.4,0.05,0.1))
  poly.image(x=olon, y=olat, z=ovar, las=1, asp=T, lwd=0.2, family="Century Gothic", zlim=c(0.0,1.0,0.1), breaks=field_pallete$breaks, col=field_pallete$col, border=rgb(0,0,0,0.1))
  map("worldHires", fill=F, add=TRUE, col="black", lwd=0.6, interior=F, asp=T)
  map("worldHires", fill=F, add=TRUE, col="grey40", lwd=0.3, interior=T, asp=T)
  map.grid(nx=72, ny=36, col="grey50", lty=3, lwd=0.5, labels=F)
  try({drawPalette(at=1:length(field_breaks), labels=field_breaks, fullpage=TRUE, col=field_pallete$col, las=1, mai=c(0,0,0.2,0.15), drawTriangles=T, cex=0)}, silent =T)
  axis(4, at=1:length(field_pallete$breaks), labels=sprintf("%.2f",field_pallete$breaks), cex.axis=1, tick=T, las=1, family="Century Gothic", cex=0.8)
  
  file.nc <- nc_open(paste0(path_in,filenames[f]))
  olon <- ncvar_get(file.nc, "geolocation_data/longitude")
  olat <- ncvar_get(file.nc, "geolocation_data/latitude")
  ovar <- ncvar_get(file.nc, "geophysical_data/aot550_uncertainty")
  nc_close(file.nc)
  par(mai=c(0.4,0.4,0.05,0.1))
  poly.image(x=olon, y=olat, z=ovar, las=1, asp=T, lwd=0.2, family="Century Gothic", zlim=c(0.0,1.0,0.1), breaks=field_pallete$breaks, col=field_pallete$col, border=rgb(0,0,0,0.1))
  map("worldHires", fill=F, add=TRUE, col="black", lwd=0.6, interior=F, asp=T)
  map("worldHires", fill=F, add=TRUE, col="grey40", lwd=0.3, interior=T, asp=T)
  map.grid(nx=72, ny=36, col="grey50", lty=3, lwd=0.5, labels=F)
  try({drawPalette(at=1:length(field_breaks), labels=field_breaks, fullpage=TRUE, col=field_pallete$col, las=1, mai=c(0,0,0.2,0.15), drawTriangles=T, cex=0)}, silent =T)
  axis(4, at=1:length(field_pallete$breaks), labels=sprintf("%.2f",field_pallete$breaks), cex.axis=1, tick=T, las=1, family="Century Gothic", cex=0.8)
  
  file.nc <- nc_open(paste0(path_in,filenames[f]))
  olon <- ncvar_get(file.nc, "geolocation_data/longitude")
  olat <- ncvar_get(file.nc, "geolocation_data/latitude")
  ovar <- ncvar_get(file.nc, "geophysical_data/aot550_uncertainty") / ncvar_get(file.nc, "geophysical_data/aot550")
  nc_close(file.nc)
  par(mai=c(0.4,0.4,0.05,0.1))
  poly.image(x=olon, y=olat, z=ovar, las=1, asp=T, lwd=0.2, family="Century Gothic", zlim=c(0.0,1.0,0.1), breaks=field_pallete$breaks, col=field_pallete$col, border=rgb(0,0,0,0.1))
  map("worldHires", fill=F, add=TRUE, col="black", lwd=0.6, interior=F, asp=T)
  map("worldHires", fill=F, add=TRUE, col="grey40", lwd=0.3, interior=T, asp=T)
  map.grid(nx=72, ny=36, col="grey50", lty=3, lwd=0.5, labels=F)
  try({drawPalette(at=1:length(field_breaks), labels=field_breaks, fullpage=TRUE, col=field_pallete$col, las=1, mai=c(0,0,0.2,0.15), drawTriangles=T, cex=0)}, silent =T)
  axis(4, at=1:length(field_pallete$breaks), labels=sprintf("%.2f",field_pallete$breaks), cex.axis=1, tick=T, las=1, family="Century Gothic", cex=0.8)
  
  file.nc <- nc_open(paste0(path_in,filenames[f]))
  olon <- ncvar_get(file.nc, "geolocation_data/longitude")
  olat <- ncvar_get(file.nc, "geolocation_data/latitude")
  ovar <- ncvar_get(file.nc, "geophysical_data/aot550_first_guess")
  nc_close(file.nc)
  par(mai=c(0.4,0.4,0.05,0.1))
  poly.image(x=olon, y=olat, z=ovar, las=1, asp=T, lwd=0.2, family="Century Gothic", zlim=c(0.0,1.0,0.1), breaks=field_pallete$breaks, col=field_pallete$col, border=rgb(0,0,0,0.1))
  map("worldHires", fill=F, add=TRUE, col="black", lwd=0.6, interior=F, asp=T)
  map("worldHires", fill=F, add=TRUE, col="grey40", lwd=0.3, interior=T, asp=T)
  map.grid(nx=72, ny=36, col="grey50", lty=3, lwd=0.5, labels=F)
  try({drawPalette(at=1:length(field_breaks), labels=field_breaks, fullpage=TRUE, col=field_pallete$col, las=1, mai=c(0,0,0.2,0.15), drawTriangles=T, cex=0)}, silent =T)
  axis(4, at=1:length(field_pallete$breaks), labels=sprintf("%.2f",field_pallete$breaks), cex.axis=1, tick=T, las=1, family="Century Gothic", cex=0.8)
  
  ### WORLD MAP
  projection="+proj=robin"
  mapPlot(coastlineWorld, col="grey", grid=T, axes=F, drawBox=F, projection=projection)
  #mapPolygon( lwd=2, border="red",
  #            longitude=c( min(olon), min(olon), max(olon), max(olon) ),
  #            latitude =c( min(olat), max(olat), max(olat), min(olat) ) )
  mapPoints( longitude=olon, latitude=olat, pch=19, col="red", cex=0.001)
  
  
  ### SCATTERPLOTS
  par(mai=c(0.40,0.40,0.1,0.1))
  file.nc <- nc_open(paste0(path_in,filenames[f]))
  var1 <- ncvar_get(file.nc, "geophysical_data/aot550")
  var2 <- ncvar_get(file.nc, "geophysical_data/aot550_uncertainty")
  ID <- which(var2 < 10)
  var1 <- var1[ID]
  var2 <- var2[ID]
  xmin <- ymin <- min(c(var1,var2),na.rm=T)
  xmax <- ymax <- max(c(var1,var2),na.rm=T)
  nc_close(file.nc)
  plot(x=var1, y=var2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), las=1, pch=19, col=rgb(0,0,0,0.3), asp=T, family="Century Gothic", tick=F, line=-0.8, xlab="", ylab="")
  title(xlab=bquote(AOD[550]), family="Century Gothic", line=2)
  title(ylab=bquote(AOD[550]~Uncertainty), family="Century Gothic", line=2)
  abline(a=0, b=1, col="black", lwd=1.5);abline(a=0, b=1, col="green", lwd=0.75)
  grid(lwd=0.5, lty=1)
  
  par(mai=c(0.40,0.40,0.1,0.1))
  file.nc <- nc_open(paste0(path_in,filenames[f]))
  var1 <- ncvar_get(file.nc, "geophysical_data/aot550")
  var2 <- ncvar_get(file.nc, "geophysical_data/aot550_uncertainty") / ncvar_get(file.nc, "geophysical_data/aot550")
  ID <- which(var2 < 10)
  var1 <- var1[ID]
  var2 <- var2[ID]
  xmin <- ymin <- min(c(var1,var2),na.rm=T)
  xmax <- ymax <- max(c(var1,var2),na.rm=T)
  nc_close(file.nc)
  plot(x=var1, y=var2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), las=1, pch=19, col=rgb(0,0,0,0.3), asp=T, family="Century Gothic", tick=F, line=-0.8, xlab="", ylab="")
  title(xlab=bquote(AOD[550]), family="Century Gothic", line=2)
  title(ylab=bquote(AOD[550]~Relative~Uncertainty), family="Century Gothic", line=2)
  abline(a=0, b=1, col="black", lwd=1.5);abline(a=0, b=1, col="green", lwd=0.75)
  grid(lwd=0.5, lty=1)
  
  par(mai=c(0.40,0.40,0.1,0.1))
  file.nc <- nc_open(paste0(path_in,filenames[f]))
  var1 <- ncvar_get(file.nc, "geophysical_data/aot550")
  var2 <- ncvar_get(file.nc, "geophysical_data/aot550_first_guess")
  ID <- which(var2 < 10)
  var1 <- var1[ID]
  var2 <- var2[ID]
  xmin <- ymin <- min(c(var1,var2),na.rm=T)
  xmax <- ymax <- max(c(var1,var2),na.rm=T)
  nc_close(file.nc)
  plot(x=var1, y=var2, xlim=c(xmin,xmax), ylim=c(ymin,ymax), las=1, pch=19, col=rgb(0,0,0,0.3), asp=T, family="Century Gothic", tick=F, line=-0.8, xlab="", ylab="")
  title(xlab=bquote(AOD[550]), family="Century Gothic", line=2)
  title(ylab=bquote(AOD[550]~First~Guess), family="Century Gothic", line=2)
  abline(a=0, b=1, col="black", lwd=1.5);abline(a=0, b=1, col="green", lwd=0.75)
  grid(lwd=0.5, lty=1)
  
  dev.off()
  
  compress_image(file_in=file_out, file_out=file_out)
}
