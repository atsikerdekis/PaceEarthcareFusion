### Plot an RGB from MSI. They way it is plotted is a bit heavy so avoid for large areas, it may take a long time.
### 
###
plot_MSI_RGB <- function(
    filename,
    lonmin,
    lonmax,
    latmin,
    latmax
    ) {
  
  message("Plotting MSI RGB...")
  
  #################
  ### LIBRARIES ###
  #################
  library(ncdf4)
  
  #################
  ### FUNCTIONS ###
  #################
  #source("print_progress.R")
  stretch <- function(x, lower = 2, upper = 98) {
    q <- quantile(x, probs = c(lower, upper) / 100, na.rm = TRUE)
    x <- (x - q[1]) / (q[2] - q[1])
    x[x < 0] <- 0
    x[x > 1] <- 1
    return(x)
  }
  
  ############
  ### READ ###
  ############
  file.nc <- nc_open(filename)
  msi_lon <- ncvar_get(file.nc, "ScienceData/longitude")
  msi_lat <- ncvar_get(file.nc, "ScienceData/latitude")
  msi_val <- ncvar_get(file.nc, "ScienceData/pixel_values")
  nc_close(file.nc)
  
  #############
  ### CLEAN ###
  #############
  msi_lon[msi_lon > 9.969210e+35] <- NA
  msi_lat[msi_lat > 9.969210e+35] <- NA
  msi_val[msi_val > 9.969210e+35] <- NA
  
  #####################
  ### RGB COMPOSITE ###
  #####################
  # TODO: Maybe other option can be added here...
  # Assign bands (false-color composite)
  # Red  : SWIR-1 (1.65 um)
  # Green: NIR (0.865 um)
  # Blue : VIS (0.67 um)
  msi_rgb <- msi_val[,,c(3,2,1)]
  
  ########################################
  ### APPLY LON/LAT BOUNDARY SELECTION ###
  ########################################
  IDx <- which(sapply(1:nrow(msi_lon), function(i) { any(msi_lon[i, ] > lonmin & msi_lon[i, ] < lonmax & msi_lat[i, ] > latmin & msi_lat[i, ] < latmax) }))
  IDy <- which(sapply(1:ncol(msi_lon), function(j) { any(msi_lon[, j] > lonmin & msi_lon[, j] < lonmax & msi_lat[, j] > latmin & msi_lat[, j] < latmax) }))
  msi_lon <- msi_lon[IDx,IDy]
  msi_lat <- msi_lat[IDx,IDy]
  msi_rgb <- msi_rgb[IDx,IDy,]
  
  ###############
  ### STRETCH ###
  ###############
  msi_rgb[,,1] <- stretch(msi_rgb[,,1])
  msi_rgb[,,2] <- stretch(msi_rgb[,,2])
  msi_rgb[,,3] <- stretch(msi_rgb[,,3])
  
  ####################
  ### NAs to WHITE ###
  ####################
  msi_rgb[is.na(msi_rgb)] <- 1
  
  ################
  ### FLIPPING ###
  ################
  #msi_rgb <- msi_rgb[nrow(msi_rgb):1, , ] # Flip dimensions... for some reason
  msi_rgb <- aperm(msi_rgb, c(2, 1, 3))   # Change dimensions... for some reason
  
  ##################################
  ### INFLATE LON/LAT BOUNDARIES ### -> Purely a plotting thing so borders between pixels not appear
  ##################################
  inflate_quad <- function(x, y, factor = 0.0001) {
    cx <- mean(x)
    cy <- mean(y)
    x_new <- cx + (x - cx) * (1 + factor)
    y_new <- cy + (y - cy) * (1 + factor)
    return(list(x = x_new, y = y_new))
  }
  
  #####################
  ### PLOT POLYGONS ### -> The time consuming part... only efficient for small areas/arrays
  #####################
  nrows <- dim(msi_lon)[1]
  ncols <- dim(msi_lon)[2]
  for (i in 1:(nrows - 1)) {
    time_start <- Sys.time()
    #print_progress(i=i, N=(nrows-1))
    for (j in 1:(ncols - 1)) {
      lon_quad <- c(msi_lon[i, j],     msi_lon[i+1, j],
                    msi_lon[i+1, j+1], msi_lon[i, j+1])
      lat_quad <- c(msi_lat[i, j],     msi_lat[i+1, j],
                    msi_lat[i+1, j+1], msi_lat[i, j+1])
      if (any(is.na(c(lon_quad, lat_quad)))) next
      col <- rgb(msi_rgb[j, i, 1], msi_rgb[j, i, 2], msi_rgb[j, i, 3], maxColorValue = 1)
      inflated <- inflate_quad(lon_quad, lat_quad, factor=0.5)
      polygon(inflated$x, inflated$y, col = col, border = NA)
    } # END OF j LOOP
  } # END OF i LOOP
  
}


