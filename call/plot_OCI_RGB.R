### Plot an RGB from oci. They way it is plotted is a bit heavy so avoid for large areas, it may take a long time.
### 
###
plot_OCI_RGB <- function(
    filename,
    lonmin,
    lonmax,
    latmin,
    latmax,
    RGB_composite="true" # "true" or "pseudo"
) {
  
  #################
  ### LIBRARIES ###
  #################
  library(ncdf4)
  library(abind)
  
  #################
  ### FUNCTIONS ###
  #################
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
  oci_lon <- ncvar_get(file.nc, "geolocation_data/longitude")
  oci_lat <- ncvar_get(file.nc, "geolocation_data/latitude")
  ### True-color
  if (RGB_composite == "true") {
    message("Plotting OCI true-color RGB...")
    # BLUE
    oci_B_wavelength <- ncvar_get(file.nc, "sensor_band_parameters/blue_wavelength")
    oci_B_wavelength_ID <- which.min(abs(oci_B_wavelength - 443))
    oci_B <- ncvar_get(file.nc, "observation_data/rhot_blue")[,,oci_B_wavelength_ID]
    # GREEN
    oci_G_wavelength <- ncvar_get(file.nc, "sensor_band_parameters/blue_wavelength")
    oci_G_wavelength_ID <- which.min(abs(oci_G_wavelength - 551))
    oci_G <- ncvar_get(file.nc, "observation_data/rhot_blue")[,,oci_G_wavelength_ID]
    # RED
    oci_R_wavelength <- ncvar_get(file.nc, "sensor_band_parameters/red_wavelength")
    oci_R_wavelength_ID <- which.min(abs(oci_R_wavelength - 671))
    oci_R <- ncvar_get(file.nc, "observation_data/rhot_red")[,,oci_R_wavelength_ID]
  }
  if (RGB_composite == "pseudo") {
    message("Plotting OCI pseudo-color RGB...")
    ### Pseudo-color -> To get something similar with OCI
    # BLUE
    oci_B_wavelength <- ncvar_get(file.nc, "sensor_band_parameters/red_wavelength")
    oci_B_wavelength_ID <- which.min(abs(oci_B_wavelength - 670)) # Many bands almost picking exactly the same
    oci_B <- ncvar_get(file.nc, "observation_data/rhot_red")[,,oci_B_wavelength_ID]
    # GREEN
    oci_G_wavelength <- ncvar_get(file.nc, "sensor_band_parameters/red_wavelength")
    oci_G_wavelength_ID <- which.min(abs(oci_G_wavelength - 865)) # Many bands almost picking exactly the same
    oci_G <- ncvar_get(file.nc, "observation_data/rhot_red")[,,oci_G_wavelength_ID]
    # RED
    oci_R_wavelength <- ncvar_get(file.nc, "sensor_band_parameters/SWIR_wavelength")
    oci_R_wavelength_ID <- which.min(abs(oci_R_wavelength - 1650)) # Closest being 1619...
    oci_R <- ncvar_get(file.nc, "observation_data/rhot_SWIR")[,,oci_R_wavelength_ID]
  }
  # Merge them into one array
  oci_rgb <- abind(oci_R, oci_G, oci_B, along = 3)
  nc_close(file.nc)
  
  ########################################
  ### APPLY LON/LAT BOUNDARY SELECTION ###
  ########################################
  IDx <- which(sapply(1:nrow(oci_lon), function(i) { any(oci_lon[i, ] > lonmin & oci_lon[i, ] < lonmax & oci_lat[i, ] > latmin & oci_lat[i, ] < latmax) }))
  IDy <- which(sapply(1:ncol(oci_lon), function(j) { any(oci_lon[, j] > lonmin & oci_lon[, j] < lonmax & oci_lat[, j] > latmin & oci_lat[, j] < latmax) }))
  oci_lon <- oci_lon[IDx,IDy]
  oci_lat <- oci_lat[IDx,IDy]
  oci_rgb <- oci_rgb[IDx,IDy,]
  
  ###############
  ### STRETCH ###
  ###############
  oci_rgb[,,1] <- stretch(oci_rgb[,,1])
  oci_rgb[,,2] <- stretch(oci_rgb[,,2])
  oci_rgb[,,3] <- stretch(oci_rgb[,,3])
  
  ####################
  ### NAs to WHITE ###
  ####################
  oci_rgb[is.na(oci_rgb)] <- 1
  
  ################
  ### FLIPPING ###
  ################
  #oci_rgb <- oci_rgb[nrow(oci_rgb):1, , ] # Flip dimensions... for some reason
  oci_rgb <- aperm(oci_rgb, c(2, 1, 3))   # Change dimensions... for some reason
  
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
  nrows <- dim(oci_lon)[1]
  ncols <- dim(oci_lon)[2]
  if (nrows+ncols!=0) { #  Plot only if files are found within the domain of interest
    message(paste0("nrows:",nrows))
    message(paste0("ncols:",ncols))
    for (i in 1:(nrows - 1)) {
      time_start <- Sys.time()
      print_progress(i=i, N=(nrows-1))
      for (j in 1:(ncols - 1)) {
        lon_quad <- c(oci_lon[i, j],     oci_lon[i+1, j],
                      oci_lon[i+1, j+1], oci_lon[i, j+1])
        lat_quad <- c(oci_lat[i, j],     oci_lat[i+1, j],
                      oci_lat[i+1, j+1], oci_lat[i, j+1])
        if (any(is.na(c(lon_quad, lat_quad)))) next
        col <- rgb(oci_rgb[j, i, 1], oci_rgb[j, i, 2], oci_rgb[j, i, 3], maxColorValue = 1)
        inflated <- inflate_quad(lon_quad, lat_quad, factor=0.5)
        polygon(inflated$x, inflated$y, col = col, border = NA)
      } # END OF j LOOP
    } # END OF i LOOP
  }
}


