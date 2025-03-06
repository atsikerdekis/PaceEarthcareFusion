spatiotemporal_collocation <- function(origID1=1:length(lon1), origID2=1:length(lon2), lon1, lat1, tim1, lon2, lat2, tim2, dist, tdif) {
  ### lon1: Vector of longitude for dataset1 (numeric)
  ### lat1: Vector of latitude for dataset1 (numeric)
  ### tim1: Vector of time for dataset1 (POSIXct)
  ### lon2: Vector of longitude for dataset2 (numeric)
  ### lat2: Vector of latitude for dataset2 (numeric)
  ### tim2: Vector of time for dataset2 (POSIXct)
  ### dist: Minimum distance between two pairs to consider collocation
  ### tdif: Minimum time (seconds) between two pairs to consider collocation
  
  # Precompute distances between all pairs of points
  lon_distances <- outer(lon1, lon2, "-")
  lat_distances <- outer(lat1, lat2, "-")
  all_distances <- sqrt(lon_distances^2 + lat_distances^2)
  
  # Precompute temporal differences between all pairs of points
  all_temporal_diff <- outer(tim1, tim2, function(x, y) as.numeric(difftime(x, y, units = "secs")))
  
  # Find indices of points meeting distance and time thresholds
  matching_idx <- which(all_distances <= dist & abs(all_temporal_diff) <= tdif, arr.ind = TRUE)
  
  # Extract indices of matched points
  idx1 <- matching_idx[, 1]
  idx2 <- matching_idx[, 2]
  
  # Store matched data
  collocated_data <- data.frame(origID1=origID1[idx1], origID2=origID2[idx2], ID1=idx1, ID2=idx2, LON1=lon1[idx1], LON2=lon2[idx2], LAT1=lat1[idx1], LAT2=lat2[idx2], TIM1=tim1[idx1], TIM2=tim2[idx2], DIST=all_distances[matching_idx], TDIF=abs(all_temporal_diff)[matching_idx])
  
  # For each pair, select the minimum distance for each unique ID1 point
  # Only one pair for each lon1-lat1, if the dist and tdif are satisfied
  collocated_data_unique <- NULL
  uniqueID1 <- unique(collocated_data$ID1)
  for (n in 1:length(uniqueID1)) {
    sameID1 <- which(collocated_data$ID1 == uniqueID1[n])
    if (length(sameID1) == 1) {
      collocated_data_unique <- rbind(collocated_data_unique, collocated_data[sameID1,]) 
    }
    if (length(sameID1) > 1) {
      minID1 <- which.min(collocated_data$DIST[sameID1])
      collocated_data_unique <- rbind(collocated_data_unique, collocated_data[sameID1,][minID1,]) 
    }
  }
  
  # Return matched data
  #return(collocated_data)
  return(collocated_data_unique)
  #return(data.frame(idx1,idx2))
}

# Working Example for development...
if (1==2) {
  # Input data
  tdif <- 3600
  dist <- 2
  lon1 <- seq(1,10,1)
  lat1 <- seq(1,10,1)
  tim1 <- seq.POSIXt(from=as.POSIXct("2021-01-01 00:00:00", tz="GMT"), length.out=length(lon1), by="1 hour", tz="GMT")
  lon2 <- seq(2,6,0.3)
  lat2 <- seq(4,8,0.3)
  tim2 <- seq.POSIXt(from=as.POSIXct("2021-01-01 00:00:00", tz="GMT"), length.out=length(lon2), by="1 hour", tz="GMT")
  plot(lon1, lat1)
  points(lon2,lat2, col="red")
  
  # Example usage
  result <- spatiotemporal_collocation(lon1, lat1, tim1, lon2, lat2, tim2, dist=dist, tdif=tdif)
  result
  
  plot(result$LON1, result$LAT1, col=result$LON1, pch=result$LON1, cex=3, xlim=c(0,11), ylim=c(0,11))
  points(result$LON2, result$LAT2, col=result$LON1, pch=result$LON1, cex=3)
  points(lon1,lat1, col="blue")
  points(lon2,lat2, col="red")
  text(result$LON1, result$LAT1, result$TIM1)
  text(result$LON2, result$LAT2, result$TIM2)
  abline(h=1:10,lwd=0.5, lty=3, col="grey")
  abline(v=1:10,lwd=0.5, lty=3, col="grey")
}

