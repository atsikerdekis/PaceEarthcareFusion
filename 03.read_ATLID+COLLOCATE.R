###
### Created : Thanos Tsikerdekis (KNMI) | Aug 2024 |
### Contact : thanos.tsikerdekis@knmi.nl
### Purpose : Read ATLID data per orbit...
### Example : Dependency for script 00.start.R, no need to change or run it individually.
### Envirom : ...
###

##################
### INIT ATLID ###
##################
### ATLID products
col_vardata_all        <- vector("list", length(atlid_product))
names(col_vardata_all) <- atlid_product
### ATLID geodata information
atlid_lonname        <- "ScienceData/longitude"
atlid_latname        <- "ScienceData/latitude"
atlid_timname        <- "ScienceData/time"
atlid_geodata        <- vector("list", length(atlid_product))
names(atlid_geodata) <- atlid_product
### ATLID varname information
atlid_varname          <- vector("list", length(atlid_product))
names(atlid_varname)   <- atlid_product
atlid_varname[["TC_"]] <- c(#"ScienceData/quality_status",
  "ScienceData/classification",
  "ScienceData/height")
atlid_varname[["FM_"]] <- c("ScienceData/featuremask","ScienceData/height")
atlid_varname[["EBD"]] <- c("ScienceData/quality_status",
                            #"ScienceData/particle_extinction_coefficient_355nm",
                            "ScienceData/particle_extinction_coefficient_355nm_medium_resolution",
                            #"ScienceData/particle_extinction_coefficient_355nm_low_resolution",
                            "ScienceData/height") # "ScienceData/tropopause_height"
atlid_varname[["AER"]] <- c("ScienceData/quality_status","ScienceData/particle_extinction_coefficient_355nm","ScienceData/aerosol_classification","ScienceData/height")
### ATLID vartitle information
atlid_vartitle          <- vector("list", length(atlid_product))
names(atlid_vartitle)   <- atlid_product
atlid_vartitle[["TC_"]] <- c(#"Quality Status",
  "Classification",
  "Height")
atlid_vartitle[["FM_"]] <- c("Feature Mask","Height")
atlid_vartitle[["EBD"]] <- c("Quality Status",
                             #"PEC 355nm\nUnitless",
                             "PEC MedRes 355nm\nUnitless",
                             #"PEC LowRes 355nm\nUnitless",
                             "Height")
atlid_vartitle[["AER"]] <- c("Quality Status","PEC 355nm\nUnitless","Aerosol Class","Height")
### ATLID vardata information
atlid_vardata        <- vector("list", length(atlid_product))
names(atlid_vardata) <- atlid_product
for (product in atlid_product) {
  atlid_vardata[[product]] <- list()
  for (varname in atlid_varname[[product]]) { atlid_vardata[[product]][[varname]]   <- matrix(1)[0,] }
  names(atlid_vardata[[product]]) <- atlid_varname[[product]]
}
### ATLID path_in and filename
atlid_filename        <- vector("list", length(atlid_product))
names(atlid_filename) <- atlid_product
atlid_path_in         <- vector("list", length(atlid_product))
names(atlid_path_in)  <- atlid_product
for (product in atlid_product) {
  atlid_path_in[[product]]  <- paste0(path_data_earthcare,"/ATL_",product,"_2A/",YYYY,"/",MM,"/",DD,"/")
  #atlid_filename[[product]] <- list.files(path=atlid_path_in[[product]], pattern=paste0("ECA_EXAA_ATL_",product,"_2A_*")) # EXAA
  atlid_filename[[product]] <- list.files(path=atlid_path_in[[product]], pattern=paste0("ECA_EXAC_ATL_",product,"_2A_*")) # EXAC
}
### Initializing collocated variables
col_geodata <- atlid_geodata
col_vardata <- atlid_vardata
### Initializing atlid for orbits that contain collocated points
atlid_geodata_sel <- atlid_geodata
atlid_vardata_sel <- atlid_vardata
### Initializing atlid geodata for all orbits
atlid_geodata_all <- atlid_geodata


############
### READ ### Reading atlid per product (p) and file orbit (f) for several variables (v) and doing the collocation with SPEXone
############
for (p in 1:length(atlid_product)) { # 1:length(atlid_product)
  message(paste0("\n### Reading ATLID ",atlid_product[p])," ...")
  time_start <- Sys.time()
  for (f in 1:length(atlid_filename[[p]])) { # length(atlid_filename[[product]])
    print_progress(i=f, N=length(atlid_filename[[p]]), time_start=time_start, time_units="secs")
    ### Unzip
    unzip(zipfile=paste0(atlid_path_in[[p]],atlid_filename[[p]][[f]]), exdir=path_temp)
    ### Open file
    file.nc <- nc_open( paste0(path_temp, gsub("ZIP","h5",atlid_filename[[p]][[f]])) )
    ### Read geodata
    atlid_geodata[[p]] <- data.frame(lon=ncvar_get(file.nc, atlid_lonname), lat=ncvar_get(file.nc, atlid_latname), tim=ncvar_get(file.nc, atlid_timname))
    atlid_geodata[[p]]$tim <- as.POSIXct(as.numeric(atlid_geodata[[p]]$tim), origin="2000-01-01", tz="UTC") # - 86400 * 57 # CORRECTING DATE FOR TESTING To match 2024-06-16
    ### Store geodata for all atlid orbits
    atlid_geodata_all[[p]] <- rbind(atlid_geodata_all[[p]], atlid_geodata[[p]])
    ### Read vardata
    for (v in 1:length(atlid_varname[[p]])) { # 1:length(atlid_varname[[p]])
      vardim <- length(dim(ncvar_get(file.nc, atlid_varname[[p]][[v]])))
      if (vardim==1) { atlid_vardata[[p]][[v]] <- ncvar_get(file.nc, atlid_varname[[p]][[v]]) }                     # 2D variables ( along_track )
      if (vardim==2) { atlid_vardata[[p]][[v]] <- aperm(ncvar_get(file.nc, atlid_varname[[p]][[v]]), perm=c(2,1)) } # 3D variables ( along_track + ... )
    }
    ### Close file
    nc_close(file.nc)
    ### Remove temp
    file.remove(paste0(path_temp, gsub("ZIP","h5",atlid_filename[[p]][[f]])))
    file.remove(paste0(path_temp, gsub("ZIP","HDR",atlid_filename[[p]][[f]])))
    
    ###################
    ### COLLOCATION ###
    ###  PER ORBIT  ###
    ###################
    lonmin <- min(atlid_geodata[[p]]$lon)
    lonmax <- max(atlid_geodata[[p]]$lon)
    latmin <- min(atlid_geodata[[p]]$lat)
    latmax <- max(atlid_geodata[[p]]$lat)
    timmin <- min(atlid_geodata[[p]]$tim)
    timmax <- max(atlid_geodata[[p]]$tim)
    ###
    spex_ID  <- which(
      spex_geodata$lon >= (lonmin + dist) & spex_geodata$lon <= (lonmax - dist) &
        spex_geodata$lat >= (latmin + dist) & spex_geodata$lat <= (latmax - dist) &
        spex_geodata$tim >= (timmin - tdif) & spex_geodata$tim <= (timmax + tdif)
    )
    atlid_ID <- as.numeric(rownames(atlid_geodata[[p]]))
    ###
    if (length(spex_ID)!=0) {
      spex_temp  <- spex_geodata[spex_ID,]
      atlid_temp <- atlid_geodata[[p]]
      collocated_temp <- spatiotemporal_collocation(
        origID1=atlid_ID,
        lon1=atlid_temp$lon,
        lat1=atlid_temp$lat,
        tim1=atlid_temp$tim,
        origID2=spex_ID,
        lon2=spex_temp$lon,
        lat2=spex_temp$lat,
        tim2=spex_temp$tim,
        dist=dist,
        tdif=tdif)
      if (length(collocated_temp$origID1)!=0) {
        message("Found some collocated points... ",length(collocated_temp$origID1))
        ### col_geodata
        col_geodata[[p]]       <- rbind(col_geodata[[p]], data.frame(collocated_temp,orbit_ID=f))
        atlid_geodata_sel[[p]] <- rbind(atlid_geodata_sel[[p]], data.frame(atlid_geodata[[p]], orbit_ID=f, atlid_ID=atlid_ID))
        ### col_vardata: For 2D variables (AOD, AE etc)
        for (v in 1:length(atlid_varname[[p]])) { # 1:length(atlid_varname[[p]])
          if (atlid_varname[[p]][v] == "ScienceData/particle_extinction_coefficient_355nm") {
            atlid_PROF_QualityStatus      <- atlid_vardata[[p]]$`ScienceData/quality_status`[collocated_temp$origID1,]
            atlid_PROF_Extinction         <- atlid_vardata[[p]]$`ScienceData/particle_extinction_coefficient_355nm`[collocated_temp$origID1,]
            atlid_PROF_Extinction[which(atlid_PROF_Extinction > 9.969210e+35)]=NA
            atlid_PROF_Extinction[which(atlid_PROF_QualityStatus == 2 | atlid_PROF_QualityStatus == 3 | atlid_PROF_QualityStatus == 4, arr.ind=T)]=NA # Filter based on quality status
            atlid_PROF_LayerHeight        <- atlid_vardata[[p]]$`ScienceData/height`[collocated_temp$origID1,]
            atlid_PROF_LayerThickness     <- cbind(atlid_PROF_LayerHeight[,1]+496, atlid_PROF_LayerHeight[,1:253]) - atlid_PROF_LayerHeight
            atlid_PROF_ExtinctionUnitless <- atlid_PROF_Extinction * atlid_PROF_LayerThickness # Extinction (m^-1) to Extinction (Unitless)
            atlid_PROF_ExtinctionUnitless[which(atlid_PROF_ExtinctionUnitless<0)]=NA           # Make negative extinction to NA? 100% a wrong move...
            atlid_AOD <- rowSums(atlid_PROF_ExtinctionUnitless,na.rm=T)
            spex_AOD <- spex_vardata$spex_AOD355[collocated_temp$origID2]
            col_vardata[[p]] <- rbind(col_vardata[[p]], data.frame(collocated_temp,atlid_AOD355=atlid_AOD,spex_AOD355=spex_AOD,orbit_ID=f))
            col_vardata_all[[p]] <- rbind(col_vardata_all[[p]], col_vardata[[p]]) # This is before filtering with EBD with TC, so it is unusable...
          }
        }
        ### Add as a list of the atlid_vardata_sel the orbit ID
        atlid_vardata_sel[[p]]$orbit_ID <- c( atlid_vardata_sel[[p]]$orbit_ID, rep(f, dim(atlid_vardata[[p]][[v]])[1]) )
        for (v in 1:length(atlid_varname[[p]])) {
          vardim <- length(dim(atlid_vardata[[p]][[v]]))
          if (vardim==1) { atlid_vardata_sel[[p]][[v]] <- c(atlid_vardata_sel[[p]][[v]], atlid_vardata[[p]][[v]]) }
          if (vardim==2) { atlid_vardata_sel[[p]][[v]] <- rbind(atlid_vardata_sel[[p]][[v]], atlid_vardata[[p]][[v]]) }
        }
      } # END IF FOR FOUNDED COLLOCATTED POINTS
    } # END IF FOR COLLOCATION CHECK
  } # END LOOP FILENAME (f)
} # END LOOP PRODUCTS (p)


