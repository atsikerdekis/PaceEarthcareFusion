###
### Created : Thanos Tsikerdekis (KNMI) | Aug 2024 |
### Contact : thanos.tsikerdekis@knmi.nl
### Purpose : Read SPEXone data
### Example : Dependency for script 00.start.R, no need to change or run it individually.
### Envirom : ...
###

### INPUT FOR TESTING
orbit_ID      <- unique(col_geodata[[p]]$orbit_ID)
loop_product  <- 1:length(atlid_product)
loop_orbit    <- 1:length(orbit_ID) # 1:length(orbit_ID)

#############
### INPUT ###
#############
filter_cloud_column <- TRUE
filter_half_column  <- FALSE
plot_version         <- "FilterWarmCloudColumn_FilterSupercooledCloudColumn_IncludeNAT" # String for plot versioning e.g. "filter_cloud_columns_applied"... a mouthful but you get the point

### CREATE PLOT DATE
dir.create( paste0(path_plot,gsub("-","",mydate)), showWarnings=F, recursive = TRUE)

############
### PLOT ###
############
time_start <- Sys.time()
for (p in loop_product) {
  
  loop_variable <- 1:(length(atlid_varname[[p]])-1)
  for ( v in loop_variable) {
    
    message(paste0("\n### Ploting ATLID ",atlid_product[p]," (",gsub("\n","",atlid_vartitle[[p]][v]),") vs SPEXone ..."))
    
    ### TODO: Should make this into a list and put it on init.R
    if (atlid_varname[[p]][v] == "ScienceData/featuremask") {
      field_breaks <- c(-3.5:10.5)
      field_pallete <- colormap( 
        col=colorRampPalette(c("#000000","#FFFFFF","#0016BE","#02BBFA","#3FF2FE","#63F8FE","#82FAFE","#96F6FE","#BEBEBE","#F2DB10","#F5AD18","#CA700D","#FF1002","#A51417"))(length(field_breaks)-1),
        breaks=field_breaks,
        missingColor="#FFFFFF00")
    }
    if (atlid_varname[[p]][v] == "ScienceData/particle_optical_depth_355nm") {
      field_breaks <- c(0:20)
      field_pallete <- colormap( 
        col=colorRampPalette(c("#E1C6DD","#85C0F9","#EBCB3B","#F5793A","#A95AA1","#382119"))(length(field_breaks)-1),
        breaks=field_breaks,
        missingColor="#FFFFFF00")
    }
    if ( grepl("particle_extinction_coefficient", atlid_varname[[p]][v]) ) {
      field_breaks <- seq(-0.0002,0.1,0.0002)
      #field_breaks <- c(seq(-0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1))
      field_pallete <- colormap( 
        #col=colorRampPalette(c("#E1C6DD","#EBCB3B","orange","red","brown","darkred"))(length(field_breaks)-1),
        #col=colorRampPalette(c("white","cyan","blue","navy","darkgreen","green","yellow","orange","red","darkred","#3d0000"))(length(field_breaks)-1),
        #col=colorRampPalette(c("#301934","navy","purple","blue","cyan","green","yellow","orange","red","darkred","#3d0000"))(length(field_breaks)-1),
        col=colorRampPalette(c("#301934","purple","blue","green","yellow","orange","red","darkred"))(length(field_breaks)-1),
        breaks=field_breaks,
        missingColor="#FFFFFF00")
    }
    if (atlid_varname[[p]][v] == "ScienceData/aerosol_classification") {
      field_breaks <- c(1000.5:1008.5)
      field_pallete <- colormap( 
        col=colorRampPalette(c("grey99","#eac4d5","#dbb051","blue","yellow","grey","#86775F","#A52A2A"))(length(field_breaks)-1),
        breaks=field_breaks,
        missingColor="#FFFFFF00")
    }
    if (atlid_varname[[p]][v] == "ScienceData/quality_status") {
      field_breaks <- seq(-0.5,4.5,1)
      field_pallete <- colormap( 
        col=colorRampPalette(c("darkgreen","green","orange","red","darkred"))(length(field_breaks)-1),
        breaks=field_breaks,
        missingColor="#FFFFFF00")
    }
    if (atlid_varname[[p]][v] == "ScienceData/classification") {
      field_breaks <- c(1000.5:1025.5)
      field_pallete <- colormap( 
        col=colorRampPalette(c(
          "black",
          "#a2653e",
          "#d8dcd7",
          "cyan",#"#ffffff",
          "#ffff84",
          "#95fa7b",
          "#96d0fc",
          "#e2ca76",
          "#ffbacd",
          "#b2996e",
          "#937e94",
          "#856798",
          "#ac85a8",
          "#d0fefe",
          "#0fff0e",
          "#2afeb7",
          "#59656d",
          "#76434e",
          "#363737",
          "#ffbacd",
          "#dfc5fe",
          "#84597e",
          "#3b638c",
          "#cfff04",
          "#4efd54"
        ))(length(field_breaks)-1),
        breaks=field_breaks,
        missingColor="#FFFFFF00")
    }
    
    for (s in loop_orbit) {
      print_progress(i=s, N=length(orbit_ID), time_start=time_start, time_units="secs") # print progress
      
      ##########################
      ### ATLID PROFILE DATA ###
      ##########################
      ### ATLID and SPEXone MIN and MAX lon-lat for specific orbit
      ID <- which(col_geodata[[p]]$orbit_ID==orbit_ID[s])
      orbit_lonmax <- max(c(col_geodata[[p]]$LON1[ID],col_geodata[[p]]$LON2[ID]))
      orbit_lonmin <- min(c(col_geodata[[p]]$LON1[ID],col_geodata[[p]]$LON2[ID]))
      orbit_latmax <- max(c(col_geodata[[p]]$LAT1[ID],col_geodata[[p]]$LAT2[ID]))
      orbit_latmin <- min(c(col_geodata[[p]]$LAT1[ID],col_geodata[[p]]$LAT2[ID]))
      ### ATLID find ALL (not only the collocated points) within this MIN and MAX area
      ID <- which(atlid_geodata_sel[[p]]$orbit_ID==orbit_ID[s] &
                    atlid_geodata_sel[[p]]$lon <= orbit_lonmax &
                    atlid_geodata_sel[[p]]$lon >= orbit_lonmin &
                    atlid_geodata_sel[[p]]$lat <= orbit_latmax &
                    atlid_geodata_sel[[p]]$lat >= orbit_latmin)
      ID <- rev(ID)
      ### ATLID select ALL (not only the collocated points) within this MIN and MAX area
      atlid_PROF_plot_lon <- atlid_geodata_sel[[p]]$lon[ID]
      atlid_PROF_plot_lat <- atlid_geodata_sel[[p]]$lat[ID]
      atlid_PROF_plot_ID  <- atlid_geodata_sel[[p]]$atlid_ID[ID]
      atlid_geodata_sel[[p]]$orbit_ID[ID]
      atlid_geodata_sel[[p]]$atlid_ID[ID]
      atlid_PROF_x <- atlid_PROF_plot_lon
      #atlid_PROF_y <- rev(apply(atlid_vardata_sel[[p]]$`ScienceData/height`[ID,],2,mean))/1000 # 2D
      atlid_PROF_y <- atlid_vardata_sel[[p]]$`ScienceData/height`[ID,]/1000 # 3D
      atlid_PROF_y <- atlid_PROF_y[,254:1]
      atlid_PROF_z <- atlid_vardata_sel[[p]][[v]][ID,]
      atlid_PROF_z[which(atlid_PROF_z > 9.969210e+35)]=NA
      
      
      
      #######################################
      ### FILTERS AND VARIABLE CORRECTION ###
      #######################################
      if ( grepl("particle_extinction_coefficient", atlid_varname[[p]][v]) & atlid_product[[p]]=="EBD" ) {
        ### EBD Extinction filtered based on Target Classification (TC)
        # Keep only:
        # TC=10 (Dust)
        # TC=11 (Sea Salt)
        # TC=12 (Continental Pollution)
        # TC=13 (Smoke)
        # TC=14 (Dust Smoke)
        # TC=15  (Dusty Mix)
        # TC=25 (Stratospheric Ash)
        # TC=26 (Stratospheric Sulphate)
        # TC=27 (Stratospheric Smoke)
        # TC=101
        # TC=102
        # TV=104
        # TC=105
        atlid_TC_classification <- atlid_vardata_sel$TC_$`ScienceData/classification`[ID,]
        ID_class <- which(atlid_TC_classification != 10 &
                            #atlid_TC_classification != 1 & # Liquid Cloud (Warm) --> JUST FOR A TEST
                            atlid_TC_classification != 11 &
                            atlid_TC_classification != 12 &
                            atlid_TC_classification != 13 &
                            atlid_TC_classification != 14 &
                            atlid_TC_classification != 15 &
			    atlid_TC_classification != 21 & # NAT = Stratospheric (Nitric Acid Trihydrate) mixtures
                            atlid_TC_classification != 25 &
                            atlid_TC_classification != 26 &
                            atlid_TC_classification != 27 &
                            atlid_TC_classification != 101 &
                            atlid_TC_classification != 102 &
                            atlid_TC_classification != 104 &
                            atlid_TC_classification != 105, arr.ind=T)
        atlid_PROF_z[ID_class]=NA
        
        #########################################
        ### FILTER CLOUD CONTAMINATED COLUMNS ### EBD Extinction filtered based on Clouds -> Exclude the whole column if any pixel is cloud (Warm=1 of Supercooled=2)
        #########################################
        if (filter_cloud_column == TRUE) {
          atlid_TC_classification <- atlid_vardata_sel$TC_$`ScienceData/classification`[ID,]
          ID_class_row <- unique(which(atlid_TC_classification == 1 | atlid_TC_classification==2, arr.ind=T)[,1])
          atlid_PROF_z[ID_class_row,]=NA
        }
        
        #TODO (Not working...)
        # On second thought it doesn't much sense.
        # The half columns can be totally valid columns that could have been picked up by PACE as well.
        # They just have a thinner aerosol layer.
        ##################################### EBD Extinction filtered out columns that are not complete column
        ### FILTER HALF RETRIEVED COLUMNS ### (1) Considering pixel above surface >0m and below <5000m where most aerosol are
        ##################################### (2) Should include at least 95% within this range
        #        if (filter_half_column == TRUE) {
        #          mask <- atlid_PROF_LayerHeight>0 & atlid_PROF_LayerHeight<5000
        #          atlid_EBD_classification <- atlid_vardata_sel$EBD$`ScienceData/quality_status`[ID,]
        #          #ID_class <- which(atlid_PROF_z == 0)
        #          #atlid_PROF_z[ID_class,]=NA
        #        }
        
      }
      
      if ( grepl("particle_extinction_coefficient", atlid_varname[[p]][v]) ) {
        # Filter out Likely_Bad=2,Bad=3 and Very Bad=4 pixels based on quality status
        atlid_quality_status <- atlid_vardata_sel[[p]]$`ScienceData/quality_status`[ID,]
        ID_class <- which(atlid_quality_status == 2 | atlid_quality_status == 3 | atlid_quality_status == 4, arr.ind=T)
        atlid_PROF_z[ID_class]=NA
        ### Extinction (m^-1) to Extinction (Unitless)
        atlid_PROF_LayerThickness <- atlid_vardata_sel[[p]]$`ScienceData/height`[ID,]
        atlid_PROF_LayerThickness <- cbind(atlid_PROF_LayerThickness[,1]+496, atlid_PROF_LayerThickness[,1:253]) - atlid_PROF_LayerThickness
        atlid_PROF_z              <- atlid_PROF_z * atlid_PROF_LayerThickness
        # Make negative extinction to NA? 100% a wrong move...
        atlid_PROF_z[which(atlid_PROF_z<0)]=NA
        # Remove extreme extinction values (very rare)
        atlid_PROF_z[which(atlid_PROF_z > 1)]=NA
      }
      
      ###
      atlid_PROF_z <- atlid_PROF_z[,dim(atlid_PROF_z)[2]:1]
      atlid_PROF_z <- matrix(data=as.numeric(unlist(atlid_PROF_z)), nrow=dim(atlid_PROF_z)[1], ncol=dim(atlid_PROF_z)[2])
      if (atlid_varname[[p]][v] == "ScienceData/aerosol_classification") {
        atlid_PROF_z[which(atlid_PROF_z==-2)]=1001
        atlid_PROF_z[which(atlid_PROF_z==-1)]=1002
        atlid_PROF_z[which(atlid_PROF_z==1)]=1003
        atlid_PROF_z[which(atlid_PROF_z==2)]=1004
        atlid_PROF_z[which(atlid_PROF_z==3)]=1005
        atlid_PROF_z[which(atlid_PROF_z==4)]=1006
        atlid_PROF_z[which(atlid_PROF_z==5)]=1007
        atlid_PROF_z[which(atlid_PROF_z==6)]=1008
      }
      if (atlid_varname[[p]][v] == "ScienceData/classification") {
        atlid_PROF_z[which(atlid_PROF_z==-3)]=1+1000 # Missing Data
        atlid_PROF_z[which(atlid_PROF_z==-2)]=2+1000 # Surface/Subsurface
        atlid_PROF_z[which(atlid_PROF_z==-1)]=3+1000 # Noise
        atlid_PROF_z[which(atlid_PROF_z==0)]=4+1000 # Clear
        atlid_PROF_z[which(atlid_PROF_z==1)]=5+1000 # Liquid Cloud (Warm)
        atlid_PROF_z[which(atlid_PROF_z==2)]=6+1000 # Liquid Cloud (Supercooled)
        atlid_PROF_z[which(atlid_PROF_z==3)]=7+1000 # Ice Cloud
        atlid_PROF_z[which(atlid_PROF_z==10)]=8+1000 # Dust
        atlid_PROF_z[which(atlid_PROF_z==11)]=9+1000 # Sea Salt
        atlid_PROF_z[which(atlid_PROF_z==12)]=10+1000 # Continental Pollution
        atlid_PROF_z[which(atlid_PROF_z==13)]=11+1000 # Smoke
        atlid_PROF_z[which(atlid_PROF_z==14)]=12+1000 # Dusty Smoke
        atlid_PROF_z[which(atlid_PROF_z==15)]=13+1000 # Dusty Mix
        atlid_PROF_z[which(atlid_PROF_z==20)]=14+1000 # STS = Stratospheric Supercooled Ternary droplets
        atlid_PROF_z[which(atlid_PROF_z==21)]=15+1000 # NAT = Stratospheric (Nitric Acid Trihydrate) mixtures
        atlid_PROF_z[which(atlid_PROF_z==22)]=16+1000 # Stratospheric Ice
        atlid_PROF_z[which(atlid_PROF_z==25)]=17+1000 # Stratospheric Ash
        atlid_PROF_z[which(atlid_PROF_z==26)]=18+1000 # Stratospheric Sulphate
        atlid_PROF_z[which(atlid_PROF_z==27)]=19+1000 # Stratospheric Smoke
        atlid_PROF_z[which(atlid_PROF_z==101)]=20+1000 # Unknown Aerosol 1
        atlid_PROF_z[which(atlid_PROF_z==102)]=21+1000 # Unknown Aerosol 2
        atlid_PROF_z[which(atlid_PROF_z==104)]=22+1000 # Unknown Str Aerosol1 
        atlid_PROF_z[which(atlid_PROF_z==105)]=23+1000 # Unknown Str Aerosol 2
        atlid_PROF_z[which(atlid_PROF_z==106)]=24+1000 # Unknown PSC1
        atlid_PROF_z[which(atlid_PROF_z==107)]=25+1000 # Unknown PSC2
      }
      
      ### SPEXone
      spex_plot_z1 <- NULL
      spex_plot_z2 <- NULL
      spex_plot_origID2 <- NULL
      spex_plot_LON2 <- NULL
      spex_plot_LAT2 <- NULL
      spex_plot_TIM2 <- NULL
      spex_plot_DIST <- NULL
      spex_plot_TDIF <- NULL
      for (i in atlid_geodata_sel[[p]]$atlid_ID[ID]) {
        N_col <- which(col_geodata[[p]]$origID1 == i & col_geodata[[p]]$orbit_ID == orbit_ID[s])
        if (length(N_col)==0) {
          spex_plot_z1 <- c(spex_plot_z1, NA)
          spex_plot_z2 <- c(spex_plot_z2, NA)  
          spex_plot_origID2 <- c(spex_plot_origID2, NA) 
          spex_plot_LON2 <- c(spex_plot_LON2, NA) 
          spex_plot_LAT2 <- c(spex_plot_LAT2, NA) 
          spex_plot_TIM2 <- c(spex_plot_TIM2, NA) 
          spex_plot_DIST <- c(spex_plot_DIST, NA) 
          spex_plot_TDIF <- c(spex_plot_TDIF, NA) 
        }
        if (length(N_col)!=0) {
          ID_spex <- col_geodata[[p]]$origID2[N_col]
          spex_plot_origID2 <- c(spex_plot_origID2, ID_spex)
          spex_plot_LON2 <- c(spex_plot_LON2, col_geodata[[p]]$LON2[N_col])
          spex_plot_LAT2 <- c(spex_plot_LAT2, col_geodata[[p]]$LAT2[N_col])
          spex_plot_TIM2 <- c(spex_plot_TIM2, col_geodata[[p]]$TIM2[N_col])
          spex_plot_DIST <- c(spex_plot_DIST, col_geodata[[p]]$DIST[N_col])
          spex_plot_TDIF <- c(spex_plot_TDIF, col_geodata[[p]]$TDIF[N_col])
          spex_plot_z1 <- c(spex_plot_z1, spex_vardata[ID_spex,"spex_AOD355"])
          spex_plot_z2 <- c(spex_plot_z2, get_AngstromExponent(aodW1=spex_vardata[ID_spex,"spex_AOD440"], aodW2=spex_vardata[ID_spex,"spex_AOD870"], wave1=440, wave2=870) )
        }
      }
      
      #######################################################################
      ### GATHER AND AGGREGATE COMPARISON  BASED ON UNIQUE SPEXone POINTS ###
      #######################################################################
      if ( grepl("particle_extinction_coefficient", atlid_varname[[p]][v]) ) {
        # ATLID AOD calculated from ATLID extinction
        ATLID_AOD_from_EXT <- rowSums(atlid_PROF_z,na.rm=T)
        
        ###
        ### FILTERS AOD
        ###
        ATLID_AOD_from_EXT[which(ATLID_AOD_from_EXT==0)]=NA
        ATLID_AOD_from_EXT[which(is.na(spex_plot_z1))]=NA
        spex_plot_z1[which(is.na(ATLID_AOD_from_EXT))]=NA
	atlid_PROF_z[which(is.na(spex_plot_z1)),]=NA
	
	###
	### FILTER AOD OUTLIERS: Several rules...
	###
        # (1) ATLID AOD > ATLID mean AOD of the scene * 4
        ID_AOD_outlier1 <- which(ATLID_AOD_from_EXT >  mean(ATLID_AOD_from_EXT,na.rm=T)*4)
        # (2) ATLID AOD > ATLID mean AOD of the scene * 3.5 and 1 pixel-distance from liquid clouds (Warm or Supercooled)
        ID_AOD_outlier2 <- which(ATLID_AOD_from_EXT >  mean(ATLID_AOD_from_EXT,na.rm=T)*3.5)
        ID_AOD_outlier2_CloudDistance <- which(apply(atlid_TC_classification, 1, function(row) any(row %in% c(1, 2))))       # Check which ATLID profiles are Liquid Clouds (Warm or SuperCooled)
        CloseToCloudsID <- sapply(ID_AOD_outlier2, function(x) any(abs(x - ID_AOD_outlier2_CloudDistance) <= 1))             # Create a logical vector indicating if each value in Vector1 is close to any in Vector2
        if (length(CloseToCloudsID)>=1) {ID_AOD_outlier2 <- ID_AOD_outlier2[CloseToCloudsID]} else {ID_AOD_outlier2 <- NULL} # Keep only the values that are close to clouds
        # (3) ATLID AOD > ATLID mean AOD of the scene * 3.0 and 2 pixel-distance from liquid clouds (Warm or Supercooled)
        ID_AOD_outlier3 <- which(ATLID_AOD_from_EXT >  mean(ATLID_AOD_from_EXT,na.rm=T)*3.0)
        ID_AOD_outlier3_CloudDistance <- which(apply(atlid_TC_classification, 1, function(row) any(row %in% c(1, 2)))) # Check which ATLID profiles are Liquid Clouds (Warm or SuperCooled)
        CloseToCloudsID <- sapply(ID_AOD_outlier3, function(x) any(abs(x - ID_AOD_outlier3_CloudDistance) <= 2))       # Create a logical vector indicating if each value in Vector1 is close to any in Vector2
        if (length(CloseToCloudsID)>=1) {ID_AOD_outlier3 <- ID_AOD_outlier3[CloseToCloudsID]} else {ID_AOD_outlier3 <- NULL} # Keep only the values that are close to clouds
        # (4) ATLID AOD > ATLID mean AOD of the scene * 2.5 and 3 pixel-distance from liquid clouds (Warm or Supercooled)
        ID_AOD_outlier4 <- which(ATLID_AOD_from_EXT >  mean(ATLID_AOD_from_EXT,na.rm=T)*2.5)
        ID_AOD_outlier4_CloudDistance <- which(apply(atlid_TC_classification, 1, function(row) any(row %in% c(1, 2)))) # Check which ATLID profiles are Liquid Clouds (Warm or SuperCooled)
        CloseToCloudsID <- sapply(ID_AOD_outlier4, function(x) any(abs(x - ID_AOD_outlier4_CloudDistance) <= 3))       # Create a logical vector indicating if each value in Vector1 is close to any in Vector2
        if (length(CloseToCloudsID)>=1) {ID_AOD_outlier4 <- ID_AOD_outlier4[CloseToCloudsID]} else {ID_AOD_outlier4 <- NULL} # Keep only the values that are close to clouds
        # (5) ATLID AOD > ATLID mean AOD of the scene * 2.0 and 4 pixel-distance from liquid clouds (Warm or Supercooled)
        ID_AOD_outlier5 <- which(ATLID_AOD_from_EXT >  mean(ATLID_AOD_from_EXT,na.rm=T)*2.0)
        ID_AOD_outlier5_CloudDistance <- which(apply(atlid_TC_classification, 1, function(row) any(row %in% c(1, 2)))) # Check which ATLID profiles are Liquid Clouds (Warm or SuperCooled)
        CloseToCloudsID <- sapply(ID_AOD_outlier5, function(x) any(abs(x - ID_AOD_outlier5_CloudDistance) <= 4))       # Create a logical vector indicating if each value in Vector1 is close to any in Vector2
        if (length(CloseToCloudsID)>=1) {ID_AOD_outlier5 <- ID_AOD_outlier5[CloseToCloudsID]} else {ID_AOD_outlier5 <- NULL} # Keep only the values that are close to clouds
        # Exlude cases that belong to another category with order
        ID_AOD_outlier2 <- setdiff(ID_AOD_outlier2, ID_AOD_outlier1)
        ID_AOD_outlier3 <- setdiff(ID_AOD_outlier3, c(ID_AOD_outlier1, ID_AOD_outlier2))
        ID_AOD_outlier4 <- setdiff(ID_AOD_outlier4, c(ID_AOD_outlier1, ID_AOD_outlier2, ID_AOD_outlier3))
        ID_AOD_outlier5 <- setdiff(ID_AOD_outlier5, c(ID_AOD_outlier1, ID_AOD_outlier2, ID_AOD_outlier3, ID_AOD_outlier4))
        # Clean and save vectors for ploting
        ATLID_AOD_from_EXT_outliers1 <- ATLID_AOD_from_EXT
        ATLID_AOD_from_EXT_outliers2 <- ATLID_AOD_from_EXT
        ATLID_AOD_from_EXT_outliers3 <- ATLID_AOD_from_EXT
        ATLID_AOD_from_EXT_outliers4 <- ATLID_AOD_from_EXT
        ATLID_AOD_from_EXT_outliers5 <- ATLID_AOD_from_EXT
        if (length(ID_AOD_outlier1)>=1) {ATLID_AOD_from_EXT_outliers1[-ID_AOD_outlier1]=NA} else {ATLID_AOD_from_EXT_outliers1[]=NA}
        if (length(ID_AOD_outlier2)>=1) {ATLID_AOD_from_EXT_outliers2[-ID_AOD_outlier2]=NA} else {ATLID_AOD_from_EXT_outliers2[]=NA}
        if (length(ID_AOD_outlier3)>=1) {ATLID_AOD_from_EXT_outliers3[-ID_AOD_outlier3]=NA} else {ATLID_AOD_from_EXT_outliers3[]=NA}
        if (length(ID_AOD_outlier4)>=1) {ATLID_AOD_from_EXT_outliers4[-ID_AOD_outlier4]=NA} else {ATLID_AOD_from_EXT_outliers4[]=NA}
        if (length(ID_AOD_outlier5)>=1) {ATLID_AOD_from_EXT_outliers5[-ID_AOD_outlier5]=NA} else {ATLID_AOD_from_EXT_outliers5[]=NA}
	ATLID_AOD_from_EXT_outliers <- c(ATLID_AOD_from_EXT_outliers1, ATLID_AOD_from_EXT_outliers2, ATLID_AOD_from_EXT_outliers3, ATLID_AOD_from_EXT_outliers4, ATLID_AOD_from_EXT_outliers5) # Just for use in ymax
        ATLID_AOD_from_EXT[c(ID_AOD_outlier1,ID_AOD_outlier2,ID_AOD_outlier3,ID_AOD_outlier4,ID_AOD_outlier5)]=NA

        plot_data <- data.table(spex_AOD355 = spex_plot_z1,
                                atlid_AOD355 = ATLID_AOD_from_EXT,
                                spex_origID2 = spex_plot_origID2,
                                spex_LON2 = spex_plot_LON2,
                                spex_LAT2 = spex_plot_LAT2,
                                spex_TIM2 = spex_plot_TIM2,
                                spex_DIST = spex_plot_DIST,
                                spex_TDIF = spex_plot_TDIF)
        plot_data <- plot_data[, lapply(.SD, mean, na.rm = TRUE), by = spex_origID2]
        plot_data <- na.omit(plot_data)
        plot_data_all <- rbind(plot_data_all, plot_data)
        
        ##############################
        ### PLOT SCATTERPLOT PLOTS ###
        ##############################
        if (dim(plot_data)[1] > 0) {
          ymax <- max(c(plot_data$spex_AOD355, plot_data$atlid_AOD355),na.rm=T)
          myplot <- ScatterPLOT3(dataX=plot_data$spex_AOD355, dataY=plot_data$atlid_AOD355, titleX=bquote(SPEXone~AOD[355]), titleY=bquote(ATLID~AOD[355]), smin=0, smax=ymax+ymax*0.1, psize=4)
          ggsave(plot=myplot, width=5, height=5, dpi=dpi,
                 filename=paste0(path_plot,gsub("-","",mydate),"/PROFILE_Case_",atlid_version,"_",sprintf("%02d",s),"_",atlid_product[p],"_",gsub(" ","",gsub("\n","",atlid_vartitle[[p]][v])),"_DIST",dist,"_TDIF",tdif,"_",mydate,"_",plot_version,".png"))
        }
      }
      
      ### COLLOCATED DATA
      col_data <- col_geodata[[p]][col_geodata[[p]]$orbit_ID==orbit_ID[s],]
      
      ##########################
      ### PLOT PROFILE PLOTS ###
      ##########################
      dpi <- 300
      png(filename=paste0(path_plot,gsub("-","",mydate),"/Case_",atlid_version,"_",sprintf("%02d",s),"_",atlid_product[p],"_",gsub(" ","",gsub("\n","",atlid_vartitle[[p]][v])),"_DIST",dist,"_TDIF",tdif,"_",mydate,"_",plot_version,".png"), width=6.0*dpi+0.3*dpi+1*dpi, height=0.5*dpi+3*dpi)
      #myl <- layout(mat=matrix(c(1:3,4,2,3,5:7),3,3, byrow=T), widths=c(6.0,0.3,1), heights=c(0.5,0.5,3)) # FOR AE timeseries
      myl <- layout(mat=matrix(c(1:6),2,3, byrow=T), widths=c(6.0,0.3,1), heights=c(1,3))
      layout.show(myl)
      
      ### PLOT AOD FOR ATLID & SPEXone
      par(mai=c(0.0,0.9,0.0,0.5), family="Century Gothic")
      if ( grepl("particle_extinction_coefficient", atlid_varname[[p]][v]) ) { 
        if (all(is.na(spex_plot_z1))==FALSE) {
          ymax <- max(c(spex_plot_z1,ATLID_AOD_from_EXT,ATLID_AOD_from_EXT_outliers),na.rm=T)+0.1*max(c(spex_plot_z1,ATLID_AOD_from_EXT,ATLID_AOD_from_EXT_outliers),na.rm=T)
          diff_x1 <- atlid_PROF_x[1]-diff(atlid_PROF_x)[1]/2
          diff_x2 <- atlid_PROF_x[length(atlid_PROF_x)]+diff(atlid_PROF_x)[length(atlid_PROF_x)-1]/2
          plot(c(diff_x1,atlid_PROF_x,diff_x2),c(NA,spex_plot_z1,NA), lwd=2, xaxt='n', yaxt='n', bty='n', ann=FALSE, ylim=c(0,ymax), col="red", pch=19)
          axis(2, seq(0,100,round(ymax/5,2)), las=1, cex.axis=1.5)
          abline(h=seq(0,100,round(ymax/5,2)), lwd=0.5, col="grey")
          title(ylab=bquote(SPEXone~AOD[355]), line=4, cex.lab=1.8)
          ID       <- which(is.na(spex_plot_z1))
          #split_ID <- which(abs(diff(ID)) > 1)
          #if (length(ID)!=0 & length(split_ID)==0) {
          #  polygon(x=c(min(atlid_PROF_x[ID]),min(atlid_PROF_x[ID]),max(atlid_PROF_x[ID]),max(atlid_PROF_x[ID]),min(atlid_PROF_x[ID])), y=c(-10,10,10,-10,-10), density=10, border=F, angle=45, lwd=0.2)
          #  polygon(x=c(min(atlid_PROF_x[ID]),min(atlid_PROF_x[ID]),max(atlid_PROF_x[ID]),max(atlid_PROF_x[ID]),min(atlid_PROF_x[ID])), y=c(-10,10,10,-10,-10), density=10, border=F, angle=135, lwd=0.2)
          #}
          #if (length(ID)!=0 & length(split_ID)>=1) {
          #  IDs <- c(ID[1], ID[split_ID+1])
          #  IDe <- c(ID[split_ID], ID[length(ID)])
          #  for (n in 1:length(IDs)) {
          #    polygon(x=c(atlid_PROF_x[IDs[n]],atlid_PROF_x[IDs[n]],atlid_PROF_x[IDe[n]],atlid_PROF_x[IDe[n]],atlid_PROF_x[IDs[n]]), y=c(-10,10,10,-10,-10), density=10, border=F, angle=45, lwd=0.2)
          #    polygon(x=c(atlid_PROF_x[IDs[n]],atlid_PROF_x[IDs[n]],atlid_PROF_x[IDe[n]],atlid_PROF_x[IDe[n]],atlid_PROF_x[IDs[n]]), y=c(-10,10,10,-10,-10), density=10, border=F, angle=135, lwd=0.2)
          #  }
          #}
	  abline(v=atlid_PROF_x[ID], col="grey95", lwd=3.5)
          ### Plot ATLID AOD
          points(atlid_PROF_x,ATLID_AOD_from_EXT, col="blue", pch=19)
	  points(atlid_PROF_x,ATLID_AOD_from_EXT_outliers1, col="blue", pch=as.character(1), cex=2)
          points(atlid_PROF_x,ATLID_AOD_from_EXT_outliers2, col="blue", pch=as.character(2), cex=2)
          points(atlid_PROF_x,ATLID_AOD_from_EXT_outliers3, col="blue", pch=as.character(3), cex=2)
          points(atlid_PROF_x,ATLID_AOD_from_EXT_outliers4, col="blue", pch=as.character(4), cex=2)
          points(atlid_PROF_x,ATLID_AOD_from_EXT_outliers5, col="blue", pch=as.character(5), cex=2)
          box(lwd=2, col="grey")
        }
        if (all(is.na(spex_plot_z1))==TRUE) { plot.new() }
      }
      if ( grepl("particle_extinction_coefficient", atlid_varname[[p]][v]) == FALSE) { plot.new() }
      
      ### INFO
      par(mai=c(0,0,0,0), family="Century Gothic")
      plot.new()
      text(0.5,0.5, cex=1.5, 
           paste0("Collocation\n",
                  dist," degrees\n",
                  tdif," seconds\n\n",
                  "Mean distance\n",
                  round(mean(col_data$DIST,na.rm=T),2)," degrees\n\n",
                  "Mean time\ndifference\n",
                  round(mean(col_data$TDIF,na.rm=T),0)," seconds")
      )
      
      ### WORLD MAP
      par(mai=c(0.0,0.5,0.0,0.1), family="Century Gothic")
      lonmin <- min(c(col_data$atlid_lonLON1,col_data$LON2),na.rm=T)
      lonmax <- max(c(col_data$LON1,col_data$LON2),na.rm=T)
      latmin <- min(c(col_data$LAT1,col_data$LAT2),na.rm=T)
      latmax <- max(c(col_data$LAT1,col_data$LAT2),na.rm=T)
      mapPlot(projection=paste0("+proj=tpers +h=1e7 +lon_0=",mean(c(lonmin,lonmax))," +lat_0=",mean(c(latmin,latmax))), 
              longitudelim=c(lonmin-20,lonmax+20), latitudelim=c(latmin-30,latmax+30), grid=F, axes=F, drawBox=F, col="grey95")
      mapGrid(longitude=seq(-180,180,5), latitude=seq(-90,90,5), lwd=0.5)
      mapPoints(spex_geodata$lon, spex_geodata$lat, cex=0.1, col="orange", pch=3)
      mapPoints(atlid_geodata_all[[p]]$lon, atlid_geodata_all[[p]]$lat, col="#00BFFF", pch=19, cex=0.1)
      mapPolygon(longitude=c(lonmin,lonmin,lonmax,lonmax,lonmin), latitude=c(latmin,latmax,latmax,latmin,latmin), border="red", lwd=2)
      box(lwd=2, col="grey")
      
      ### SPEXone AE440-870
      #      par(mai=c(0.0,0.9,0.0,0.5), family="Century Gothic")
      #      plot(atlid_PROF_x,spex_plot_z2, lwd=2, type="l", xaxt='n', yaxt='n', bty='n', ann=FALSE, xaxs="i")
      #      axis(2, seq(0,10,0.2), las=1, cex.axis=1.5)
      #      abline(h=seq(0,10,0.2), lwd=0.5, col="grey")
      #      title(ylab=bquote(SPEXone~AE[440-870]), line=4, cex.lab=1.8)
      #      ID       <- which(is.na(spex_plot_z1))
      #      split_ID <- which(abs(diff(ID)) > 1)
      #      if (length(ID)!=0 & length(split_ID)==0) {
      #        polygon(x=c(min(atlid_PROF_x[ID]),min(atlid_PROF_x[ID]),max(atlid_PROF_x[ID]),max(atlid_PROF_x[ID]),min(atlid_PROF_x[ID])), y=c(-10,10,10,-10,-10), density=10, border=F, angle=45, lwd=0.2)
      #        polygon(x=c(min(atlid_PROF_x[ID]),min(atlid_PROF_x[ID]),max(atlid_PROF_x[ID]),max(atlid_PROF_x[ID]),min(atlid_PROF_x[ID])), y=c(-10,10,10,-10,-10), density=10, border=F, angle=135, lwd=0.2)
      #      }
      #      if (length(ID)!=0 & length(split_ID)>=1) {
      #        IDs <- c(ID[1], ID[split_ID+1])
      #        IDe <- c(ID[split_ID], ID[length(ID)])
      #        for (n in 1:length(IDs)) {
      #          polygon(x=c(atlid_PROF_x[IDs[n]],atlid_PROF_x[IDs[n]],atlid_PROF_x[IDe[n]],atlid_PROF_x[IDe[n]],atlid_PROF_x[IDs[n]]), y=c(-10,10,10,-10,-10), density=10, border=F, angle=45, lwd=0.2)
      #          polygon(x=c(atlid_PROF_x[IDs[n]],atlid_PROF_x[IDs[n]],atlid_PROF_x[IDe[n]],atlid_PROF_x[IDe[n]],atlid_PROF_x[IDs[n]]), y=c(-10,10,10,-10,-10), density=10, border=F, angle=135, lwd=0.2)
      #        }
      #      }
      #      box(lwd=2, col="grey")
      
      ### ATLID PROFILE
      par(mai=c(1.1,0.9,0.0,0.5), family="Century Gothic")
      #image(atlid_PROF_x,atlid_PROF_y,atlid_PROF_z,las=1, col=field_pallete$col, breaks=field_pallete$breaks, cex.axis=1.5, xlab="", ylab="", xaxt='n', yaxt='n', bty='n', ylim=c(0,31))
      #imagep(x=atlid_PROF_x, y=atlid_PROF_y, z=atlid_PROF_z, ylim=c(0,31), zlim=c(0,0.3), 
      #       col=oceColorsTurbo,  # Use oce's Jet colormap
      #       xlab="Distance (km)", ylab="Altitude (km)", 
      #       zlab="Temperature (°C)", 
      #       drawContours=F, las=1)
      dim_x <- dim(atlid_PROF_z)[1]
      dim_y <- dim(atlid_PROF_z)[2]
      x <- matrix(data=rep(atlid_PROF_x,dim_y),nrow=dim_x,ncol=dim_y)
      y <- atlid_PROF_y
      z <- atlid_PROF_z
      z[which(z>max(field_pallete$breaks))]=max(field_pallete$breaks)
      poly.image(x=x, y=y, z=z, col=field_pallete$col, breaks=field_pallete$breaks, xlab="", ylab="", xaxt='n', yaxt='n', bty='n', ylim=c(0,30))
      axis(1, at=atlid_PROF_x, labels=paste0(round(atlid_PROF_plot_lon,2),"\n",round(atlid_PROF_plot_lat,2)), las=1, cex.axis=1.5, line=1.5, tick=F)
      abline(h=seq(0,40,5), lwd=0.5, col="grey")
      abline(v=atlid_PROF_x[ID], col="grey95", lwd=3.5)
      #lines(atlid_PROF_x, atlid_PROF_TropopauseHeight)
      profile_distance <- round(oce::geodDist(longitude1=orbit_lonmin, latitude1=orbit_latmin, longitude2=orbit_lonmax, latitude2=orbit_latmin),1) ### DISTANCE IN KM
      title(xlab=paste0("ATLID Longitude/Latitude    |    Distance=",profile_distance," km"), line=4.5, cex.lab=2.2)
      title(xlab=paste0("ATLID file: ",atlid_filename[[p]][orbit_ID[s]],"    |    ATLID ScienceData/along_track: ",min(atlid_PROF_plot_ID),"-",max(atlid_PROF_plot_ID)), line=6.5, cex.lab=1.5, col="grey80")
      #title(xlab=paste0("ATLID Longitude/Latitude    |    Distance=",profile_distance," km"), line=5, cex.lab=2.2)
      axis(2, seq(0,40,5), las=1, cex.axis=1.5)
      abline(h=seq(0,40,5), lwd=0.5, col="grey")
      title(ylab="ATLID Altitude (km)", line=4, cex.lab=2.2)
      box(lwd=2, col="grey")
      ### Extinction? Then display the percentage of extinction per 5km
      if ( grepl("particle_extinction_coefficient", atlid_varname[[p]][v]) ) {
        x_percentage <- atlid_PROF_x[1] - abs(atlid_PROF_x[1]-atlid_PROF_x[length(atlid_PROF_x)])*0.02
        points(x=x_percentage, y=2.5, col="black", pch=15, cex=10)
        points(x=x_percentage, y=7.5, col="black", pch=15, cex=10)
        points(x=x_percentage, y=12.5, col="black", pch=15, cex=10)
        points(x=x_percentage, y=17.5, col="black", pch=15, cex=10)
        points(x=x_percentage, y=22.5, col="black", pch=15, cex=10)
        points(x=x_percentage, y=27.5, col="black", pch=15, cex=10)
        text(x=x_percentage, y=2.5, labels=paste0(round(sum(atlid_PROF_z[which(atlid_PROF_y < 5, arr.ind=T)],na.rm=T) / sum(atlid_PROF_z,na.rm=T),2)*100,"%"), col="white", cex=2)
        text(x=x_percentage, y=7.5, labels=paste0(round(sum(atlid_PROF_z[which(atlid_PROF_y >= 5 & atlid_PROF_y < 10, arr.ind=T)],na.rm=T) / sum(atlid_PROF_z,na.rm=T),2)*100,"%"), col="white", cex=2)
        text(x=x_percentage, y=12.5, labels=paste0(round(sum(atlid_PROF_z[which(atlid_PROF_y >= 10 & atlid_PROF_y < 15, arr.ind=T)],na.rm=T) / sum(atlid_PROF_z,na.rm=T),2)*100,"%"), col="white", cex=2)
        text(x=x_percentage, y=17.5, labels=paste0(round(sum(atlid_PROF_z[which(atlid_PROF_y >= 15 & atlid_PROF_y < 20, arr.ind=T)],na.rm=T) / sum(atlid_PROF_z,na.rm=T),2)*100,"%"), col="white", cex=2)
        text(x=x_percentage, y=22.5, labels=paste0(round(sum(atlid_PROF_z[which(atlid_PROF_y >= 20 & atlid_PROF_y < 25, arr.ind=T)],na.rm=T) / sum(atlid_PROF_z,na.rm=T),2)*100,"%"), col="white", cex=2)
        text(x=x_percentage, y=27.5, labels=paste0(round(sum(atlid_PROF_z[which(atlid_PROF_y >= 25 & atlid_PROF_y < 30, arr.ind=T)],na.rm=T) / sum(atlid_PROF_z,na.rm=T),2)*100,"%"), col="white", cex=2)
      }
      
      if ( grepl("particle_extinction_coefficient", atlid_varname[[p]][v]) ) {
        x_percentage_max <- atlid_PROF_x[length(atlid_PROF_x)] + abs(atlid_PROF_x[1]-atlid_PROF_x[length(atlid_PROF_x)])*0.02
        points(x=x_percentage_max, y=2.5, col="black", pch=15, cex=12)
        points(x=x_percentage_max, y=7.5, col="black", pch=15, cex=10)
        points(x=x_percentage_max, y=12.5, col="black", pch=15, cex=10)
        points(x=x_percentage_max, y=17.5, col="black", pch=15, cex=10)
        points(x=x_percentage_max, y=22.5, col="black", pch=15, cex=10)
        points(x=x_percentage_max, y=27.5, col="black", pch=15, cex=10)
        temp_aod <- NULL
        for (i in 1:dim(atlid_PROF_y)[1]) { temp_aod <- c(temp_aod, sum(atlid_PROF_z[i, which(atlid_PROF_y[i,] < 5)],na.rm=T) ) }
        text(x=x_percentage_max, y=2.5, labels=round(mean(temp_aod),3), col="white", cex=1.8)
        temp_aod <- NULL
        for (i in 1:dim(atlid_PROF_y)[1]) { temp_aod <- c(temp_aod, sum(atlid_PROF_z[i, which(atlid_PROF_y[i,] >= 5 & atlid_PROF_y[i,] < 10)],na.rm=T) ) }
        text(x=x_percentage_max, y=7.5, labels=round(mean(temp_aod),3), col="white", cex=1.8)
        temp_aod <- NULL
        for (i in 1:dim(atlid_PROF_y)[1]) { temp_aod <- c(temp_aod, sum(atlid_PROF_z[i, which(atlid_PROF_y[i,] >= 10 & atlid_PROF_y[i,] < 15)],na.rm=T) ) }
        text(x=x_percentage_max, y=12.5, labels=round(mean(temp_aod),3), col="white", cex=1.8)
        temp_aod <- NULL
        for (i in 1:dim(atlid_PROF_y)[1]) { temp_aod <- c(temp_aod, sum(atlid_PROF_z[i, which(atlid_PROF_y[i,] >= 15 & atlid_PROF_y[i,] < 20)],na.rm=T) ) }
        text(x=x_percentage_max, y=17.5, labels=round(mean(temp_aod),3), col="white", cex=1.8)
        temp_aod <- NULL
        for (i in 1:dim(atlid_PROF_y)[1]) { temp_aod <- c(temp_aod, sum(atlid_PROF_z[i, which(atlid_PROF_y[i,] >= 20 & atlid_PROF_y[i,] < 25)],na.rm=T) ) }
        text(x=x_percentage_max, y=22.5, labels=round(mean(temp_aod),3), col="white", cex=1.8)
        temp_aod <- NULL
        for (i in 1:dim(atlid_PROF_y)[1]) { temp_aod <- c(temp_aod, sum(atlid_PROF_z[i, which(atlid_PROF_y[i,] >= 25 & atlid_PROF_y[i,] < 30)],na.rm=T) ) }
        text(x=x_percentage_max, y=27.5, labels=round(mean(temp_aod),3), col="white", cex=1.8)
      }
      
      ### ATLID PROFILE LEGEND
      par(mai=c(0.9,0.0,0.9,0.0), family="Century Gothic", bg="#FFFFFFFF")
      if (atlid_varname[[p]][v] == "ScienceData/featuremask" | 
          atlid_varname[[p]][v] == "ScienceData/aerosol_classification" | 
          atlid_varname[[p]][v] == "ScienceData/classification" | 
          atlid_varname[[p]][v] == "ScienceData/quality_status") { try({drawPalette(at=1:length(field_breaks), labels=field_breaks, fullpage=TRUE, col=field_pallete$col, las=1, mai=c(0,0,0,0.4), drawTriangles=F, cex=0)}, silent=T) }
      if (atlid_varname[[p]][v] != "ScienceData/featuremask" & 
          atlid_varname[[p]][v] != "ScienceData/aerosol_classification" & 
          atlid_varname[[p]][v] != "ScienceData/classification" & 
          atlid_varname[[p]][v] != "ScienceData/quality_status") { try({drawPalette(at=1:length(field_breaks), labels=field_breaks, fullpage=TRUE, col=field_pallete$col, las=1, mai=c(0,0,0.2,0.2), drawTriangles=T, cex=0)}, silent=T) }
      mylegend_at     <- 1:length(field_breaks[-1])
      mylegend_labels <- field_breaks[-1]
      axis(3, at=1, labels=paste0("ATLID ",atlid_product[p],"\n",atlid_vartitle[[p]][[v]],"\n"), cex.axis=1.5, line=1, las=1, family="Century Gothic", tick=F)
      if (atlid_varname[[p]][v] == "ScienceData/featuremask") { axis(4, at=mylegend_at+0.5, labels=c("Surface","No\nretrievals","Attenuated","Clear\nSky","Likely\nClear\nSky 1","Likely\nClear\nSky 2","Likely\nClear\nSky 3","Likely\nClear\nSky 4","Low\nAltitude\nAerosol","Aerosol\nThin\nCloud 1","Aerosol\nThin\nCloud 2","Thick\nAerosol\nCloud 1","Thick\nAerosol\nCloud 2","Thick\nCloud"), cex.axis=1.3, las=1, family="Century Gothic", tick=F, adj=0.5) }
      if (atlid_varname[[p]][v] == "ScienceData/aerosol_classification") { axis(4, at=mylegend_at+0.5, labels=c("No Class","OutParSpace","Dust","Sea Salt","Cont. Pollution","Smoke","Dusty Smoke","Dusty Mix"), cex.axis=1.5, las=0, family="Century Gothic", tick=F) }
      if (atlid_varname[[p]][v] == "ScienceData/classification") { axis(4, at=mylegend_at+0.5, labels=c("Missing\nData","Surface\nSubsurface","Noise","Clear","Liquid Cloud\n(Warm)","Liquid Cloud\n(Supercooled)","Ice Cloud","Dust","Sea Salt","Continental\nPollution","Smoke","Dusty Smoke","Dusty Mix","STS","NAT","Stratospheric\nIce","Stratospheric\nAsh","Stratospheric\nSulphate","Stratospheric\nSmoke","Unknown\nAerosol1","Unknown\nAerosol2","Unknown\nStr. Aerosol1","Unknown\nStr. Aerosol2","Unknown\nPSC1","Unknown\nPSC2"), cex.axis=1, las=1, family="Century Gothic", tick=F) }
      if (atlid_varname[[p]][v] == "ScienceData/quality_status") { axis(4, at=mylegend_at+0.5, labels=c("Good","Likely Good","Likely Bad","Bad","Missing or Bad L1"), cex.axis=1.5, las=0, family="Century Gothic", tick=F) }
      if (atlid_varname[[p]][v] != "ScienceData/featuremask" & 
          atlid_varname[[p]][v] != "ScienceData/aerosol_classification" & 
          atlid_varname[[p]][v] != "ScienceData/classification" & 
          atlid_varname[[p]][v] != "ScienceData/quality_status") { 
        mylegend_at     <- 1:length(field_breaks)
        mylegend_labels <- field_breaks
        axis(4, at=mylegend_at[seq(2,1002,50)], labels=mylegend_labels[seq(2,1002,50)], cex.axis=1.5, las=1, family="Century Gothic", tick=F) 
      }
      box(lwd=2, col="grey")
      
      ### MAP ZOOM
      par(mai=c(0.5,0.5,0.0,0.1), family="Century Gothic")
      plot(col_data$LON1, col_data$LAT1, col="white", cex=1, pch=19, cex.axis=1.5, las=1, asp=T, xlab="",ylab="", xlim=c(orbit_lonmin,orbit_lonmax), ylim=c(orbit_latmin,orbit_latmax))
      map("worldHires", fill=T, add=TRUE, col="grey95", lwd=1, interior=F, asp=T)
      points(col_data$LON1, col_data$LAT1, col="blue", xlim=c(117,118), ylim=c(-7,-12), cex=1.5, pch=19, cex.axis=1.5, las=1)
      points(col_data$LON2, col_data$LAT2, col="red", cex=1.5, pch=19)
      points(spex_geodata$lon, spex_geodata$lat, cex=1, col="orange", pch=3)
      for (n in 1:length(col_data$LON1)) { lines(x=c(col_data$LON1[n],col_data$LON2[n]), y=c(col_data$LAT1[n],col_data$LAT2[n]), lwd=0.1, col="red") }
      points(atlid_geodata_all[[p]]$lon, atlid_geodata_all[[p]]$lat, col="#00BFFF", pch=19, cex=0.5)
      abline(v=seq(-180,180,dist),col="grey",lwd=0.5)
      abline(h=seq(-90,90,dist),col="grey",lwd=0.5)
      box(lwd=2, col="grey")
      
      dev.off()
    } # LOOP END FOR ORBIT (s)
  } # LOOP END FOR VARIABLE (v)
} # LOOP END FOR PRODUCT (p)


#######################################################
### WRITE AND PLOT collocated SPEXone vs ATLID data ### (aggregated for each SPEXone unique retrieval)
#######################################################
write.table( x=plot_data_all, file=paste0(path_plot,gsub("-","",mydate),"/PLOT_DATA.txt"), quote=F, row.names=F, sep=";")
ymax   <- 1
myplot <- ScatterPLOT3(dataX=plot_data_all$spex_AOD355, dataY=plot_data_all$atlid_AOD355, titleX=bquote(SPEXone~AOD[355]), titleY=bquote(ATLID~AOD[355]), smin=0, smax=ymax+ymax*0.1, psize=4)
ggsave(plot=myplot, width=6, height=6, dpi=dpi, filename=paste0(path_plot,gsub("-","",mydate),"/PLOT_DATA.png") )



