###
### Created : Thanos Tsikerdekis (KNMI) | Aug 2024 |
### Contact : thanos.tsikerdekis@knmi.nl
### Purpose : Starting script of the SPEXone VS ATLID comparison
### Example : Rscript 01.start.R
### Envirom : mamba activate PEF (set it up via: environment/PEF_environment.yml)
###

### TODO
### 

### START CLEAN
rm(list=ls())

#############
### INPUT ###
#############
dist  <- 0.2              # Spatial Distance for Collocation (degrees)
tdif  <- 7200             # Temporal Difference for Collocation (seconds)
sDate <- "2025-01-05"     # Start date
eDate <- "2025-01-05"     # End date
atlid_product <- c("TC_","EBD") # Which ATLID products to analyze? Can be several at once: e.g. c("TC_","FM_","EBD","AER")
atlid_version <- "EXBA" # Which ATLID version to use
flag_plot_MSI <- TRUE
flag_plot_OCI <- TRUE

####################
### INITITIALIZE ###
####################
#path_code <- "/Users/tsikerde/Workspace/AIRSENSE/PEF/code/"     # Laptop
path_code <- "/usr/people/tsikerde/Workspace/AIRSENSE/PEF/code/" # KNMI Desktop
source(paste0(path_code,"01.init.R"))
seqDate <- seq.Date(from=as.Date(sDate), to=as.Date(eDate), by="1 day")

###############################
### START LOOP FOR EACH DAY ###
###############################
#mydate = as.character(seqDate)[1] # Debugging
for (mydate in as.character(seqDate)) {
  ### Init
  plot_data_all <- NULL

  ### Informer
  message(paste0("### Started analysis for ",mydate))
  
  ### Start timer
  stime <- Sys.time()
  
  ### Time stuff
  YYYY <- substr(mydate,1,4)
  MM   <- substr(mydate,6,7)
  DD   <- substr(mydate,9,10)
  MM1  <- as.numeric( substr(mydate,6,7) )
  DD1  <- as.numeric( substr(mydate,9,10) )
  # Next day
  YYYYp1 <- format(as.Date(mydate)+1, "%Y")
  MMp1 <- format(as.Date(mydate)+1, "%m")
  DDp1 <- format(as.Date(mydate)+1, "%d")
  DDDp1 <- sprintf( "%03d", yday(as.Date(mydate)+1) )

  ### Download SPEXone 
  message("### Download SPEXone")
  source("02.download_SPEXone.R")
  
  ### Check for data availability
  path_spex_date <- paste0(path_spex,gsub("-","",mydate),"/")
  if (dir.exists(path_spex_date)==TRUE)  { data_found <- TRUE }
  if (dir.exists(path_spex_date)==FALSE) { data_found <- FALSE }

  if (data_found==TRUE) {
    ### Download EarthCARE -> Do this after checking for SPEXone data availability to avoid downloading ATLID
    message("### Download ATLID")
    #source("03.download_ATLID_no_filter_dl.R")
    #source("03.download_ATLID_filter_dl.R")

    ### Read SPEXone
    source(paste0(path_code,"04.read_SPEXone.R"))

    ### Read ATLID
    source(paste0(path_code,"05.read_ATLID+COLLOCATE.R"))
    ### If collocated data found plot
    if (length(col_geodata$EBD$origID1)==0) { message(paste0("No collocated data found between SPEXone and ATLID for ",mydate)) }
    if (length(col_geodata$EBD$origID1)!=0) { source(paste0(path_code,"06.plot_profiles.R")) }
  }
  if (data_found==FALSE) { message(paste0("No data for SPEXone, thus skipping ",mydate)) }

  ### End timer
  etime <- Sys.time()
  message(paste0("\n### Ended analysis for ",mydate," (",round(difftime(etime,stime, units="mins"),1)," minutes)"))
  
} # END LOOP date
