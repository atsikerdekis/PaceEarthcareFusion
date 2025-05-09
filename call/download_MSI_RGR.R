download_MSI_RGR <- function(
    input_date,
    input_stime,
    input_etime
    ) {

############
### INIT ###
############
path_temp <- "/nobackup/users/tsikerde/AIRSENSE/PEF/temp/"
host <- "ftps://ec-pdgs-dissemination1.eo.esa.int"
user <- "thanos.tsikerdekis@knmi.nl"
pass <- "TakeMyPass1#"

#############
### INPUT ###
#############
YYYY <- substr(input_date,1,4)
MM   <- substr(input_date,5,6)
DD   <- substr(input_date,7,9)
input_stime <- as.POSIXct(paste0(input_date,"T",input_stime), format="%Y%m%dT%H%M%S", tz="UTC")
input_etime <- as.POSIXct(paste0(input_date,"T",input_etime), format="%Y%m%dT%H%M%S", tz="UTC")
input_mtime <- mean(c(input_stime, input_etime))
path <- paste0("EarthCARE/EarthCAREL1Validated/MSI_RGR_1C/AF/",YYYY,"/",MM,"/",DD,"/")

#########################
### GET LIST OF FILES ###
#########################
cmd <- paste0("lftp -u '",user,"','",pass,"' -e 'set ftp:ssl-force true; set ssl:verify-certificate no;set ftp:ssl-protect-data true; cls ",path,"; bye' ",host)
file_list <- system(cmd, intern = TRUE)
#print(file_list)

####################################################
### GET FILES START (stime) AND END (etime) TIME ###
####################################################
stime <- NULL
etime <- NULL
for (n in 1:(length(file_list)-1)) {
  stime <- c(stime, strsplit(file_list[n], "_")[[1]][8])
  etime <- c(etime, strsplit(file_list[n+1], "_")[[1]][8])
}
### For the last file of the day... super non-elegant contratz buddy...
stime <- c(stime, strsplit(file_list[length(file_list)], "_")[[1]][8])
etime <- c(etime, strsplit(file_list[length(file_list)], "_")[[1]][8])
stime <- as.POSIXct(stime, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
etime <- as.POSIXct(etime, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
etime[length(etime)] <- etime[length(etime)] + 15*60

#print(etime)
#print(stime)
#print(input_stime)
#print(input_etime)
print(input_mtime)
print(paste0(input_date,"T",input_stime))
print(paste0(input_date,"T",input_etime))


### Which file is close to the orbit scene
ID <- which(stime <= input_mtime & etime >= input_mtime)
ID <- ID[1]
#print(file_list[ID])

################
### DOWNLOAD ###
################
if (!file.exists(paste0(path_temp,basename(file_list[ID])))) {
  cmd <- paste0("lftp -u '",user,"','",pass,"' -e 'set ftp:ssl-force true; set ssl:verify-certificate no;set ftp:ssl-protect-data true; lcd ",path_temp,"; get ",file_list[ID],"; bye' ",host)
  message(paste0("Downloading file: ",basename(file_list[ID])))
  system(cmd, intern = TRUE)
} else {
  message(paste("File already exists, skipping download: ", path_temp, basename(file_list[ID])))
}

return(paste0(path_temp,basename(file_list[ID])))

}

