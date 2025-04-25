#############
### INPUT ###
#############
#path_out <- "/Users/tsikerde/Workspace/AIRSENSE/data/PACE/SPEXone-L2-DL20241125" # Laptop
path_out <- "/nobackup/users/tsikerde/AIRSENSE/PEF/PACE/SPEXone-L2-DL20250127"       # KNMI desktop

#################
### ARGUMENTS ###
#################
args = commandArgs(trailingOnly=TRUE)
if (length(args)==1) {
  sDate     <- args[1]
  eDate     <- args[1]
}
if (length(args)==2) {
  sDate     <- args[1]
  eDate     <- args[2]
}
if (length(args)!=1 & length(args)!=2) {
  stop("\n---> Script requires 1 or 2 arguments e.g.\nRscript download_PACE.R 20240101\n---> Rscript download_PACE.R 20240101 20240102")
}
sDate <- seq.Date(
  from=as.Date( paste0(substr(sDate,1,4),"-",substr(sDate,5,6),"-",substr(sDate,7,8)) ),
  to  =as.Date( paste0(substr(eDate,1,4),"-",substr(eDate,5,6),"-",substr(eDate,7,8)) ),
  by="day")
sDate <- gsub(pattern="-", replacement="", x=sDate)

########################
### DOWNLOAD SPEXone ###
########################
for (d in 1:length(sDate)) {
  message(paste0("wget -r -nH --cut-dirs=3 --no-parent --reject='index.html*' -q -P ",path_out," https://public.spider.surfsara.nl/project/spexone/RemoTAP-SPEXone/",sDate[d],"/"))
  system(paste0("wget -r -nH --cut-dirs=3 --no-parent --reject='index.html*' -q -P ",path_out," https://public.spider.surfsara.nl/project/spexone/RemoTAP-SPEXone/",sDate[d],"/"))
}



