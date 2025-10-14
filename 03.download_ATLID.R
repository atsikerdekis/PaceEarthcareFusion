########################
### DOWNLOAD SPEXone ###
########################
#message(paste0(path_oads_python,"python ",path_oads,"oads_download.py AEBD -st ",YYYY,MM,DD," -et ",YYYYp1,MMp1,DDp1," --no_unzip"))
system(paste0(path_oads_python,"python ",path_oads,"oads_download.py AEBD:",substr(atlid_version,3,4)," -st ",YYYY,MM,DD," -et ",YYYYp1,MMp1,DDp1," --no_unzip"))
system(paste0(path_oads_python,"python ",path_oads,"oads_download.py ATC:",substr(atlid_version,3,4)," -st ",YYYY,MM,DD," -et ",YYYYp1,MMp1,DDp1," --no_unzip"))
