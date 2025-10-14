########################
### DOWNLOAD SPEXone ###
########################
message(paste0("wget -r -nH --cut-dirs=3 --no-parent --reject='index.html*' -q -P ",path_spex," https://public.spider.surfsara.nl/project/spexone/RemoTAP-SPEXone/",YYYY,MM,DD,"/"))
system(paste0("wget -r -nH --cut-dirs=3 --no-parent --reject='index.html*' -q -P ",path_spex," https://public.spider.surfsara.nl/project/spexone/RemoTAP-SPEXone/",YYYY,MM,DD,"/"))

