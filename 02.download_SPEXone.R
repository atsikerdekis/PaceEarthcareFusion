#################
### LIBRARIES ###
#################
# Parse the timestamp to Date
date     <- as.Date(paste0(YYYY,MM,DD), "%Y%m%d")
date_str <- format(date, "%d-%b-%Y")  # e.g., "11-Jan-2025"
year_str <- format(date, "%Y")

# Construct the URL
base_url <- "https://oceandata.sci.gsfc.nasa.gov/directdataaccess/Level-2/PACE-SPEXONE"
full_url <- sprintf("%s/%s/%s/", base_url, year_str, date_str)

# Try reading the directory page
message("Accessing directory: ", full_url)
tryCatch({
  page <- read_html(full_url)
  file_links <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    grep("\\.nc$", ., value = TRUE)
  
  if (length(file_links) == 0) {
    message("No files found in directory.")
    return(NULL)
  }

  # Keep only RTAP files for land (LD) and ocean (OC)
  download_url <- file_links[grepl("RTAP", file_links)]
  
  # Prepare and set destination
  dir.create(paste0(path_spex,YYYY,MM,DD))
  dest_path <- paste0(path_spex,YYYY,MM,DD,"/",basename(download_url))

  # Download each file
  for (n in seq_along(download_url)) {
    #message("Downloading from   : ", download_url[n])
    #message("Downloading to     : ", dest_path[n])
    cmd <- paste0(
      "wget -c -nv ",
      "--load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies ",
      "--keep-session-cookies --auth-no-challenge=on ",
      "--user-agent='Wget/1.21.3 (linux-gnu)' ",
      "-O ", shQuote(dest_path[n]), " ", shQuote(download_url[n])
    )
    system(cmd)
  }
  
  return(dest_path)
  
}, error = function(e) {
  message("Error accessing or downloading: ", e$message)
  return(NULL)
})

