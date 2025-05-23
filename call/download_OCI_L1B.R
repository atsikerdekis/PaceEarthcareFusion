download_OCI_L1B <- function(timestamp, save_dir = ".") {
  # Libraries
  library(httr)
  library(rvest)
  library(lubridate)
  
  # Parse the timestamp to Date
  datetime <- ymd_hms(paste0(substr(timestamp, 1, 8), " ", substr(timestamp, 10, 11), ":", substr(timestamp, 12, 13), ":", substr(timestamp, 14, 15)))
  date_str <- format(datetime, "%d-%b-%Y")  # e.g., "11-Jan-2025"
  year_str <- format(datetime, "%Y")
  
  # Construct the URL
  base_url <- "https://oceandata.sci.gsfc.nasa.gov/directdataaccess/Level-1B/PACE-OCI"
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
    
    # Extract timestamps from filenames
    file_timestamps <- gsub(".*(\\d{8}T\\d{6}).*", "\\1", file_links)
    file_datetimes <- ymd_hms(gsub("T", "", file_timestamps))
    
    # Find closest timestamp
    target_time <- ymd_hms(gsub("T", "", timestamp))
    closest_idx <- which.min(abs(difftime(file_datetimes, target_time, units = "secs")))
    
    file_name <- file_links[closest_idx]
    download_url <- file_name
    dest_path <- paste0(save_dir, basename(file_name))
    
    # Download the file
    message("Downloading from   : ", download_url)
    message("Downloading to     : ", dest_path)
    #message(paste0("wget -c --progress=dot:mega --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --auth-no-challenge=on --user-agent='Wget/1.21.3 (linux-gnu)' -O ",dest_path," ",download_url))    
    #system(paste0("wget -c --progress=dot:mega --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --auth-no-challenge=on --user-agent='Wget/1.21.3 (linux-gnu)' -O ",dest_path," ",download_url))
    message(paste0("wget -c -nv --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --auth-no-challenge=on --user-agent='Wget/1.21.3 (linux-gnu)' -O ",dest_path," ",download_url))
    system(paste0("wget -c -nv --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --auth-no-challenge=on --user-agent='Wget/1.21.3 (linux-gnu)' -O ",dest_path," ",download_url)) 
    return(dest_path)
  }, error = function(e) {
    message("Error accessing or downloading: ", e$message)
    return(NULL)
  })
}

#test <- download_pace_file(timestamp="20250311T213004", save_dir="AIRSENSE/data/PACE/")


