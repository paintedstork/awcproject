# drive_helper.R
library(googledrive)
library(dplyr)
library(stringr)

# --------------------------------------------------
# Configuration
# --------------------------------------------------
read_locally <- FALSE  # TRUE = read from local file, FALSE = use Google Drive
LOCAL_FILE <- "C:/Users/91990/AppData/Local/Temp/Rtmpo3tgiq/file1e3c2ae246a.zip"


# --------------------------------------------------
# Authentication
# --------------------------------------------------
authenticate_drive <- function(drive_json) {
  # Authenticate only if not in local mode
  if (!read_locally) {
    drive_auth(path = drive_json)
  } else {
    message("🔍 Running in local mode — skipping Google Drive authentication.")
  }
}

# --------------------------------------------------
# Get ZIP file
# --------------------------------------------------
get_zip_file <- function(folder_id) {
  if (read_locally) {
    message("📂 Reading local ZIP file: ", LOCAL_FILE)
    if (!file.exists(LOCAL_FILE)) stop("Local file not found: ", LOCAL_FILE)
    temp_zip <- LOCAL_FILE
    file_date <- file.info(LOCAL_FILE)$ctime  # creation date for local file
  } else {
    message("☁️ Fetching ZIP from Google Drive folder...")
    files <- drive_ls(as_id(folder_id)) %>%
      filter(str_detect(name, "\\.zip$"))
    
    if (nrow(files) == 0) {
      warning("⚠️ No ZIP file found in Google Drive folder.")
      return(NULL)
    }
    
    zip_file <- files[1, ]  # take the first ZIP
    temp_zip <- tempfile(fileext = ".zip")
    drive_download(zip_file$id, path = temp_zip, overwrite = TRUE)
    message("✅ File downloaded to: ", temp_zip)
    
    # Get Drive file creation time
    drive_resource <- zip_file$drive_resource[[1]]
    created_time <- as.POSIXct(drive_resource$createdTime,
                               format = "%Y-%m-%dT%H:%M:%OSZ",
                               tz = "UTC")
    file_date <- created_time
  }
  
  return(list(file_path = temp_zip, created_time = file_date))
}

