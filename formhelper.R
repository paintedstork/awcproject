# --------------------------------------------------
# form_helper.R
# --------------------------------------------------
library(googlesheets4)
library(dplyr)
library(stringr)

# --------------------------------------------------
# Configuration
# --------------------------------------------------
read_local_sheet <- FALSE  # TRUE = read from local CSV if testing offline
LOCAL_FORM_FILE <- "data/form_data.csv"  # optional fallback file

# --------------------------------------------------
# Authentication
# --------------------------------------------------
authenticate_sheets <- function(service_json) {
  if (!read_local_sheet) {
    gs4_auth(path = service_json)
    message("✅ Google Sheets authentication successful.")
  } else {
    message("🔍 Running in local mode — skipping Sheets authentication.")
  }
}

# --------------------------------------------------
# Get Google Form Data
# --------------------------------------------------
get_form_data <- function(sheet_url) {
  if (read_local_sheet) {
    message("📂 Reading local form CSV: ", LOCAL_FORM_FILE)
    if (!file.exists(LOCAL_FORM_FILE)) stop("Local file not found: ", LOCAL_FILE)
    form_data <- read.csv(LOCAL_FORM_FILE, stringsAsFactors = FALSE)
  } else {
    message("☁️ Fetching form data from Google Sheet...")
    form_data <- read_sheet(sheet_url)
  }
  
  # Basic cleaning and normalization
  clean_data <- form_data %>%
    rename(
      Name = `Your name`,
      List = `Link to your eBird List`,
      Wetland = `Name of Wetland Site counted`,
      Parent_Site = `Name of Parent Site`,
      State = `Name of State in which the waterbird count was made`
    ) %>%
    mutate(across(everything(), ~str_trim(as.character(.)))) %>%
    
    # Extract first checklist ID (e.g., S288120441) from the List field
    mutate(
      List = str_extract(List, "S\\d+")
    ) %>%
    
    distinct()
  
  message("✅ Form data successfully loaded: ", nrow(clean_data), " rows.")
  return(clean_data)
}

