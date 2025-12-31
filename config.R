# config.R
source("secrets.R")
# Google Drive service account JSON path
drive_json <- "awc-shiny-9a53124c8a7f.json"

# Google Drive folder ID
folder_id <- "1qLLsCKQjZZtdKwaYqUtmQXE8HyUNLO1g"
sheet_url <- "https://docs.google.com/spreadsheets/d/1G7seWkbGY6jhopc0WrQfMf9oItM-7fOiJkCxI4GkKjY"

# Default date filters for the QC
default_start_date <- as.Date("2026-01-03")
default_end_date   <- as.Date("2026-01-18")
superSecretKey <- triggerKey

# Path to the ZIP (can be fixed or configurable)
summary_zip_name <- "AWCSummaries.zip"
readsummaries <- TRUE

