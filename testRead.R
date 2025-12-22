# test_read_full.R
# Test all functions from the eBird helper file, including sensitive species

library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

# ---- Source helpers ----
source("config.R")        # Contains default_start_date, default_end_date, etc.
source("ebirdhelper.R")   # All helper functions

# ---- 1. Specify local zip paths ----
zip_path <- "C:/Users/91990/Downloads"  #C:/Users/91990/AppData/Local/Temp/RtmpCQnQfQ/
zip_file <- "project-report-434e6bef-6c52-4600-9e05-0d3075e23745.zip"  #file153c767b76ec.zip
unzip_folder <- file.path(zip_path, "unzipped_test")

if(!file.exists(file.path(zip_path, zip_file))) stop("Zip file not found at: ", zip_path)

# ---- 2. Read eBird data ----
data_list <- read_ebird_data(zip_path, zip_file)

cat("Main data rows:", nrow(data_list$main), "\n")
cat("Sampling data rows:", nrow(data_list$sampling), "\n\n")

# ---- 3. Prepare main data (clean + remove exotic + sensitive species) ----
main_clean <- prepare_main_data(data_list$main)
cat("After preparing main data rows:", nrow(main_clean), "\n\n")

# ---- 4. Filter main data by state/district (example) ----
main_filtered <- filter_main_data(main_clean, state = "Kerala", district = "Thrissur")
cat("After filtering for Kerala/Thrissur:", nrow(main_filtered), "\n\n")

# ---- 5. Calculate QC summaries ----
qc <- generate_summary_tables(data_list$sampling, form_data = data.frame(List = character()), 
                              start_date = default_start_date, end_date = default_end_date)
cat("Survey Completed:", nrow(qc$survey_completed), "\n")
cat("Covered Earlier:", nrow(qc$covered_earlier), "\n")
cat("No Hotspot Lists:", nrow(qc$no_hotspot_lists), "\n")
cat("Incomplete Lists:", nrow(qc$incomplete_lists), "\n\n")

# ---- 6. Load India common names CSV (if available) ----
india_names <- read_india_names()   # expects english_india.csv in working dir

# ---- 7. Generate species summary tables ----
species_summary <- species_summary_table(main_clean, india_names)
cat("--- Species summary table preview ---\n")
print(head(species_summary))

# ---- 8. Generate summary tables for sampling & form data ----
# For testing, create dummy form_data if not using actual Google Form sheet
form_data <- data.frame(
  List = unique(data_list$sampling$SAMPLING.EVENT.IDENTIFIER),
  stringsAsFactors = FALSE
)

summary_tables <- generate_summary_tables(data_list$sampling, form_data,
                                          default_start_date, default_end_date)
cat("\n--- Summary tables preview ---\n")
cat("Survey Completed:", nrow(summary_tables$survey_completed), "\n")
cat("Covered Earlier:", nrow(summary_tables$covered_earlier), "\n")
cat("No Hotspot Lists:", nrow(summary_tables$no_hotspot_lists), "\n")
cat("Incomplete Lists:", nrow(summary_tables$incomplete_lists), "\n")
