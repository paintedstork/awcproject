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
zip_file <- "project-report-64d0d9e7-d7f9-407b-a064-77933cefa015.zip"  #file153c767b76ec.zip

if(!file.exists(file.path(zip_path, zip_file))) stop("Zip file not found at: ", zip_path)

# ---- 2. Read eBird data ----
data_list <- read_ebird_data(zip_path, zip_file)

data_list$main <- data_list$main %>% filter (STATE == 'Gujarat') %>% filter (COUNTY == "Kachchh")
data_list$sampling <- data_list$sampling %>% filter (STATE == 'Gujarat') %>% filter (COUNTY == "Kachchh")

cat("Main data rows:", nrow(data_list$main), "\n")
cat("Sampling data rows:", nrow(data_list$sampling), "\n\n")

# ---- 3. Prepare main data (clean + remove exotic + sensitive species) ----
main_clean <- prepare_main_data(data_list$main, default_start_date, default_end_date)
cat("After preparing main data rows:", nrow(main_clean), "\n\n")

# ---- 4. Filter main data by state/district (example) ----
main_filtered <- filter_main_data(main_clean, state = "Gujarat", district = "Kachchh")
cat("After filtering for Gujarat/Kachchh:", nrow(main_filtered), "\n\n")

# ---- 5. Calculate QC summaries ----
qc <- generate_summary_tables(data_list$sampling, form_data = data.frame(List = character()), 
                              start_date = default_start_date, end_date = default_end_date)
cat("Survey Completed:", nrow(qc$survey_completed), "\n")
cat("Covered Earlier:", nrow(qc$covered_earlier), "\n")
cat("Covered Later:", nrow(qc$covered_later), "\n")
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
cat("Covered Later:", nrow(qc$covered_later), "\n")
cat("No Hotspot Lists:", nrow(summary_tables$no_hotspot_lists), "\n")
cat("Incomplete Lists:", nrow(summary_tables$incomplete_lists), "\n")

# ---- 9. Verify QC summary counts vs individual QC tables ----

cat("\n--- Checking internal consistency within QC summary tables ---\n")

# helper: count sites per district/state in a table
count_sites <- function(tbl, label) {
  if (is.null(tbl) || nrow(tbl) == 0) return(data.frame(State=character(), District=character(), n=integer()))
  
  site_col <- intersect(c("Wetland", "District", "State"), names(tbl))
  if (length(site_col) == 0) return(data.frame(State=character(), District=character(), n=integer()))
  
  tbl %>%
    group_by(State, District) %>%
    summarise(n = n_distinct(.data[[site_col[1]]]), .groups = "drop") %>%
    mutate(Source = label)
}

# gather counts from each QC table
qc_counts <- bind_rows(
  count_sites(qc$survey_completed, "Covered (Recommended Dates)"),
  count_sites(qc$covered_earlier, "Covered (Earlier)"),
  count_sites(qc$covered_later, "Covered (Later)"),
  count_sites(qc$no_hotspot_lists, "No Hotspot Lists"),
  count_sites(qc$incomplete_lists, "Incomplete Lists")
)

# reshape for comparison
qc_counts_wide <- qc_counts %>%
  pivot_wider(names_from = Source, values_from = n, values_fill = 0)

# join with qc$summary to compare
qc_compare <- qc$summary %>%
  left_join(qc_counts_wide, by = c("State", "District"))

# compute differences
cols_to_check <- c("Covered (Recommended Dates)", "Covered (Earlier)", 
                   "Covered (Later)", "No Hotspot Lists", "Incomplete Lists")
for (col in cols_to_check) {
  if (col %in% names(qc_compare)) {
    qc_compare[[paste0(col, "_diff")]] <-
      qc_compare[[col]] - qc_compare[[paste0(col, ".y")]]
  }
}

# show mismatches only
qc_diff <- qc_compare %>%
  select(State, District, ends_with("_diff")) %>%
  filter(if_any(ends_with("_diff"), ~ . != 0))

if (nrow(qc_diff) == 0) {
  cat("✅ All counts in qc$summary match their individual component tables.\n")
} else {
  cat("⚠️  Mismatches found between qc$summary and component tables:\n")
  print(qc_diff)
}

cat("\n--- QC summary integrity check complete ---\n")
