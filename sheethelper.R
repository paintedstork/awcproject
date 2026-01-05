# --------------------------------------------------
# sheethelper.R
# --------------------------------------------------
library(readxl)
library(googledrive)
library(dplyr)
library(stringr)
library(writexl)
library(zip)



# --------------------------------------------------
# Read summary sheets from extracted Excel
# --------------------------------------------------
read_summary_sheets <- function(zip_path) {
  # Unzip locally
  temp_dir <- tempdir()
  unzip(zip_path, exdir = temp_dir)
  excel_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
  
  if (length(excel_files) == 0) stop("No Excel file found in ZIP.")
  excel_file <- excel_files[1]
  message("📖 Reading Excel: ", excel_file)
  
  # Read all expected tabs safely
  read_if_exists <- function(sheet_name) {
    sheets <- excel_sheets(excel_file)
    if (sheet_name %in% sheets) {
      message("📖 Reading tab: ", sheet_name)
      read_excel(excel_file, sheet = sheet_name)
    } else {
      message("⚠️ Tab '", sheet_name, "' not found.")
      data.frame()
    }
  }
  
  summaries <- list(
    summary          = read_if_exists("Summary"),
    survey_completed = read_if_exists("Completed"),
    covered_earlier  = read_if_exists("Earlier"),
    covered_later    = read_if_exists("Later"),
    no_hotspot_lists = read_if_exists("No_Hotspot"),
    incomplete_lists = read_if_exists("Incomplete")
  )
  
  # Bird count
  overview_tab <- read_if_exists("Overview")

  bird_count     <- NA
  species_count  <- NA
  district_count <- NA
  state_count    <- NA
  
  overview_tab <- read_if_exists("Overview")
  if (nrow(overview_tab) > 0 && all(c("Metric", "Value") %in% names(overview_tab))) {
    overview_tab$Value <- suppressWarnings(as.numeric(overview_tab$Value))
  } else {
    overview_tab <- data.frame(Metric = character(), Value = numeric())
  }
  
  # Extract counts (if present)
  get_value <- function(metric) {
    val <- overview_tab$Value[overview_tab$Metric == metric]
    if (length(val) == 0) return(NA) else return(val)
  }

  bird_count     = get_value("Bird Count")
  species_count  = get_value("Species Count")
  district_count = get_value("District Count")
  state_count    = get_value("State Count")
  
  main_summary <- list(
    data = NULL,  # we don’t have species-level data here
    bird_count = bird_count,
    species_count = species_count,
    district_count = district_count,
    state_count = state_count
  )
  
  message("✅ Summary sheets successfully loaded.\n")
  list(
    summaries = summaries,
    main_summary = main_summary
  )
}

write_summary_sheets <- function(summaries, main_summary, output_file = "AWCSummaries.xlsx") {
  sheets <- list(
    Summary      = summaries$summary,
    Completed    = summaries$survey_completed,
    Earlier      = summaries$covered_earlier,
    Later        = summaries$covered_later,
    No_Hotspot   = summaries$no_hotspot_lists,
    Incomplete   = summaries$incomplete_lists,
    Overview = data.frame(
                 Metric = c("Bird Count", "Species Count", "District Count", "State Count"),
                 Value  = c(main_summary$bird_count,
                 main_summary$species_count,
                 main_summary$district_count,
                 main_summary$state_count)
    )
  )
  
  writexl::write_xlsx(sheets, path = output_file)
  message("✅ Summary Excel written to: ", output_file)
  return(normalizePath(output_file))
}

# --------------------------------------------------
# 2. Zip the Excel file
# --------------------------------------------------
zip_summary_file <- function(excel_file, zip_file = "AWCSummaries.zip") {
  zip::zipr(zip_file, files = excel_file)
  message("✅ Excel zipped to: ", zip_file)
  return(normalizePath(zip_file))
}

# --------------------------------------------------
# 3. Upload ZIP to Drive (optionally overwrite existing)
# --------------------------------------------------
upload_summary_zip <- function(zip_file, folder_id, zip_name = "AWCSummaries.zip") {
  # Check if a ZIP with the same name exists
  existing <- drive_ls(as_id(folder_id)) %>%
    filter(name == zip_name)
  
  if (nrow(existing) > 0) {
    # Overwrite
    drive_update(file = existing$id[1], media = zip_file)
    message("🔄 Existing ZIP updated on Drive: ", zip_name)
  } else {
    # Upload new
    drive_upload(zip_file, path = as_id(folder_id), name = zip_name)
    message("✅ ZIP uploaded to Drive: ", zip_name)
  }
}
