headlessProc <- function()
{
  authenticate_drive(drive_json)
  authenticate_form_sheets(drive_json)
  
  # Headless processing
  cat("🚀 Headless trigger detected — starting processing...\n")
  
  # --- Your five-step workflow here ---
  zip_info <- get_zip_file(folder_id, "^project-report.*\\.zip$")
  if (is.null(zip_info)) stop("No project ZIP found in Drive.")
  zip_path <- zip_info$file_path
  zip_dir  <- dirname(zip_path)
  zip_file <- basename(zip_path)
  
  form_data <- get_form_data(sheet_url)
  ebird <- read_ebird_data(zip_dir, zip_file)
  main <- ebird$main
  sampling <- ebird$sampling
  
  summaries <- generate_summary_tables(sampling, form_data, default_start_date, default_end_date)
  main_summary <- prepare_main_summary(main, default_start_date, default_end_date)
  
  excel_file <- write_summary_sheets(summaries, main_summary)
  zip_file_out <- zip_summary_file(excel_file)
  upload_summary_zip(zip_file_out, folder_id)
  
  cat("✅ Headless processing complete.\n")
  
}