library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tidyr)

read_india_names <- function()
{
  return (read.csv("english_india.csv", stringsAsFactors = FALSE))
  
}

read_waterbird_list <- function()
{
  return(read.csv("waterbirds.csv", stringsAsFactors = FALSE))
}

# Read eBird data from zip
read_ebird_data <- function(zip_path, zip_file) {
  # Full path to the zip
  zip_full <- file.path(zip_path, zip_file)
  
  # Unzip into a unique temporary folder for this zip
  temp_unzip <- file.path(zip_path, paste0(tools::file_path_sans_ext(zip_file), "_unzipped"))
  dir.create(temp_unzip, showWarnings = FALSE)
  unzip(zip_full, exdir = temp_unzip)
  
  # List all subfolders inside the unzipped folder
  subfolders <- list.dirs(temp_unzip, full.names = TRUE, recursive = FALSE)
  
  # Detect folder with main + sampling report files
  correct_folder <- NULL
  main_file <- sampling_file <- NULL
  for(f in subfolders) {
    files <- list.files(f, full.names = TRUE)
    main_file_candidate     <- files[str_detect(files, "-report-[\\w_]+\\.txt$") & !str_detect(files, "sampling")]
    sampling_file_candidate <- files[str_detect(files, "sampling-report-[\\w_]+\\.txt$")]
    
    if(length(main_file_candidate) == 1 && length(sampling_file_candidate) == 1) {
      correct_folder <- f
      main_file <- main_file_candidate
      sampling_file <- sampling_file_candidate
      break
    }
  }
  
  if(is.null(correct_folder)) stop("Could not find main or sampling report file in the unzipped folder")
  
  # Columns to read
  main_cols <- c(
    "GLOBAL.UNIQUE.IDENTIFIER", "LAST.EDITED.DATE", "TAXONOMIC.ORDER", "CATEGORY",
    "COMMON.NAME", "SCIENTIFIC.NAME", "SUBSPECIES.SCIENTIFIC.NAME", "EXOTIC.CODE",
    "OBSERVATION.COUNT", "AGE.SEX", "COUNTRY", "COUNTRY.CODE", "STATE", "STATE.CODE",
    "COUNTY", "COUNTY.CODE", "IBA.CODE", "LOCALITY", "LOCALITY.ID", "LOCALITY.TYPE",
    "LATITUDE", "LONGITUDE", "OBSERVATION.DATE", "TIME.OBSERVATIONS.STARTED",
    "OBSERVER.ID", "SAMPLING.EVENT.IDENTIFIER", "OBSERVATION.TYPE", "PROTOCOL.NAME",
    "PROTOCOL.CODE", "PROJECT.NAMES", "PROJECT.IDENTIFIERS", "DURATION.MINUTES",
    "EFFORT.DISTANCE.KM", "EFFORT.AREA.HA", "NUMBER.OBSERVERS", "ALL.SPECIES.REPORTED",
    "GROUP.IDENTIFIER", "HAS.MEDIA", "APPROVED", "REVIEWED", "REASON",
    "CHECKLIST.COMMENTS", "SPECIES.COMMENTS"
  )
  
  sampling_cols <- c(
    "LAST.EDITED.DATE", "COUNTRY", "COUNTRY.CODE", "STATE", "STATE.CODE",
    "COUNTY", "COUNTY.CODE", "IBA.CODE", "BCR.CODE", "USFWS.CODE", "ATLAS.BLOCK",
    "LOCALITY", "LOCALITY.ID", "LOCALITY.TYPE", "LATITUDE", "LONGITUDE",
    "OBSERVATION.DATE", "TIME.OBSERVATIONS.STARTED", "OBSERVER.ID", "OBSERVER_ORCID_ID",
    "SAMPLING.EVENT.IDENTIFIER", "OBSERVATION.TYPE", "PROTOCOL.NAME", "PROTOCOL.CODE",
    "PROJECT.NAMES", "PROJECT.IDENTIFIERS", "DURATION.MINUTES", "EFFORT.DISTANCE.KM",
    "EFFORT.AREA.HA", "NUMBER.OBSERVERS", "ALL.SPECIES.REPORTED", "GROUP.IDENTIFIER",
    "CHECKLIST.COMMENTS"
  )
  
  # Helper to read only required columns
  read_selected <- function(file, cols) {
    nms <- read.delim(file, nrows = 1, sep = "\t", header = TRUE, quote = "\"", stringsAsFactors = FALSE)
    col_classes <- ifelse(names(nms) %in% cols, NA, "NULL")
    
    read.delim(
      file,
      colClasses = col_classes,
      sep = "\t",
      header = TRUE,
      quote = "\"",
      stringsAsFactors = FALSE,
      na.strings = c("", " ", NA)
    )
  }
  
  # Read data
  main_data <- read_selected(main_file, main_cols) %>% 
    mutate(
      OBSERVATION.DATE = str_sub(OBSERVATION.DATE, 1, 10),
      OBSERVATION.DATE = lubridate::ymd(OBSERVATION.DATE)
    )
  
  sampling_data <- read_selected(sampling_file, sampling_cols) %>%
    mutate(
      OBSERVATION.DATE = str_sub(OBSERVATION.DATE, 1, 10),
      OBSERVATION.DATE = lubridate::ymd(OBSERVATION.DATE)
    )
  
  # Add GROUP.ID for sampling
  sampling_data <- sampling_data %>%
    mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))
  
  list(main = main_data, sampling = sampling_data)
}


generate_summary_tables <- function(sampling_data, form_data, start_date, end_date) {
  
  # ---- Step 1. Prepare ----
  samp <- sampling_data %>%
    mutate(
      OBSERVATION.DATE = ymd(OBSERVATION.DATE),
      is_recommended = OBSERVATION.DATE >= start_date & OBSERVATION.DATE <= end_date,
      List = paste0("<a href='https://ebird.org/checklist/", SAMPLING.EVENT.IDENTIFIER,
                    "' target='_blank'>", SAMPLING.EVENT.IDENTIFIER, "</a>"),
      Hotspot = ifelse(!is.na(LOCALITY.ID),
                             paste0("<a href='https://ebird.org/hotspot/", LOCALITY.ID,
                                    "' target='_blank'>", LOCALITY, "</a>"),
                             LOCALITY),
      event_num = as.numeric(str_remove(SAMPLING.EVENT.IDENTIFIER, "^S"))
    )
  
  # ---- Step 1b: Join form data to mark 'Form Submitted' ----
  samp <- samp %>%
    # Extract only the first checklist ID from form_data$List
    mutate(Form_List_ID = SAMPLING.EVENT.IDENTIFIER %in% form_data$List) %>%
    # Add a column to mark "Form Submitted" if it exists in form_data
    mutate(Form_Submitted = if_else(Form_List_ID, "Yes", "No"))
  
  # ---- Step 2. Deduplicate by GROUP.ID ----
  dedup <- samp %>%
    group_by(GROUP.ID) %>%
    arrange(
      desc(ALL.SPECIES.REPORTED),  # prefer complete
      desc(is_recommended),        # prefer within window
      desc(LOCALITY.TYPE == "H"),  # prefer hotspot
      event_num                    # prefer smaller numeric part
    ) %>%
    slice(1) %>%
    ungroup()
  
  # ---- Step 3. Summaries use deduplicated data ----
  # Recommended wetlands
  wetlands_recommended <- dedup %>%
    filter(is_recommended, LOCALITY.TYPE == "H", ALL.SPECIES.REPORTED == 1) %>%
    group_by(LOCALITY.ID, Hotspot, COUNTY, STATE) %>%
    summarise(
      List = paste(List, collapse = ", "),
      Form = if_else(any(Form_Submitted == "Yes"), "Yes", "No"),
      .groups = "drop"
    ) %>%
    rename(Wetland = Hotspot, District = COUNTY, State = STATE) %>%
    arrange(State, District, Wetland)
  
  # ---- No Hotspot Lists (grouped + ordered) ----
  no_hotspot_lists <- dedup %>%
    filter(is.na(LOCALITY.ID) | LOCALITY.TYPE != "H") %>%
    group_by(LOCALITY.ID, LOCALITY, COUNTY, STATE) %>%
    summarise(
      List = paste(List, collapse = ", "),
      Recommended_Dates = if_else(any(is_recommended), "Yes", "No"),
      .groups = "drop"
    ) %>%
    rename(Locality = LOCALITY, District = COUNTY, State = STATE) %>%
    arrange(State, District, Locality) %>%
    select(Locality, District, State, List, Recommended_Dates) 
  
  # ---- Incomplete Lists (grouped + ordered) ----
  incomplete_lists <- dedup %>%
    filter(ALL.SPECIES.REPORTED == 0) %>%
    group_by(LOCALITY.ID, LOCALITY, COUNTY, STATE) %>%
    summarise(
      List = paste(List, collapse = ", "),
      Recommended_Dates = if_else(any(is_recommended), "Yes", "No"),
      .groups = "drop"
    ) %>%
    rename(Wetland = LOCALITY, District = COUNTY, State = STATE) %>%
    arrange(State, District, Wetland) %>%
    select(Wetland, District, State, List, Recommended_Dates)
  
  visited_within <- wetlands_recommended %>% select(LOCALITY.ID)
  
  covered_earlier <- dedup %>%
    filter(OBSERVATION.DATE < start_date & OBSERVATION.DATE >= as.Date(paste0(year(end_date)-1, "-12-01")),
           LOCALITY.TYPE == "H", 
           ALL.SPECIES.REPORTED == 1) %>%
    anti_join(visited_within, by = "LOCALITY.ID") %>%
    group_by(LOCALITY.ID, Hotspot, COUNTY, STATE) %>%
    summarise(
      List = paste(List, collapse = ", "),
      Form = if_else(any(Form_Submitted == "Yes"), "Yes", "No"),
      .groups = "drop"
    ) %>%
    rename(Wetland = Hotspot, District = COUNTY, State = STATE) %>%
    arrange(State, District, Wetland)
  
  covered_later <- dedup %>%
    filter(
      OBSERVATION.DATE > end_date & OBSERVATION.DATE <= as.Date(paste0(year(end_date), "-02-28")),
      LOCALITY.TYPE == "H",
      ALL.SPECIES.REPORTED == 1
    ) %>%
    anti_join(wetlands_recommended %>% select(LOCALITY.ID), by = "LOCALITY.ID") %>%
    group_by(LOCALITY.ID, Hotspot, COUNTY, STATE) %>%
    summarise(
      List = paste(List, collapse = ", "),
      Form = if_else(any(Form_Submitted == "Yes"), "Yes", "No"),
      .groups = "drop"
    ) %>%
    rename(Wetland = Hotspot, District = COUNTY, State = STATE) %>%
    arrange(State, District, Wetland)
  
  summary_table <- dedup %>%
    group_by(STATE, COUNTY) %>%
    summarise(
      `Covered (Recommended Dates)` = n_distinct(LOCALITY.ID[is_recommended]),
      `Covered (Earlier)` = n_distinct(LOCALITY.ID[
        OBSERVATION.DATE < start_date &
          OBSERVATION.DATE >= as.Date(paste0(year(end_date) - 1, "-12-01"))
      ]),
      `Covered (Later)` = n_distinct(LOCALITY.ID[
        OBSERVATION.DATE > end_date &
          OBSERVATION.DATE <= as.Date(paste0(year(end_date), "-02-28"))
      ]),
      `No Hotspot Lists` = n_distinct(SAMPLING.EVENT.IDENTIFIER[is.na(LOCALITY.ID) | LOCALITY.TYPE != "H"]),
      `Form Submitted` = sum(Form_Submitted == "Yes", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(State = STATE, District = COUNTY)
  
  list(
    summary = summary_table,
    survey_completed = wetlands_recommended %>% select(Wetland, District, State, List, Form),
    covered_earlier = covered_earlier %>% select(Wetland, District, State, List, Form),
    covered_later = covered_later %>% select(Wetland, District, State, List, Form),
    no_hotspot_lists = no_hotspot_lists %>% select(List, Locality, District, State, Recommended_Dates),
    incomplete_lists = incomplete_lists %>% select(List, Wetland, District, State, Recommended_Dates)
  )
}

# --------------------------------------------------
# Species summary helpers
# --------------------------------------------------

# Sensitive species table with region and date ranges
sensitive_species_df <- tibble::tribble(
  ~SCIENTIFIC.NAME,        ~STATE,                         ~START.DATE, ~END.DATE,
  "Antigone antigone",      "Chhattisgarh",                 NA,          NA,      # year-round
  "Vanellus gregarius",     "Gujarat",            "11-01",     "03-31",
  "Vanellus gregarius",     "Rajasthan",            "11-01",     "03-31",
  "Rynchops albicollis",    NA,                    "02-01",     "07-31",
  "Sterna acuticauda",      NA,                     "02-01",     "06-30",
  "Bubo bengalensis",       NA,                     NA,          NA,
  "Buceros bicornis",       "Kerala", NA, NA,
  "Buceros bicornis",       "Karnataka", NA, NA,
  "Buceros bicornis",       "Tamil Nadu", NA, NA,
  "Buceros bicornis",       "Maharashtra", NA, NA,
  "Buceros bicornis",       "Goa", NA, NA,
  "Chlamydotis macqueenii", "Gujarat",            NA,          NA,
  "Chlamydotis macqueenii", "Rajasthan",            NA,          NA,
  "Falco cherrug",          NA,                     NA,          NA,
  "Psittacula derbiana",    NA,                     NA,          NA,
  "Ardeotis nigriceps",     NA,                     NA,          NA,
  "Sypheotides indicus",    NA,                     NA,          NA,
  "Ploceus megarhynchus",   NA,                     NA,          NA,
  "Sporaeginthus formosus", NA,                     NA,          NA,
  "Emberiza aureola",       NA,                     NA,          NA
)

filter_sensitive_species <- function(data) {
  if (nrow(data) == 0) return(data)
  
  data <- data %>%
    mutate(
      STATE = trimws(STATE),
      OBSERVATION.DATE = as.Date(OBSERVATION.DATE),
      OBS_MONTH_DAY = format(OBSERVATION.DATE, "%m-%d")
    )
  
  # Split data into sensitive vs others
  sens_data <- data %>%
    filter(SCIENTIFIC.NAME %in% sensitive_species_df$SCIENTIFIC.NAME)
  non_sens_data <- data %>%
    filter(!(SCIENTIFIC.NAME %in% sensitive_species_df$SCIENTIFIC.NAME))
  
  if (nrow(sens_data) == 0) return(data)  # no sensitive species present
  
  # Join in state/date info (will repeat species rows for multiple state entries)
  sens_joined <- left_join(
    sens_data,
    sensitive_species_df,
    by = "SCIENTIFIC.NAME",
    relationship = "many-to-many"
  ) 
  # Determine which records to remove
  sens_filtered <- sens_joined %>%
    mutate(
      # State match: NA means all India, otherwise exact state match
      STATE_MATCH = is.na(STATE.y) | toupper(STATE.x) == toupper(STATE.y),
      
      # Handle cross-year date ranges (like Nov → Mar)
      cross_year = !is.na(START.DATE) & !is.na(END.DATE) & START.DATE > END.DATE,
      
      DATE_MATCH = case_when(
        is.na(START.DATE) | is.na(END.DATE) ~ TRUE,
        cross_year ~ (OBS_MONTH_DAY >= START.DATE | OBS_MONTH_DAY <= END.DATE),
        TRUE ~ (OBS_MONTH_DAY >= START.DATE & OBS_MONTH_DAY <= END.DATE)
      ),
      
      REMOVE = STATE_MATCH & DATE_MATCH
    )
  
  # Summarize removal per record (if any row for a species-state combination triggers REMOVE)
  to_remove <- sens_filtered %>%
    group_by(SCIENTIFIC.NAME, STATE.x, OBSERVATION.DATE) %>%
    summarise(REMOVE = any(REMOVE), .groups = "drop") %>%
    filter(REMOVE)
  
  # Keep only records not removed
  cleaned <- sens_data %>%
    anti_join(to_remove, by = c("SCIENTIFIC.NAME", "STATE" = "STATE.x", "OBSERVATION.DATE")) %>%
    bind_rows(non_sens_data)
  
  message(
    "Filtered ", nrow(to_remove),
    " sensitive records out of ", nrow(data)
  )
  
  return(cleaned)
}



# 1. Prepare and filter main data for analysis
prepare_main_data <- function(main_data, start_date, end_date) {
  
  cleaned <- main_data %>%
    # Keep only hotspot lists, complete lists, approved
    filter(
      LOCALITY.TYPE == "H",
      ALL.SPECIES.REPORTED == 1,
      APPROVED == 1,
      !(EXOTIC.CODE %in% c("X", "P")),
      OBSERVATION.DATE >= start_date & OBSERVATION.DATE <= end_date
    ) %>%
    # Remove OBSERVATION.COUNT marked as "X"
    mutate(OBSERVATION.COUNT = as.numeric(ifelse(OBSERVATION.COUNT == "X", NA, OBSERVATION.COUNT))) %>%
    # Deduplicate using GROUP.ID (like sampling)
    mutate(GROUP.ID = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
    group_by(GROUP.ID, SCIENTIFIC.NAME) %>%
    slice(1) %>%
    ungroup() %>%
    # Call sensitive species filter stub
    filter_sensitive_species()
  
  return(cleaned)
}

# --- 2. Filter main data by state/district ---
filter_main_data <- function(main_data, state = NULL, district = NULL) {
  filtered <- main_data
  if (!is.null(state) && nzchar(state)) filtered <- filtered %>% filter(STATE == state)
  if (!is.null(district) && nzchar(district)) filtered <- filtered %>% filter(COUNTY == district)
  return(filtered)
}

# --- 3. Species summary table ---
species_summary_table <- function(main_data, india_com_names) {
  
  if (nrow(main_data) == 0) return(data.frame(Message = "No data available for this selection."))
  
  # --- Create TAXONOMIC.ORDER table ---
  taxa_order_table <- main_data %>%
    select(SCIENTIFIC.NAME, COMMON.NAME, TAXONOMIC.ORDER) %>%
    distinct()
  
  # --- Deduplicate per site before region-wise aggregation ---
  dedup_data <- main_data %>%
    # ensure OBSERVATION.COUNT is numeric and valid
    mutate(OBSERVATION.COUNT = suppressWarnings(as.numeric(OBSERVATION.COUNT))) %>%
    group_by(LOCALITY.ID, SCIENTIFIC.NAME) %>%
    summarise(
      OBSERVATION.COUNT = if (all(is.na(OBSERVATION.COUNT))) NA_real_
      else max(OBSERVATION.COUNT, na.rm = TRUE),
      STATE = first(STATE),
      COUNTY = first(COUNTY),
      LOCALITY = first(LOCALITY),
      .groups = "drop"
    )  
  # --- Determine view level ---
  n_states <- n_distinct(dedup_data$STATE)
  n_districts <- n_distinct(dedup_data$COUNTY)
  
  if (n_states > 1) {
    # Species × State
    summary_df <- dedup_data %>%
      group_by(SCIENTIFIC.NAME, STATE) %>%
      summarise(Count = sum(OBSERVATION.COUNT, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = STATE, values_from = Count, values_fill = 0)
    
  } else if (n_districts > 1) {
    # Species × District
    summary_df <- dedup_data %>%
      group_by(SCIENTIFIC.NAME, COUNTY) %>%
      summarise(Count = sum(OBSERVATION.COUNT, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = COUNTY, values_from = Count, values_fill = 0)
    
  } else {
    # Species × Wetland
    summary_df <- dedup_data %>%
      group_by(SCIENTIFIC.NAME, LOCALITY) %>%
      summarise(Count = sum(OBSERVATION.COUNT, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = LOCALITY, values_from = Count, values_fill = 0)
  }  
  # --- Join with TAXONOMIC.ORDER + COMMON.NAME ---
  summary_df <- summary_df %>%
    left_join(taxa_order_table, by = "SCIENTIFIC.NAME")  # join to get order and original common name
  
  lang_replacements <- c(
    "Gray" = "Grey",
    "gray" = "grey",
    "Color" = "Colour",
    "color" = "colour"
  )
  
  # --- Override COMMON.NAME with REVISED_ALTERNATE_COM_NAME if provided ---
  if (!is.null(india_com_names)) {
    summary_df <- summary_df %>%
      left_join(india_com_names, by = c("SCIENTIFIC.NAME" = "sci_name")) %>%
      mutate(COMMON.NAME = ifelse(!is.na(REVISED_ALTERNATE_COM_NAME),
                                  REVISED_ALTERNATE_COM_NAME,
                                  COMMON.NAME),
             COMMON.NAME = str_replace_all(COMMON.NAME, lang_replacements)) %>%
      select(-REVISED_ALTERNATE_COM_NAME)
  }  
  
  # --- Sort by TAXONOMIC.ORDER and reorder columns ---
  summary_df <- summary_df %>%
    arrange(TAXONOMIC.ORDER) %>%
    select(COMMON.NAME, everything(), -TAXONOMIC.ORDER, -SCIENTIFIC.NAME) %>%
    distinct()                           # remove exact duplicate rows
    
  return(summary_df)
}

prepare_main_summary <- function(main_data, start_date, end_date) {
  cleaned <- prepare_main_data(main_data, start_date, end_date)
  
  waterbirds <- read_waterbird_list()
  india_com_names <- read_india_names()
  
  waterbird_data <- cleaned %>%
    inner_join(waterbirds, by = c("SCIENTIFIC.NAME" = "Name"))
  
  dedup_summary <- species_summary_table(waterbird_data, india_com_names)

  numeric_cols <- dedup_summary %>%
    select(where(is.numeric)) %>%
    as.matrix()
  
  bird_count <- sum(numeric_cols, na.rm = TRUE)  
  
  # ---- NEW: Additional metrics for new dashboard dials ----
  species_count  <- n_distinct(waterbird_data$SCIENTIFIC.NAME)
  district_count <- n_distinct(waterbird_data$COUNTY)
  state_count    <- n_distinct(waterbird_data$STATE)
  
  list(
    data = cleaned, # full data for raw download
    bird_count = bird_count,
    species_count = species_count, # total unique waterbird species
    district_count = district_count, # total districts with data
    state_count = state_count      # total states/UTs with data
  )
}
