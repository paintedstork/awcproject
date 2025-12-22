library(shiny)
library(dplyr)
library(DT)
library(lubridate)
library(stringr)

server <- function(input, output, session) {
  # Load configs & helpers
  source("config.R")
  source("drivehelper.R")
  source("formhelper.R")
  source("ebirdhelper.R")
  
  authenticate_drive(drive_json)
  authenticate_sheets(drive_json)
  india <- read_india_names()
  
  # Reactive data processing
  data_ready <- reactiveVal(NULL)
  
  observe({
    zip_info <- get_zip_file(folder_id)
    req(zip_info)
    
    zip_path <- dirname(zip_info$file_path)
    zip_file <- basename(zip_info$file_path)
    
    data_list <- read_ebird_data(zip_path, zip_file)
    
    form_data <- get_form_data(sheet_url)
    message("✅ Form data loaded with ", nrow(form_data), " entries.")
    
    qc_summary <- generate_summary_tables(
      data_list$sampling,
      form_data,
      default_start_date,
      default_end_date
    )
    
    # Store summary + raw main data (lazy prepare later)
    data_ready(list(
      summary = qc_summary,
      main_raw = data_list$main,
      zip_created  = zip_info$created_time
    ))
  })
  
  # --------------------------
  # Render QC summary outputs
  # --------------------------
  output$summary_table <- renderDT({
    req(data_ready())
    datatable(
      data_ready()$summary$summary,
      rownames = FALSE,
      options = list(pageLength = 100)
    )
  })
  
  output$survey_completed <- renderDT({
    req(data_ready())
    datatable(
      data_ready()$summary$survey_completed,
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 100)
    )
  })
  
  output$covered_earlier <- renderDT({
    req(data_ready())
    datatable(
      data_ready()$summary$covered_earlier,
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 100)
    )
  })
  
  output$no_hotspot_lists <- renderDT({
    req(data_ready())
    datatable(
      data_ready()$summary$no_hotspot_lists,
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 100)
    )
  })
  
  output$zip_date <- renderText({
    req(data_ready()$zip_created)
    format(
      with_tz(data_ready()$zip_created, tzone = "Asia/Kolkata"),
      "%d %B %Y, %H:%M IST"
    )
  })
  
  output$incomplete_lists <- renderDT({
    req(data_ready())
    datatable(
      data_ready()$summary$incomplete_lists,
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 100)
    )
  })
  
  # --------------------------
  # Species Summary Tab Logic
  # --------------------------
  
  # Prepare main data lazily — only when tab opened
  main_data_lazy <- reactive({
    req(data_ready()$main_raw)
    
    # Trigger only when the "species_summary" tab is active
    if (!is.null(input$sidebar_tabs) && input$sidebar_tabs == "species_summary") {
      message("⏳ Preparing main data for species summary...")
      prepared <- prepare_main_data(data_ready()$main_raw, default_start_date, default_end_date)
      message("✅ Main data ready: ", nrow(prepared), " rows.")
      return(prepared)
    }
    NULL
  })
  
  # Update State dropdown
  observe({
    req(main_data_lazy())
    states <- sort(unique(main_data_lazy()$STATE))
    updateSelectInput(session, "state_select",
                      choices = c("All", states),
                      selected = "All")
  })
  
  # Update District dropdown when state changes
  observeEvent(input$state_select, {
    req(main_data_lazy())
    if (is.null(input$state_select) || input$state_select == "All") {
      districts <- sort(unique(main_data_lazy()$COUNTY))
    } else {
      districts <- main_data_lazy() %>%
        filter(STATE == input$state_select) %>%
        pull(COUNTY) %>%
        unique() %>%
        sort()
    }
    updateSelectInput(session, "district_select",
                      choices = c("All", districts),
                      selected = "All")
  })
  
  # Reactive: filter data based on selections
  main_filtered <- reactive({
    req(main_data_lazy())
    data <- main_data_lazy()
    
    if (!is.null(input$state_select) && input$state_select != "All") {
      data <- data %>% filter(STATE == input$state_select)
    }
    
    if (!is.null(input$district_select) && input$district_select != "All") {
      data <- data %>% filter(COUNTY == input$district_select)
    }
    
    data
  })
  
  # Reactive: compute summary
  species_summary <- reactive({
    req(main_filtered())
    species_summary_table(main_filtered(), india)
  })
  
  # Render the summary table
  output$species_table <- renderDT({
    req(species_summary())
    datatable(
      species_summary(),
      rownames = FALSE,
      options = list(pageLength = 50, scrollX = TRUE,
                     lengthMenu = list(c(50, 500, -1), c("50", "500", "All")))
    )
  })
  
  # ZIP download handler
  output$download_species_summary <- downloadHandler(
    filename = function() {
      state <- ifelse(is.null(input$state_select) || input$state_select == "All",
                      "India", input$state_select)
      district <- ifelse(is.null(input$district_select) || input$district_select == "All",
                         "", input$district_select)
      paste0("Species_Summary_", state, "_", district, ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      
      # Summary table CSV
      summary_file <- file.path(temp_dir, "species_summary.csv")
      write.csv(species_summary(), summary_file, row.names = FALSE)
      
      # Filtered main data (only for state/district)
      if ((!is.null(input$state_select) && input$state_select != "All") ||
          (!is.null(input$district_select) && input$district_select != "All")) {
        main_file <- file.path(temp_dir, "main_data_filtered.csv")
        write.csv(main_filtered(), main_file, row.names = FALSE)
        files_to_zip <- c(summary_file, main_file)
      } else {
        files_to_zip <- summary_file
      }
      
      zip(zipfile = file, files = files_to_zip, flags = "-j")
    }
  )
}
