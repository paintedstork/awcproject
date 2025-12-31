library(shiny)
library(dplyr)
library(DT)
library(lubridate)
library(stringr)
library(later)

query_env <- parseQueryString(Sys.getenv("QUERY_STRING", ""))
if (!is.null(query_env$action) && query_env$action == "run" && query_env$key == superSecretKey) {
  message("🚀 Headless trigger received before UI finishes sending 200 OK")
}


server <- function(input, output, session) {
  # Load configs & helpers
  
  authenticate_drive(drive_json)
  authenticate_form_sheets(drive_json)
  india <- read_india_names()
  
  # Reactive data processing
  data_ready <- reactiveVal(NULL)
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (is.null(query$action))
    {
      t_start <- Sys.time()
      if (readsummaries) {
        message("📖 Reading summaries from AWCSummaries.zip...")
        
        zip_info <- get_zip_file(folder_id, "^AWCSummaries.*\\.zip$")
        if (!is.null(zip_info))
        {
          req(zip_info)
        
          results <- read_summary_sheets(zip_info$file_path)
          data_ready(list(
            summary = results$summaries,
            main_raw = NULL,
            zip_created = zip_info$created_time,
            bird_count = results$main_summary$bird_count
          ))
        } else {
          readsummaries = FALSE
        }
        
      } 
      if (!readsummaries) 
      {
        message("☁️ Running full processing workflow from Drive + Form + ZIP...")
        
        zip_info <- get_zip_file(folder_id, "^project-report.*\\.zip$")
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
        
        main_summary <- prepare_main_summary(data_list$main, 
                                             default_start_date, 
                                             default_end_date)
        
        # Store summary + raw main data (lazy prepare later)
        data_ready(list(
          summary = qc_summary,
          main_raw = main_summary$data,
          zip_created = zip_info$created_time,
          bird_count = main_summary$bird_count
        ))  
      }
      t_end <- Sys.time()
      message("⏱️ Overall observe() time: ", round(difftime(t_end, t_start, units = "secs"), 2), " sec")
      # Hide overlay
      shinyjs::runjs("$('#loading-overlay').fadeOut(500);")
    }
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
    
    if (readsummaries) return(NULL)  # No main data in spreadsheet mode
    
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
  
  # --------------------------
  # Animated Dashboard Gauges
  # --------------------------
  
  vals <- reactiveVal(list(days = 0, wetlands = 0, birds = 0))
  
  
  # Function to animate a single gauge
  animate_gauge <- function(output_id, start_value, end_value, max, color, steps = 30, delay = 0.03) {
    step_values <- seq(start_value, end_value, length.out = steps)
    
    for (i in seq_along(step_values)) {
      local({
        value <- step_values[i]  # capture the current value in this iteration
        later::later(function() {
          output[[output_id]] <- renderGauge({
            gauge(
              value = round(value),   # explicitly provide the 'value'
              min = 0,
              max = max,
              sectors = gaugeSectors(success = c(0, max), colors = color)
            )
          })
        }, delay = (i - 1) * delay)
      })
    }
  }
  
  # Update values and animate when data is ready
  observeEvent(data_ready(), {
    today <- Sys.Date()
    current_day <- if (today < default_start_date) {
      0
    } else if (today > default_end_date) {
      as.integer(default_end_date - default_start_date) + 1
    } else {
      as.integer(today - default_start_date) + 1
    }
    
    wetlands_df <- data_ready()$summary$survey_completed
    wetlands_count <- if (!is.null(wetlands_df)) nrow(wetlands_df) else 0
    birds_count <- data_ready()$bird_count %||% 0
    
    vals(list(
      days = as.numeric(current_day),
      wetlands = as.numeric(wetlands_count),
      birds = as.numeric(birds_count)
    ))
    
    # Animate the gauges
    animate_gauge(
      "days_gauge", 0, current_day,
      max = as.integer(default_end_date - default_start_date) + 1,
      color = "#2E86C1"
    )
    animate_gauge(
      "wetlands_gauge", 0, wetlands_count,
      max = max(1000, wetlands_count),
      color = "#28B463"
    )
    animate_gauge(
      "birds_gauge", 0, birds_count,
      max = max(1000000, birds_count),
      color = "#F39C12"
    )
  })
  
  # Re-animate gauges when user switches back to Overview tab
  observeEvent(input$sidebar_tabs, {
    req(vals())
    if (input$sidebar_tabs == "dashboard") {
      v <- vals()
      animate_gauge(
        "days_gauge", 0, v$days,
        max = as.integer(default_end_date - default_start_date) + 1,
        color = "#2E86C1"
      )
      animate_gauge(
        "wetlands_gauge", 0, v$wetlands,
        max = max(1000, v$wetlands),
        color = "#28B463"
      )
      animate_gauge(
        "birds_gauge", 0, v$birds,
        max = max(1000000, v$birds),
        color = "#F39C12"
      )
    }
  })
  output$mode_info <- renderText({
    if (readsummaries)
      "Running in summaries mode (showing last processed data)."
    else
      "Running in live mode (processing latest eBird data from Drive)."
  })
  
}
