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
  
  # --- Split reactive values ---
  summary_data_ready <- reactiveVal(NULL)   # fast summaries, drives all tabs except species summary
  main_data_ready    <- reactiveVal(NULL)   # full EBird main data, used only by species summary
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (is.null(query$action)) {
      t_start <- Sys.time()
      
      # --- Step 1: Load summaries immediately ---
      if (readsummaries) {
        message("📖 Reading summaries from AWCSummaries.zip...")
        zip_info <- get_zip_file(folder_id, "^AWCSummaries.*\\.zip$")
        if (!is.null(zip_info)) {
          req(zip_info)
          results <- read_summary_sheets(zip_info$file_path)
          
          summary_data_ready(list(
            summary     = results$summaries,
            bird_count  = results$main_summary$bird_count,
            zip_created = zip_info$created_time
          ))
          
          shinyjs::runjs("$('#loading-overlay').fadeOut(500);")
          
        } else {
          message("⚠️ No AWCSummaries.zip found — switching to live mode.")
          readsummaries <<- FALSE
        }
      }
      
      # --- Step 2: Live mode summaries ---
      if (!readsummaries) {
        message("☁️ Running full processing workflow (live mode for summaries)...")
        
        zip_info <- get_zip_file(folder_id, "^project-report.*\\.zip$")
        req(zip_info)
        zip_path <- dirname(zip_info$file_path)
        zip_file <- basename(zip_info$file_path)
        
        data_list <- read_ebird_data(zip_path, zip_file)
        form_data <- get_form_data(sheet_url)
        message("✅ Form data loaded with ", nrow(form_data), " entries.")
        
        qc_summary <- generate_summary_tables(
          data_list$sampling, form_data,
          default_start_date, default_end_date
        )
        
        main_summary <- prepare_main_summary(
          data_list$main, default_start_date, default_end_date
        )
        
        summary_data_ready(list(
          summary     = qc_summary,
          bird_count  = main_summary$bird_count,
          zip_created = zip_info$created_time
        ))
        
        shinyjs::runjs("$('#loading-overlay').fadeOut(500);")
      }
      
      t_end <- Sys.time()
      message("⏱️ Summary observe() time: ", round(difftime(t_end, t_start, units = "secs"), 2), " sec")
    }
  })
  
  # --------------------------
  # Render QC summary outputs
  # --------------------------
  output$summary_table <- renderDT({
    req(summary_data_ready())
    datatable(summary_data_ready()$summary$summary,
              rownames = FALSE,
              options = list(pageLength = 100))
  })
  
  output$survey_completed <- renderDT({
    req(summary_data_ready())
    datatable(summary_data_ready()$summary$survey_completed,
              escape = FALSE,
              rownames = FALSE,
              options = list(pageLength = 100))
  })
  
  output$covered_earlier <- renderDT({
    req(summary_data_ready())
    datatable(summary_data_ready()$summary$covered_earlier,
              escape = FALSE,
              rownames = FALSE,
              options = list(pageLength = 100))
  })
  
  output$no_hotspot_lists <- renderDT({
    req(summary_data_ready())
    datatable(summary_data_ready()$summary$no_hotspot_lists,
              escape = FALSE,
              rownames = FALSE,
              options = list(pageLength = 100))
  })
  
  output$incomplete_lists <- renderDT({
    req(summary_data_ready())
    datatable(summary_data_ready()$summary$incomplete_lists,
              escape = FALSE,
              rownames = FALSE,
              options = list(pageLength = 100))
  })
  
  output$zip_date <- renderText({
    req(summary_data_ready()$zip_created)
    format(with_tz(summary_data_ready()$zip_created, tzone = "Asia/Kolkata"),
           "%d %B %Y, %H:%M IST")
  })
  
  
  # --------------------------
  # Species Summary Tab Logic (Fully Fixed)
  # --------------------------
  
  # Lazy main data for species summary
  main_data_lazy <- reactiveVal(NULL)
  
  # Track if species summary is loading
  species_loading <- reactiveVal(TRUE)
  
  # Prepare main data lazily — only when tab opened or main_data_ready() changes
  observeEvent(main_data_ready(), {
    if (input$sidebar_tabs == "species_summary") {
      message("⏳ Preparing main data for species summary...")
      prepared <- prepare_main_data(
        main_data_ready(),
        default_start_date,
        default_end_date
      )
      message("✅ Main data ready: ", nrow(prepared), " rows.")
      main_data_lazy(prepared)
      species_loading(FALSE)
    }
  })
  
  # --- Step 3: Load main_data_ready ONLY when species_summary tab is opened ---
  observeEvent(input$sidebar_tabs, {
    if (input$sidebar_tabs == "species_summary" && is.null(main_data_ready())) {
      message("⏳ Loading main data for species summary...")
      species_loading(TRUE)   # << START loading
      
      zip_info2 <- get_zip_file(folder_id, "^project-report.*\\.zip$")
      req(zip_info2)
      zip_path <- dirname(zip_info2$file_path)
      zip_file <- basename(zip_info2$file_path)
      
      data_list <- read_ebird_data(zip_path, zip_file)
      main_summary <- prepare_main_summary(
        data_list$main, default_start_date, default_end_date
      )
      
      main_data_ready(main_summary$data)
      message("✅ Species summary main data ready: ", nrow(main_summary$data), " rows.")
      species_loading(FALSE)  # << DONE loading
    }
  })  
  
  # --- Update State dropdown dynamically ---
  observe({
    req(main_data_lazy())
    states <- sort(unique(main_data_lazy()$STATE))
    updateSelectInput(session, "state_select",
                      choices = c("All", states),
                      selected = "All")
  })
  
  # --- Update District dropdown dynamically ---
  observeEvent(input$state_select, {
    req(main_data_lazy())
    if (is.null(input$state_select) || input$state_select == "All") {
      districts <- sort(unique(main_data_lazy()$COUNTY))
    } else {
      districts <- main_data_lazy() %>%
        filter(STATE == input$state_select) %>%
        pull(COUNTY) %>% unique() %>% sort()
    }
    updateSelectInput(session, "district_select",
                      choices = c("All", districts),
                      selected = "All")
  })
  
  # Disable/enable filters based on loading
  observe({
    shinyjs::toggleState("state_select", !species_loading())
    shinyjs::toggleState("district_select", !species_loading())
  })
  
  # --- Reactive: filter data based on selections ---
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
  
  # --- Reactive: compute species summary ---
  species_summary <- reactive({
    req(main_filtered())
    species_summary_table(main_filtered(), india)
  })
  
  # --- Species Summary Table ---
  output$species_table <- renderDT({
    message("🔁 [species_table] renderDT triggered")
    
    # Guard: main data not ready
    # Compute summary safely
    df_sum <- NULL
    tryCatch({
      message("🔧 [species_table] Computing species_summary_table() ...")
      df_sum <- species_summary()
      message(paste0("✅ [species_table] species_summary_table() complete — rows: ",
                     ifelse(is.null(df_sum), 0, nrow(df_sum))))
    }, error = function(e) {
      message("❌ [species_table] ERROR in species_summary_table(): ", e$message)
      df_sum <<- NULL
    })
    
    # Guard: no data available
    if (is.null(df_sum) || nrow(df_sum) == 0) {
      message("⚠️ [species_table] No data available — showing warning table")
      return(datatable(
        data.frame(Message = "⚠️ No data available for this selection."),
        rownames = FALSE, options = list(dom = 't', paging = FALSE)
      ))
    }
    

    # Render DT
    dt_obj <- datatable(
      df_sum,
      rownames = FALSE,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        lengthMenu = list(c(50, 500, -1), c('50', '500', 'All'))
      )
    )
    
    # Force repaint in hidden tabs
    shinyjs::runjs("
    setTimeout(function() {
      var t = $('#species_table table.dataTable').DataTable();
      if (t) {
        console.log('🧩 [species_table] Force redraw for hidden tab');
        t.clear().draw();
      }
    }, 400);
  ")
    
    return(dt_obj)
  })
  
  # --- Force DT redraw when user switches back to Species Summary tab ---
  observeEvent(input$sidebar_tabs, {
    if (input$sidebar_tabs == 'species_summary' && !is.null(main_data_lazy()) && nrow(main_data_lazy()) > 0) {
      message('🪄 [species_table] Tab visible — redrawing DT')
      shinyjs::runjs("
      setTimeout(function() {
        var t = $('#species_table table.dataTable').DataTable();
        if (t) {
          console.log('🪄 [species_table] Adjusting columns + drawing.');
          t.columns.adjust().draw();
        }
      }, 300);
    ")
    }
  })
  
  # ZIP download handler — cross-platform, main data only for filtered selection
  output$download_species_summary <- downloadHandler(
    filename = function() {
      state <- ifelse(is.null(input$state_select) || input$state_select == "All",
                      "India", gsub("[^A-Za-z0-9_]", "_", input$state_select))
      district <- ifelse(is.null(input$district_select) || input$district_select == "All",
                         "", gsub("[^A-Za-z0-9_]", "_", input$district_select))
      zip_name <- paste0("Species_Summary_", state, "_", district, ".zip")
      print(paste0("Download filename: ", zip_name))
      zip_name
    },
    
    content = function(file) {
      req(species_summary())
      
      temp_dir <- tempdir()
      print(paste0("Temporary directory: ", temp_dir))
      
      # --- Always write the summary CSV ---
      summary_file <- file.path(temp_dir, "species_summary.csv")
      write.csv(species_summary(), summary_file, row.names = FALSE)
      files_to_zip <- summary_file
      print(paste0("Summary CSV written: ", summary_file))
      
      # --- Include main data only if filtered ---
      include_main <- (!is.null(input$state_select) && input$state_select != "All") ||
        (!is.null(input$district_select) && input$district_select != "All")
      print(paste0("State selected: ", input$state_select))
      print(paste0("District selected: ", input$district_select))
      print(paste0("Include main? ", include_main))
      
      # --- Clean old main CSV if it exists ---
      old_main_file <- file.path(temp_dir, "main_data_filtered.csv")
      if (file.exists(old_main_file)) {
        file.remove(old_main_file)
      }
      
      if (include_main) {
        main_data_to_include <- main_data_lazy()
        if (is.null(main_data_to_include)) {
          print("Warning: main_data_lazy() is NULL — cannot include main data.")
        } else {
          print(paste0("Rows in main_data_lazy before filtering: ", nrow(main_data_to_include)))
          
          # Filter by state if applicable
          if (!is.null(input$state_select) && input$state_select != "All") {
            main_data_to_include <- main_data_to_include %>% filter(STATE == input$state_select)
            print(paste0("Rows after state filter: ", nrow(main_data_to_include)))
          }
          
          # Filter by district if applicable
          if (!is.null(input$district_select) && input$district_select != "All") {
            main_data_to_include <- main_data_to_include %>% filter(COUNTY == input$district_select)
            print(paste0("Rows after district filter: ", nrow(main_data_to_include)))
          }
          
          if (nrow(main_data_to_include) > 0) {
            main_file <- file.path(temp_dir, "main_data_filtered.csv")
            write.csv(main_data_to_include, main_file, row.names = FALSE)
            files_to_zip <- c(files_to_zip, main_file)
            print(paste0("Main data CSV written: ", main_file))
          } else {
            print("No rows after filtering — main data will not be included.")
          }
        }
      } else {
        print("No specific state/district selected — main data skipped.")
      }
      
      print(paste("Files to include in ZIP:", paste(files_to_zip, collapse = ", ")))
      
      # --- Safe zip filename ---
      safe_zip_file <- file.path(temp_dir, paste0("SpeciesSummary_", format(Sys.Date(), "%Y%m%d"), ".zip"))
      print(paste0("Safe ZIP file path: ", safe_zip_file))
      
      # --- Cross-platform ZIP ---
      if (.Platform$OS.type == "windows") {
        print("Using system zip (Windows) with -j flag")
        system2("zip", args = c("-j", shQuote(safe_zip_file), shQuote(files_to_zip)))
      } else {
        print("Using base R zip (Linux/macOS)")
        zip(zipfile = safe_zip_file, files = files_to_zip)
      }
      
      # --- Copy to Shiny download location ---
      success <- file.copy(safe_zip_file, file, overwrite = TRUE)
      print(paste0("Copied ZIP to Shiny download location: ", file, " | Success? ", success))
      print(paste0("Download ready ✅"))
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
  observeEvent(summary_data_ready(), {
    today <- Sys.Date()
    current_day <- if (today < default_start_date) {
      0
    } else if (today > default_end_date) {
      as.integer(default_end_date - default_start_date) + 1
    } else {
      as.integer(today - default_start_date) + 1
    }
    
    wetlands_df <- summary_data_ready()$summary$survey_completed
    wetlands_count <- if (!is.null(wetlands_df)) nrow(wetlands_df) else 0
    birds_count <- summary_data_ready()$bird_count %||% 0
    
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
