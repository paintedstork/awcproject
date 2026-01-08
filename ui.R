# ui.R
source("config.R")
source("drivehelper.R")
source("sheethelper.R")
source("formhelper.R")
source("ebirdhelper.R")
source("headless.R")
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(flexdashboard)
library(shinycssloaders)

ui <- function(request) {
  query <- parseQueryString(request$QUERY_STRING)
  
  if (!is.null(query$action) && query$action == "run") {
    if (query$key == superSecretKey) {
      headlessProc()  # Run your processing first
      
      fluidPage(
        # Optional: a tiny message for debugging logs
        tags$head(tags$style("body { font-family: sans-serif; margin: 40px; }")),
        h3("✅ Processing  completed. Check server logs for progress.")
      )
    } else {
      fluidPage(
        tags$head(tags$style("body { font-family: sans-serif; margin: 40px; }")),
        h3("⚠️ Invalid secret key.")
      )
    }
  } else {
  dashboardPage(
    skin = "blue",  # we’ll override colors below
    dashboardHeader(
      title = "AWC 2026",
      tags$li(
        class = "dropdown",
        tags$div(
          style = "display: flex; align-items: center; justify-content: flex-end; gap: 10px; padding-right: 10px;",
          tags$img(src = "BCI.png", height = "30px", style = "margin-top: 10px;"),
          tags$img(src = "wi.jpg", height = "30px", style = "margin-top: 10px;"),
          tags$img(src = "iwc.jpg", height = "30px", style = "margin-top: 10px;"),
          tags$img(src = "bnhs.png", height = "30px", style = "margin-top: 10px;"),
          tags$img(src = "eBird.png", height = "30px", style = "margin-top: 10px;")
        )
      )
    ),
    
    dashboardSidebar(
      width = 250,
      tags$head(
        tags$style(HTML("
          /* Header and sidebar custom colors */
          .skin-blue .main-header .navbar {
            background-color: #93B791;
          }
          .skin-blue .main-header .logo {
            background-color: #93B791;
            color: white;
          }
          .skin-blue .main-sidebar {
            background-color: #93B791;
          }
          .skin-blue .sidebar-menu > li > a {
            color: white;
          }
          .skin-blue .sidebar-menu > li > a:hover {
            background-color: #1a252f;
            color: white;
          }
          /* Adjust body background */
          .content-wrapper, .right-side {
            background-color: #f8f9f4;
          }
          .circle-box {
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: flex-start; /* align items to top */
          }
          
          .circle-label {
            font-size: 18px;   /* slightly bigger */
            font-weight: normal;
            text-align: center;
            margin-top: -100px;
          }
          
          @media (max-width: 768px) {
          .circle-label {
            margin-top: -60px;
            font-size: 16px;
            }
          }
        "))
      ),
      sidebarMenu(
        id = "sidebar_tabs",
        menuItem("Overview", tabName = "dashboard", icon = icon("tachometer-alt")),
        menuItem("Survey Summary", tabName = "summary", icon = icon("table")),
        menuItem("Survey Completed", tabName = "survey_completed", icon = icon("check-circle")),
        menuItem("Covered Earlier", tabName = "covered_earlier", icon = icon("clock")),
        menuItem("No Hotspot Lists", tabName = "no_hotspot_lists", icon = icon("map-marker-alt")),
        menuItem("Incomplete Lists", tabName = "incomplete_lists", icon = icon("exclamation-triangle")),
        menuItem("Species Summary", tabName = "species_summary", icon = icon("dove")),
        menuItem("Covered Later", tabName = "covered_later", icon = icon("clock")),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    ),
    
    dashboardBody(
      useShinyjs(),   # <-- correct place      
      # Loading overlay
      # Responsive loading overlay
      
      tags$head(
        tags$style(HTML("
    /* Ensure all DT tables scroll properly on small screens */
    .dataTables_wrapper {
      width: 100%;
      overflow-x: auto;
    }

    /* Optional: prevent table text from wrapping weirdly */
    table.dataTable {
      white-space: nowrap;
    }

    /* Adjust font and spacing for smaller screens */
    @media (max-width: 768px) {
      table.dataTable td {
        font-size: 12px;
        padding: 4px;
      }
      table.dataTable th {
        font-size: 12px;
        padding: 4px;
      }
    }
  "))
      ),
      # Auto-collapse sidebar after selecting a tab
      tags$script(HTML("
    $(document).on('click', '.sidebar-menu a', function() {
      if ($(window).width() < 768) {
        $('.main-sidebar').removeClass('sidebar-open');
      }
    });
  ")), 
      div(
        id = "loading-overlay",
        style = "
    position: fixed;
    top: 0;
    left: 0;
    width: 100vw;
    height: 100vh;
    background-color: rgba(255, 255, 255, 0.9);
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    z-index: 9999;
  ",
        
        # CSS for responsiveness
        tags$style(HTML("
    #loading-overlay img {
      max-width: 30vw; /* scales with viewport width */
      height: auto;
    }
    #loading-overlay p {
      font-size: 1.2rem;
      text-align: center;
      margin-top: 10px;
    }

    /* Tablet screens */
    @media (max-width: 992px) {
      #loading-overlay img {
        max-width: 60vw;
      }
      #loading-overlay p {
        font-size: 1.2rem;
      }
    }

    /* Mobile phones */
    @media (max-width: 600px) {
      #loading-overlay img {
        max-width: 85vw;
      }
      #loading-overlay p {
        font-size: 1.1rem;
      }
    }
  ")),
        img(src = "loading.jpg"),
        p("Loading data, please wait...")
      ),
      tabItems(
        tabItem(tabName = "dashboard",
                div(
                  align = "center",
                  h2(HTML("Asian Waterbird Census<br>03–18 January 2026")),
                  br(),
                  h3(textOutput("region"), style = "color: #2E4053; font-weight: normal;")
                ),
                br(),
                fluidRow(
                  column(4, div(class = "circle-box",
                                gaugeOutput("days_gauge", width = "200px", height = "200px"),
                                div(class = "circle-label", "Days"))),
                  column(4, div(class = "circle-box",
                                gaugeOutput("wetlands_gauge", width = "200px", height = "200px"),
                                div(class = "circle-label", "Wetlands"))),
                  column(4, div(class = "circle-box",
                                gaugeOutput("birds_gauge", width = "200px", height = "200px"),
                                div(class = "circle-label", "Waterbirds"))),
                  column(4, div(class = "circle-box",
                                gaugeOutput("states_gauge", width = "200px", height = "200px"),
                                div(class = "circle-label", "States/UTs"))),
                  column(4, div(class = "circle-box",
                                gaugeOutput("districts_gauge", width = "200px", height = "200px"),
                                div(class = "circle-label", "Districts"))),
                  column(4, div(class = "circle-box",
                                gaugeOutput("species_gauge", width = "200px", height = "200px"),
                                div(class = "circle-label", "Waterbird Species")))
                ),
                br(), br()
        ),
        
        tabItem(tabName = "summary",
                h3("Wetland Coverage Summary"),
                br(),
                DTOutput("summary_table")
        ),
        
        tabItem(tabName = "survey_completed",
                h3("Wetlands Covered (Recommended Dates)"),
                br(),
                DTOutput("survey_completed")
        ),
        
        tabItem(tabName = "covered_earlier",
                h3("Wetlands Covered Earlier (Outside Recommended Dates)"),
                br(),
                DTOutput("covered_earlier")
        ),

        tabItem(tabName = "covered_later",
                h3("Wetlands Covered Later (Outside Recommended Dates)"),
                br(),
                DTOutput("covered_later")
        ),
        
        tabItem(tabName = "no_hotspot_lists",
                h3("Checklists without Hotspots"),
                br(),
                DTOutput("no_hotspot_lists")
        ),
        
        tabItem(tabName = "incomplete_lists",
                h3("Checklists Marked as Incomplete"),
                br(),
                DTOutput("incomplete_lists")
        ),
        # ---- About Page ----
        tabItem(tabName = "about",
                h3("About the Asian Waterbird Census Dashboard"),
                br(),
                tags$div(
                  style = "line-height: 1.6; font-size: 16px; max-width: 900px;",
                  
                  # Project link and title
                  tags$b("Bird Count India Tools: "),
                  tags$a("Asian Waterbird Census (India) Project", href = "https://ebird.org/projects/1051", target = "_blank"),
                  tags$p("The Asian Waterbird Census (AWC): Dashboard for eBird AWC Coordinators and Counters."),
                  
                  # Last dataset update line
                  tags$p(
                    style = "font-size: 14px; color: #444;",
                    tags$b("Last dataset update: "),
                    textOutput("zip_date", inline = TRUE)
                  ),
                  
                  tags$ul(
                    tags$li("The Asian Waterbird Census is conducted through eBird using the project 'Asian Waterbird Census (India)'."),
                    tags$li("Recommended survey dates are 03 January to 18 January 2026."),
                    tags$li("In the absence of counts from the recommended period, counts from any dates between December and January are also accepted."),
                    tags$li("The ‘Survey Completed’ tab indicates wetlands where counts are already completed."),
                    tags$li("The ‘Covered Earlier’ tab shows wetlands covered before the recommended dates, which can be revisited during the recommended period."),
                    tags$li("The ‘Covered Later’ tab shows wetlands covered after the recommended dates."),
                    tags$li("AWC counts are accepted only from hotspots. Lists under ‘No Hotspot Lists’ indicate that hotspot editors need to approve the user-suggested hotspots or contact observers regarding hotspot creation."),
                    tags$li("The ‘Species Summary’ tab is based only on lists submitted within the recommended dates following the AWC protocols."),
                    tags$li("If more than one list is submitted for a hotspot, the species summary includes one amongst the several lists. In that sense, the summary is tentative and can change"),
                    tags$li("The ‘Species Summary’ tab is based only on lists submitted within the recommended dates following AWC protocols."),
                    tags$li("Records of sensitive species are not displayed."),
                    tags$li("Raw data for a state or district can be downloaded along with the summary. For the full country, only the summary can be downloaded."),
                    tags$li("Incomplete lists are not accepted as AWC counts. Observers may be notified to review such submissions."),
                    tags$li( tags$a( "For more details, visit Bird Count India AWC page.", href = "https://birdcount.in/event/awc/", target = "_blank"))
                  ),
                  
                  tags$hr(),
                  tags$b("Change Log:"),
                  tags$ul(
                    tags$li("Added better support for small devices, added region name - 06 January 2025"),
                    tags$li("Added region specific dashboard - 05 January 2025"),
                    tags$li("Added covered later, three new overview dials, recommended dates column - 05 January 2025"),
                    tags$li("Fixed loading of species summary - 03 January 2026. Known bugs: 1. . Download of country data has a stale datafile."),
                    tags$li("Optimized load time - 31 December 2025"),
                    tags$li("Added Animated Dashboard - 29 December 2025"),
                    tags$li("Added Species Summary - 23 December 2025"),
                    tags$li("Added Google Form submission status - 21 December 2025"),
                    tags$li("Summarized hotspots and incomplete lists by location; added this About page - 18 December 2025")
                  ),
                  br(),
                  tags$p(style = "font-size: 14px; color: #555;",
                         "Developed under Bird Count India’s data analysis initiatives."),
                  tags$a("Source Code", href = "https://github.com/paintedstork/awcproject/", target = "_blank")
                )
      ),
        # ---- Species Summary ----
        tabItem(tabName = "species_summary",
                h3("Species Summary by Region (Recommended Dates)"),
                br(),
                
                # Mobile-friendly layout fix
                tags$head(
                  tags$style(HTML("
                    @media (max-width: 768px) {
                      .form-group {
                        width: 100% !important;
                      }
                    }
                  "))
                ),
                # Filters
                fluidRow(
                  column(4,
                         selectInput("state_select", "Select State:", choices = NULL, selected = NULL)
                  ),
                  column(4,
                         selectInput("district_select", "Select District:", choices = NULL, selected = NULL)
                  ),
                  column(4,
                         br(),
                         downloadButton("download_species_summary", "Download ZIP")
                  )
                ),
                
                br(),
                withSpinner(DTOutput("species_table"), type = 6)
        )
      )
    )
  )
  }
}
