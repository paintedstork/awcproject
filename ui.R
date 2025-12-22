# ui.R
library(shiny)
library(shinydashboard)
library(DT)

ui <- function() {
  dashboardPage(
    skin = "blue",  # we’ll override colors below
    dashboardHeader(
      title = "AWC 2026",
      tags$li(
        class = "dropdown",
        tags$div(
          style = "display: flex; align-items: center; justify-content: flex-end; gap: 10px; padding-right: 10px;",
          tags$img(src = "BCI.png", height = "30px", style = "margin-top: 10px;"),
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
        "))
      ),
      sidebarMenu(
        id = "sidebar_tabs",
        menuItem("Wetland Coverage Summary", tabName = "summary", icon = icon("table")),
        menuItem("Survey Completed", tabName = "survey_completed", icon = icon("check-circle")),
        menuItem("Covered Earlier", tabName = "covered_earlier", icon = icon("clock")),
        menuItem("No Hotspot Lists", tabName = "no_hotspot_lists", icon = icon("map-marker-alt")),
        menuItem("Incomplete Lists", tabName = "incomplete_lists", icon = icon("exclamation-triangle")),
        menuItem("Species Summary", tabName = "species_summary", icon = icon("dove")),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    ),
    
    dashboardBody(
      tabItems(
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
                  tags$p("The Asian Waterbird Census (AWC): Dashboard for eBird AWC Project Regional Coordinators."),
                  
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
                    tags$li("AWC counts are accepted only from hotspots. Lists under ‘No Hotspot Lists’ indicate that hotspot editors need to approve the user-suggested hotspots or contact observers regarding hotspot creation."),
                    tags$li("The ‘Species Summary’ tab is based only on lists submitted within the recommended dates following AWC protocols."),
                    tags$li("The species summary includes only approved records. If your data is not visible, it may still be under review or may not follow AWC protocol (please check other tabs)."),
                    tags$li("Records of sensitive species are not displayed."),
                    tags$li("Raw data for a state or district can be downloaded along with the summary. For the full country, only the summary can be downloaded."),
                    tags$li("Incomplete lists are not accepted as AWC counts. Observers may be notified to review such submissions.")
                  ),
                  
                  tags$hr(),
                  tags$b("Change Log:"),
                  tags$ul(
                    tags$li("Added Species Summary - 23 December 2025"),
                    tags$li("Added Google Form submission status - 21 December 2025"),
                    tags$li("Summarized hotspots and incomplete lists by location; added this About page - 18 December 2025")
                  ),
                  br(),
                  tags$p(style = "font-size: 14px; color: #555;",
                         "Developed under Bird Count India’s data analysis initiatives.")
                )
      ),
        # ---- Species Summary ----
        tabItem(tabName = "species_summary",
                h3("Species Summary by Region (Recommended Dates)"),
                br(),
                
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
                DTOutput("species_table")
        )
      )
    )
  )
}
