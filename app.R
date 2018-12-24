library(shiny)
library(shinydashboard)

library(jsonlite)
library(tidyverse)
# library(leaflet)
library(stringr)

# -----------------
# Load data

requirements <- fromJSON("data/countries.json", flatten = T)
requirements <- as_tibble(requirements)
requirements <- requirements %>%
  select(name.common, name.official, cca2, independent, status, currency, capital, region, subregion, demonym)

path <- "data/visas"
filenames <- dir(path = path, pattern="*.json", full.names = TRUE)
filenames[0:5]

visas <- filenames %>%
  map_df(function(x) {
    item <- fromJSON(x)
    parent <- "parent" %in% names(item)
    parent <- if (parent) item$parent else NA
    tibble(cca2 = item$cca2, csvc = item$csvc, requirements = list(item$requirements), parent = parent)
  })

data <- left_join(requirements, visas, by = "cca2")

# -----------------
# Define UI for application

header <- dashboardHeader(
  title = "World visas"
)

body <- dashboardBody(
  fluidRow(
    column(width = 3,
           box(width = NULL,
               textInput("country", "Country:",
                         placeholder = "Name or code. e.g. Colombia or CO")
           )
    ),
    column(width = 9,
           box(width = NULL,
               uiOutput("visasTable")
           )
    )
  ) # fluidRow
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

# -----------------
# Define server logic

code_to_name <- function(code) {
  return( (data %>% filter(csvc == code))$name.common )
}

server <- function(input, output) {
  
  output$visasTable <- renderUI({
    countries <- NA
    
    if (input$country == "") {
      # Show empty message
      
      p(
        class = "text-muted",
        "Enter a country name or code on the left"
      )
    }
    else {
      # Show table
      
      # Find country
      country_name <- str_to_title(input$country)
      country_code <- str_to_upper(input$country)
      country_reqs <- NULL
      if (country_name %in% data$name.common) {
        country <- data %>% filter(name.common == country_name)
        country_reqs <- country$requirements[[1]]
      }
      else if (country_code %in% data$cca2) {
        country <- data %>% filter(cca2 == country_code)
        country_reqs <- country$requirements[[1]]
      }
      
      # Render
      if (is.null(country_reqs)) {
        p(
          class = "text-muted",
          paste("Country \"", input$country, "\" not found.", sep = "")
        )
      }
      else {
        rows <- vector("list", length(country_reqs))

        for (i in seq_along(country_reqs)) {
          this_country <- country_reqs[i]
          country_code <- names(this_country)
          country_name <- code_to_name(country_code)
          if (length(country_name) == 0 || country_name == "") {
            next
          }
          
          permission <- get(country_code, this_country)$permission
          
          rows[[i]] <- tags$tr(
            tags$td(country_name),
            tags$td(permission)
          )
        }
        
        tags$table( class = "table",
                    tags$thead(
                      tags$tr(
                        tags$th("Country"),
                        tags$th("Requires visa")
                      )),
                      tags$tbody(
                        rows
                      )
        )
      }
    }
  }) # renderUI
}

# Run the application 
shinyApp(ui = ui, server = server)

