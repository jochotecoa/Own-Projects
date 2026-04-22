# Interactive Shiny Dashboard
# Note: Run this from the project root using: shiny::runApp("scripts/covid19_dashboard")

# Robustly source functions.R regardless of working directory
script_dir <- getwd()
if (basename(script_dir) == "covid19-dashboard") {
  source("../../utils/functions.R")
} else if (file.exists("scripts/utils/functions.R")) {
  source("scripts/utils/functions.R")
} else {
  # Fallback: simple library loader if functions.R can't be found
  forceLibrary <- function(pkgs) {
    for (p in pkgs) {
      if (!require(p, character.only = TRUE)) {
        install.packages(p)
        library(p, character.only = TRUE)
      }
    }
  }
}

forceLibrary(c("shiny", "dplyr", "ggplot2", "plotly"))

# Load data once at startup
print("Fetching full OWID data for the dashboard...")
owid_url <- "https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv"

# Wrapped in tryCatch for better error reporting
covid_data <- tryCatch({
  df <- read.csv(owid_url, stringsAsFactors = FALSE)
  
  # Handle column name differences in the new catalog
  if (!"location" %in% colnames(df) & "country" %in% colnames(df)) {
    df <- df %>% rename(location = country)
  }
  if (!"iso_code" %in% colnames(df) & "code" %in% colnames(df)) {
    df <- df %>% rename(iso_code = code)
  }
  
  df %>%
    mutate(date = as.Date(date)) %>%
    filter(!is.na(continent) & continent != "")
}, error = function(e) {
  stop("Failed to load data from OWID: ", e$message)
})

ui <- fluidPage(
  titlePanel("COVID-19 Interactive Explorer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Data source: Our World in Data"),
      selectInput("country", "Select Country:", 
                  choices = sort(unique(covid_data$location)), 
                  selected = "United States"),
      selectInput("metric", "Select Metric:", 
                  choices = c("New Cases Smoothed" = "new_cases_smoothed",
                              "New Deaths Smoothed" = "new_deaths_smoothed",
                              "Total Cases" = "total_cases",
                              "Total Vaccinations" = "total_vaccinations")),
      dateRangeInput("dates", "Date Range:",
                     start = max(min(covid_data$date, na.rm=TRUE), as.Date("2020-01-01")),
                     end = max(covid_data$date, na.rm=TRUE))
    ),
    mainPanel(
      plotlyOutput("trendPlot")
    )
  )
)

server <- function(input, output) {
  output$trendPlot <- renderPlotly({
    # Filter data based on input
    df_plot <- covid_data %>%
      filter(location == input$country,
             date >= input$dates[1],
             date <= input$dates[2])
    
    # Modern ggplot2 syntax for dynamic variables
    p <- ggplot(df_plot, aes(x = date, y = .data[[input$metric]])) +
      geom_line(color = "steelblue", linewidth = 1) +
      theme_minimal() +
      labs(title = paste(gsub("_", " ", input$metric), "in", input$country),
           x = "Date",
           y = input$metric)
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
