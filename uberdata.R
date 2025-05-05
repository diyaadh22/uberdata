# Load packages
library(tidyverse)
library(lubridate)
library(leaflet)
library(shiny)
library(plotly)
library(DT)

# Step 1: Bind all CSVs
file_list <- list.files(pattern = "uber-raw-data-.*14.csv")
uber_data <- file_list %>% map_df(~read_csv(.))

# Step 2: Clean and convert datetime
colnames(uber_data) <- c("Date.Time", "Lat", "Lon", "Base")
uber_data$Date.Time <- mdy_hms(uber_data$Date.Time)

# Step 3: Extract time features
uber_data <- uber_data %>%
  mutate(
    Hour = hour(Date.Time),
    Day = day(Date.Time),
    Month = month(Date.Time, label = TRUE),
    Weekday = wday(Date.Time, label = TRUE),
    Week = week(Date.Time)
  )

# Pivot tables for various charts
daily_table <- uber_data %>% group_by(Day) %>% summarise(Total = n())
trips_by_hour_month <- uber_data %>% count(Hour, Month)
trips_by_day_month <- uber_data %>% count(Weekday, Month)
trips_by_month <- uber_data %>% count(Month)
trips_by_base_month <- uber_data %>% count(Base, Month)

# Prediction model data and training
trip_summary <- uber_data %>% count(Hour, Weekday, Month) %>%
  mutate(Weekday = as.factor(Weekday), Month = as.factor(Month))
model <- lm(n ~ Hour + Weekday + Month, data = trip_summary)

# Subset for Leaflet performance
subset_data <- uber_data[sample(nrow(uber_data), 5000), ]

# UI
ui <- fluidPage(
  titlePanel("Uber Trips Analysis - 2014"),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month", choices = unique(uber_data$Month)),
      sliderInput("hour", "Select Hour", min = 0, max = 23, value = 0),
      hr(),
      h4("Prediction Inputs"),
      selectInput("pred_month", "Prediction Month", choices = unique(trip_summary$Month)),
      selectInput("pred_weekday", "Prediction Weekday", choices = unique(trip_summary$Weekday)),
      sliderInput("pred_hour", "Prediction Hour", min = 0, max = 23, value = 10)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trips by Hour", plotOutput("hourPlot")),
        tabPanel("Trips Every Hour", plotOutput("tripsEveryHour")),
        tabPanel("Trips Table", DTOutput("tripTable")),
        tabPanel("Trips by Day and Month", plotOutput("dayMonthPlot")),
        tabPanel("Trips by Month", plotOutput("monthPlot")),
        tabPanel("Trips by Base and Month", plotOutput("baseMonthPlot")),
        tabPanel("Leaflet Map", leafletOutput("leafMap")),
        tabPanel("Model Prediction", verbatimTextOutput("modelOutput")),
        tabPanel("Heatmaps",
                 plotOutput("heatmap_hour_weekday"),
                 plotOutput("heatmap_day_month"),
                 plotOutput("heatmap_week_month"),
                 plotOutput("heatmap_base_weekday")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$hourPlot <- renderPlot({
    df <- uber_data %>% filter(Month == input$month)
    ggplot(df, aes(x = Hour)) +
      geom_bar(fill = "steelblue") +
      labs(title = paste("Trips by Hour in", input$month), x = "Hour", y = "Trips")
  })
  
  output$tripsEveryHour <- renderPlot({
    ggplot(uber_data, aes(x = Hour)) +
      geom_bar(fill = "darkgreen") +
      labs(title = "Trips Every Hour (Overall)", x = "Hour", y = "Trips")
  })
  
  output$tripTable <- renderDT({
    datatable(daily_table)
  })
  
  output$dayMonthPlot <- renderPlot({
    ggplot(trips_by_day_month, aes(x = Weekday, y = n, fill = Month)) +
      geom_col(position = "dodge") +
      labs(title = "Trips by Day of Week and Month", x = "Day of Week", y = "Trips")
  })
  
  output$monthPlot <- renderPlot({
    ggplot(trips_by_month, aes(x = Month, y = n, fill = Month)) +
      geom_col() +
      labs(title = "Trips by Month", x = "Month", y = "Trips")
  })
  
  output$baseMonthPlot <- renderPlot({
    ggplot(trips_by_base_month, aes(x = Base, y = n, fill = Month)) +
      geom_col(position = "dodge") +
      labs(title = "Trips by Base and Month", x = "Base", y = "Trips")
  })
  
  output$leafMap <- renderLeaflet({
    leaflet(subset_data) %>%
      addTiles() %>%
      addCircleMarkers(~Lon, ~Lat, radius = 1, color = 'blue', stroke = FALSE, fillOpacity = 0.4)
  })
  
  output$modelOutput <- renderPrint({
    input_data <- data.frame(
      Hour = input$pred_hour,
      Weekday = factor(input$pred_weekday, levels = levels(trip_summary$Weekday)),
      Month = factor(input$pred_month, levels = levels(trip_summary$Month))
    )
    predict(model, input_data)
  })
  
  output$heatmap_hour_weekday <- renderPlot({
    ggplot(uber_data, aes(x = Hour, y = Weekday)) +
      geom_bin2d() +
      scale_fill_viridis_c() +
      labs(title = "Heatmap: Hour vs Weekday")
  })
  
  output$heatmap_day_month <- renderPlot({
    ggplot(uber_data, aes(x = Day, y = Month)) +
      geom_bin2d() +
      scale_fill_viridis_c() +
      labs(title = "Heatmap: Day vs Month")
  })
  
  output$heatmap_week_month <- renderPlot({
    ggplot(uber_data, aes(x = Week, y = Month)) +
      geom_bin2d() +
      scale_fill_viridis_c() +
      labs(title = "Heatmap: Week vs Month")
  })
  
  output$heatmap_base_weekday <- renderPlot({
    ggplot(uber_data, aes(x = Base, y = Weekday)) +
      geom_bin2d() +
      scale_fill_viridis_c() +
      labs(title = "Heatmap: Base vs Weekday")
  })
}

# Launch app
shinyApp(ui, server)
