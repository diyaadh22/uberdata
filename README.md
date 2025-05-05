# Uber Trips Analysis (April–September 2014)

This project presents an interactive **Shiny dashboard** analyzing over **4.5 million Uber pickup records** in New York City from **April to September 2014**. The dashboard was built using R and includes time-based visualizations, heatmaps, a geospatial Leaflet map, and a predictive model for ride volume.

---

##  Data Source

The dataset includes six months of Uber pickup data, each provided as a separate CSV file:

- `uber-raw-data-apr14.csv`
- `uber-raw-data-may14.csv`
- `uber-raw-data-jun14.csv`
- `uber-raw-data-jul14.csv`
- `uber-raw-data-aug14.csv`
- `uber-raw-data-sep14.csv`

Each file contains:
- Pickup date and time
- Latitude and longitude
- The Uber base/company associated with the ride

These files were combined and cleaned using R before being visualized in the Shiny app.

---

##  Data Preparation

Before building the dashboard, the following steps were taken:

###  Load and Bind CSVs

```r
file_list <- list.files(pattern = "uber-raw-data-.*14.csv")
uber_data <- file_list %>% map_df(~read_csv(.))
```

###  Convert and Extract Date Features

```r
uber_data$Date.Time <- mdy_hms(uber_data$Date.Time)

uber_data <- uber_data %>%
  mutate(
    Hour = hour(Date.Time),
    Day = day(Date.Time),
    Month = month(Date.Time, label = TRUE),
    Weekday = wday(Date.Time, label = TRUE),
    Week = week(Date.Time)
  )
```

###  Create Summary Tables

```r
daily_table <- uber_data %>% group_by(Day) %>% summarise(Total = n())
trips_by_hour_month <- uber_data %>% count(Hour, Month)
trips_by_day_month <- uber_data %>% count(Weekday, Month)
trips_by_month <- uber_data %>% count(Month)
trips_by_base_month <- uber_data %>% count(Base, Month)
```

---

## Dashboard Features

The interactive Shiny application includes:

###  Time-Based Charts

- Trips by Hour (filtered by month)
- Trips Every Hour
- Trips by Day of the Month
- Trips by Day of Week and Month
- Trips by Month
- Trips by Base and Month

All visualizations are implemented using `ggplot2` and appear in the Shiny app UI.

---

##  Heatmaps

All heatmaps use `geom_bin2d()` to visualize frequency:

```r
ggplot(uber_data, aes(x = Hour, y = Weekday)) +
  geom_bin2d() +
  scale_fill_viridis_c()
```

Included:
- Hour vs Weekday
- Day vs Month
- Week vs Month
- Base vs Weekday

---

##  Geospatial Mapping

To display ride pickups spatially, we used the `leaflet` package with a random sample of 5,000 observations.

```r
subset_data <- uber_data[sample(nrow(uber_data), 5000), ]

leaflet(subset_data) %>%
  addTiles() %>%
  addCircleMarkers(~Lon, ~Lat, radius = 1, color = 'blue')
```

---

## Prediction Model

I created a linear regression model to estimate ride count using hour, weekday, and month:

```r
trip_summary <- uber_data %>%
  count(Hour, Weekday, Month) %>%
  mutate(Weekday = as.factor(Weekday), Month = as.factor(Month))

model <- lm(n ~ Hour + Weekday + Month, data = trip_summary)
```

Prediction is triggered inside the Shiny app using user inputs.

---

##  Shiny Application

The dashboard was built using:

- `shiny` for layout and interactivity
- `ggplot2` for all charts
- `leaflet` for mapping
- `dplyr`, `lubridate` for data manipulation
- `DT` for interactive data tables

---

##  Live App

👉 [Click here to launch the app](https://diya11.shinyapps.io/uber/)

