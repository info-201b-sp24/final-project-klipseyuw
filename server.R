library("RSocrata")
library(shiny)
library(dplyr)
library(tidyr)
library(sf)
library(plotly)
library(gridExtra)
library(grid)
library(ggplot2)
library(lubridate)

crime_data <-  read.socrata("https://data.seattle.gov/resource/tazs-3rd5.csv")

crime_data$offense_start_datetime <-
  as.Date(as.POSIXct(crime_data$offense_start_datetime), "%m/%d/%Y")
crime_data$offense_end_datetime <-
  as.Date(as.POSIXct(crime_data$offense_end_datetime), "%m/%d/%Y")
crime_data$report_datetime <-
  as.Date(as.POSIXct(crime_data$report_datetime), "%m/%d/%Y")

summary_table <- crime_data %>%
  mutate(Year = as.integer(format(
    as.Date(offense_start_datetime, "%m/%d/%Y"), "%Y"
  ))) %>%
  count(Year, offense) %>%
  pivot_wider(names_from = offense,
              values_from = n,
              values_fill = 0) %>%
  mutate(count = rowSums(across(where(is.numeric)))) %>%
  select(Year, count, everything()) %>%
  rename_all( ~ gsub("\\.", " ", .)) %>%
  select(Year, count, everything()) %>%
  select(1:2, order(colnames(.)))

summary_table_after_2008 <- function(data) {
  data %>%
    mutate(Year = as.integer(format(
      as.Date(offense_start_datetime, "%m/%d/%Y")
    )), "%Y") %>%
    filter(Year >= 2008) %>%
    group_by(Year) %>%
    summarise(count = n()) %>%
    arrange(Year)
}

crime_data$offense_start_datetime <-
  as.Date(crime_data$offense_start_datetime, format = "%m/%d/%Y")
crime_data$offense_end_datetime <-
  as.Date(crime_data$offense_end_datetime, format = "%m/%d/%Y")

map_data <- function(dataset, date_range, shapefile) {
  dataset %>%
    filter(offense_start_datetime >= date_range[1] &
             offense_start_datetime < date_range[2]) %>%
    group_by(beat) %>%
    summarise(total_crimes = n()) %>%
    mutate(prop_crimes = total_crimes / sum(total_crimes)) %>%
    inner_join(st_read(shapefile, quiet = TRUE), by = c('beat' = 'beat'))
}

dates_2008_2015 <-
  map_data(crime_data,
           c("2008-01-01", "2015-01-01"),
           "map/beat2008/Beats_2008_2015_WM.shp")
dates_2015_2017 <-
  map_data(crime_data,
           c("2015-01-01", "2017-01-01"),
           "map/beat2015/Beats_2015_2017_WM.shp")
dates_2017_2018 <-
  map_data(crime_data,
           c("2017-01-01", "2018-01-01"),
           "map/beats/Beats_WM.shp")

server <- function(input, output) {
  output$summary_table <- renderTable({
    summary_table_after_2008(crime_data)
  })
  
  output$crime_summary_table <- renderTable({
    summary_table
  })
  
  output$summary <- renderPrint({
    number_of_unique_offense <- crime_data %>%
      count(offense, name = "num_observations")
    total_crimes <- sum(number_of_unique_offense$n)
    
    most_frequent_offense <- crime_data %>%
      count(offense, name = "num_observations") %>%
      filter(num_observations == max(num_observations))
    
    highest_crime_location <- crime_data %>%
      count(mcpp, name = "num_observations") %>%
      filter(num_observations == max(num_observations))
    
    offense_categories <- crime_data %>%
      count(offense_parent_group, name = "num_observations") %>%
      filter(num_observations == max(num_observations))
    
    most_frequent_primary_offense <- crime_data %>%
      count(offense) %>%
      filter(n == max(n))
    
    year_most_crime_reported <- crime_data %>%
      mutate(Year = format(as.Date(report_datetime, format = "%m/%d/%Y"), "%Y")) %>%
      count(Year, name = "num_observations") %>%
      filter(num_observations == max(num_observations))
    
    cat("Total Number of Unique Offenses: ")
    cat(nrow(number_of_unique_offense), "\n\n")
    
    cat("Total Number of Crimes: ")
    cat(total_crimes, "\n\n")
    
    cat("Most Frequent Primary Offense: ")
    cat(most_frequent_primary_offense$offense, "\n\n")
    
    cat("Most Frequent Offense Categories: ")
    cat(offense_categories$offense_parent_group,
        "\n\n")
    
    cat("Precinct with the Highest Crime Count: ")
    cat(highest_crime_location$mcp, "\n\n")
    
    cat("Year Highest Reported Crimes: ")
    cat(year_most_crime_reported$Year, "\n")
  })
  
  
  location_table <- reactive({
    crime_data %>%
      filter(mcpp != "") %>%   
      filter(mcpp != "<Null>") %>%          
      filter(mcpp != "NULL") %>%   
      filter(mcpp != "UNKNOWN") %>%   
      group_by(mcpp) %>%    
      summarize(count = n())
  })
  
  
  output$bar_plot <- renderPlot({
    ggplot(location_table(), aes(x = reorder(mcpp, -count), y = count)) +
      geom_bar(stat = "identity", fill = "darkred") +
      xlab("Location") +
      ylab("Number of Offenses") +
      ggtitle("Number of Crimes Committted in Seattle By Location") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
  })
  
  
  
  
  
  
  map_plot <- reactive({
    years <- input$years
    plots <- list()
    
    if ("2008_2015" %in% years) {
      plot_earliest <- ggplot() +
        geom_sf(data = dates_2008_2015, aes(geometry = geometry, fill = prop_crimes)) +
        scale_fill_gradient(name = "Crime Density",
                            low = "pink",
                            high = "red") +
        theme_void() + ggtitle("2008-2015") + theme(plot.title = element_text(hjust = 0.6))
      plots <- append(plots, list(plot_earliest))
    }
    
    if ("2015_2017" %in% years) {
      plot_middle <- ggplot() +
        geom_sf(data = dates_2015_2017, aes(geometry = geometry, fill = prop_crimes)) +
        
        scale_fill_gradient(name = "Crime Density",
                            low = "pink",
                            high = "red") +
        theme_void() + ggtitle("2015-2017") + theme(plot.title = element_text(hjust = 0.6))
      plots <- append(plots, list(plot_middle))
    }
    
    if ("2017_2018" %in% years) {
      plot_latest <- ggplot() +
        geom_sf(data = dates_2017_2018, aes(geometry = geometry, fill = prop_crimes)) +
        
        scale_fill_gradient(name = "Crime Density",
                            low = "pink",
                            high = "red") +
        theme_void() + ggtitle("2017-2018") + theme(plot.title = element_text(hjust = 0.6))
      plots <- append(plots, list(plot_latest))
    }
    
    plot_sum <- arrangeGrob(grobs = plots, ncol = 2)
    title <-
      textGrob("Crime Per Map Beat", gp = gpar(fontface = "bold", fontsize = 18))
    final <- arrangeGrob(plot_sum, top = title)
  })
  
  output$beat_map <- renderPlot({
    grid.newpage()
    grid.draw(map_plot())
  })
  
  
  
  
  
  
  
  output$overall <- renderUI({
    selectInput(
      "type_of_offense",
      "Select Offense",
      choices = c("All", unique(crime_data$offense)),
      selected = "All"
    )
  })
  
  filtered_data <- reactive({
    if (is.null(input$type_of_offense) ||
        input$type_of_offense == "All") {
      return(crime_data)
    } else {
      return(crime_data %>%
               filter(offense %in% input$type_of_offense))
    }
  })
  
  output$offense_year <- renderPlotly({
    data <- filtered_data()
    
    crimes_per_year <- data %>%
      mutate(year = year(offense_start_datetime)) %>%
      group_by(year) %>%
      summarise(num_crimes = n()) %>%
      na.omit()
    
    
    reported_crimes_per_year <- data %>%
      mutate(year = year(report_datetime)) %>%
      group_by(year) %>%
      summarise(num_reported_crimes = n()) %>%
      na.omit()
    
    combine <-
      left_join(crimes_per_year, reported_crimes_per_year, by = "year")
    
    fig <-
      plot_ly(
        combine,
        x = ~ year,
        y = ~ num_crimes,
        name = "Offenses",
        type = 'scatter',
        mode = 'lines+markers'
      )
    fig <-
      fig %>% add_trace(y = ~ num_reported_crimes,
                        name = "Reported Offenses",
                        mode = 'lines+markers')
    fig <- fig %>%
      layout(
        title = "",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Offenses and Reported Offenses per Year",
                     tickformat = ",d"),
        hovermode = "x unified"
      )
    
    fig
  })
}