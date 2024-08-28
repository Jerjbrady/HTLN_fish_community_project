install.packages("purrr")
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)
library(dataRetrieval)


# Define lists of gages
OZAR <- c("07064440", "07064533", "07066000", "07065495", "07065200", "07066510", "07068000", "07067500", "07066510")
BUFF <- c("07055646", "07055660", "07055680", "07055780", "07056000", "07056700", "07055790", "07055792", "07055875", "07056515")
PIPE <- c("06482430")
TAPR <- c("07180400", "07182200", "07182250", "07182260")


# Function to get thresholds based on park code
get_thresholds <- function(park_code) {
  if (park_code == "BUFF") {
    return(data.frame(
      site_no = c("07055646", "07055680", "07056000", "07056700"),
      threshold = c(8.79, 7.81, 11.11, 10.62)
    ))
  } else if (park_code == "OZAR") {
    return(data.frame(
      site_no = c("07064440", "07064533", "07067000", "07065200"),
      threshold = c(2, 4, 5, 4)
    ))
  } else if (park_code == "TAPR") {
    return(data.frame(
      site_no = TAPR,
      threshold = 32
    ))
  } else if (park_code == "PIPE") {
    return(data.frame(
      site_no = PIPE,
      threshold = 16
    ))
  }
  return(data.frame())
}

# User Interface
ui <- fluidPage(
  titlePanel("Flood Event Counter"),
  sidebarLayout(
    sidebarPanel(
      selectInput("park_input", "Select Park Code:", choices = c("OZAR", "BUFF", "PIPE", "TAPR")),
      dateInput("date", "Start Date:", value = Sys.Date()),
      numericInput("months", "Number of Months:", value = 1, min = 1, step = 1),
      actionButton("go", "Go")
    ),
    mainPanel(
      tableOutput("result_table"),
      plotOutput("gage_plot"),
      downloadButton("downloadData", "Download Table")
    )
  )
)

# Server logic
server <- function(input, output) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$go, {
    req(input$park_input, input$date, input$months)
    
    input_date <- as.Date(input$date)
    date_prior <- input_date %m-% months(input$months)
    
    start_date <- as.character(date_prior)
    end_date <- as.character(input_date)
    
    gages <- switch(input$park_input,
                    "OZAR" = OZAR,
                    "BUFF" = BUFF,
                    "PIPE" = PIPE,
                    "TAPR" = TAPR,
                    c())
    # Initialize an empty list to store data
    data_list_1 <- list()
    
    # Loop through each gage in the BUFF list
    for(gage in BUFF) {  
      rawDailyData_2 <- readNWISdata(
        siteNumber = gage,
        parameterCd = "00065",
        service = "uv",
        startDate = start_date,
        endDate = end_date
      ) %>%
        select(-1) %>%
        renameNWISColumns() %>%
        mutate(dateTime = parse_date_time(dateTime, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd")),
               Date = as.Date(dateTime))
      # Append the fetched data to the list
      data_list_1[[gage]] <- rawDailyData_2
    }
    
    gage_all_years_data <- do.call(rbind, data_list_1)
    
     average_data <- gage_all_years_data %>%
      filter(
        case_when(
          input$park_input == "BUFF" ~ site_no %in% c("07055646", "07055680", "07056000", "07056700"),
          input$park_input == "OZAR" ~ site_no %in% c("07064440", "07064533", "07067000", "07065200"),
          TRUE ~ TRUE  # No filtering for other inputs
        )
      ) %>%   
    group_by(site_no, Date) %>%
    summarise(average = mean(GH_Inst, na.rm = TRUE), .groups = 'drop')
    
    thresholds <- get_thresholds(input$park_input)
    
    find_flood_days <- average_data %>%
      left_join(thresholds, by = "site_no") %>%
      mutate(flood = ifelse(!is.na(threshold) & average >= threshold, TRUE, FALSE)) 
    
    flood_gages <- find_flood_days %>%
      group_by(site_no) %>%
      summarise(flood_count = sum(flood, na.rm = TRUE), .groups = 'drop')
    
    # Get maximum height for each site_no 
    height_table <- average_data %>%
      group_by(site_no) %>%
      summarise(Max_Height = max(average), .groups = 'drop')
      
      # Initialize an empty list to store data
      data_list <- list()
      
      # Loop through each gage in the BUFF list
      for(gage in gages) {
        # Fetch raw daily data for the current gage
        rawDailyData <- readNWISdv(
          siteNumber = gage,
          parameterCd = "00060",
          startDate = start_date,
          endDate = end_date
        )%>%
          select(-1) %>%
          renameNWISColumns()
        
        # Append the fetched data to the list
        data_list[[gage]] <- rawDailyData
      }
      
      # Combine all data into a single dataframe
      combined_df <- do.call(rbind, data_list)
      
      discharge <- combined_df %>%
        group_by(site_no) %>%
        slice(which.max(Flow)) %>%
        mutate(
          Date = as.Date(Date), # Ensure Date is in proper Date format
          Year = year(Date)
        ) %>%
        select(site_no, Flow, Year) %>% # Include Year in the selected columns
        ungroup()
     
     merged_data <- merge(flood_gages, height_table, by = "site_no") %>%
       merge(discharge, by = "site_no")
     
    
    result_table <- data.frame(
      StartDate = start_date,
      EndDate = end_date,
      TotalFloodEvents = find_flood_days %>%
        filter( flood == TRUE) %>%
        summarise(flood_day_count = n_distinct(Date)))
    
    output$result_table <- renderTable({
      result_table
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        year <- unique(merged_data$Year)
        park <- input$park_input
        paste("flood_events_", park, year, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(merged_data, file, row.names = FALSE)
      }
    )
    
    plot_data <- average_data %>%
      left_join(thresholds, by = "site_no") %>%
      mutate(exceeds_threshold = average >= threshold) 
    
    output$gage_plot <- renderPlot({
      ggplot(plot_data, aes(x = Date, y = average, color = site_no)) +
        geom_line() + 
        geom_point(data = plot_data %>% filter(exceeds_threshold), size = 3) + 
        geom_hline(data = thresholds, aes(yintercept = threshold, color = site_no), linetype = "dashed") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(title = "Flood Days", x = "Date", y = "Daily Average Gage Height")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)