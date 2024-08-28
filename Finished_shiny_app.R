##############################
##############################
## Fish data exploration #####
## Jeremy Brady          #####
## May 2024              #####
##############################
##############################

# Install necessary packages
install.packages("shiny")
install.packages("dataRetrieval")
install.packages("tidyverse")
install.packages("ggplot2")
################ libraries ################################
library(shiny)
library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(ggplot2)

########### variables #####################################

# Define lists of gages
OZAR <- c("07064440", "07064533", "07066000", "07065495", "07065200", "07066510", "07068000", "07067500", "07066510")
BUFF <- c("07055646", "07055660", "07055680", "07055780", "07056000", "07056700", "07055790", "07055792", "07055875", "07056515")
PIPE <- c("06482430")
TAPR <- c("07180400", "07182200", "07182250", "07182260")

####### data frame for gage data ######
all_discharge_data <- data.frame()

###### empty gage list ########################
gages <- list()

# Parameter IDs
parameterList <- list(
  "Discharge (cubic feet per second)" = "00060",
  "Precipitation (inches)" = "00045",
  "Water Temperature (Celsius)" = "00010",
  "Gage Height (feet)" = "00065"
)

# Month list for transformation
Mon_numbs <- c("JAN" = 1,
               "FEB" = 2,
               "MAR" = 3,
               "APR" = 4,
               "MAY" = 5,
               "JUN" = 6,
               "JUL" = 7,
               "AUG" = 8,
               "SEP" = 9,
               "OCT" = 10,
               "NOV" = 11,
               "DEC" = 12
)

######################## data ######################################

fish_data <- read.csv("C:/Users/jbrady/Desktop/fish project/HTLN_FishCommunities_FishCountsThru_2023_Cleaned.csv")
fish_data <- fish_data %>%
  mutate(PeriodID = gsub("OZARRMFISH|OZARRMFish|OZRSSprngs|OZRSSPRNGS|EFMOStfish|PERIStfish|BUFFRMFISH|BUFFrmfish|WICRStfish|GWCAStfish|HEHOStfish|PIPEShiner|TAPRShiner|BUFFRMFISH|HOMEShiner|HOSPStfish", "", PeriodID)) %>%
  mutate(PeriodID = gsub("Sept", "Sep", PeriodID, ignore.case = TRUE)) %>%
  mutate(PeriodID = gsub("July", "JUL", PeriodID, ignore.case = TRUE)) %>%
  mutate(Year = as.integer(substr(PeriodID, 1, 4))) %>%
  mutate(Month = toupper(substr(PeriodID, 5, 7))) %>%
  mutate(Day = as.integer(substr(PeriodID, 8, 9)))

fish_data <- fish_data %>% 
  mutate(Month = Mon_numbs[Month])

########################### Define UI ####################################
ui <- fluidPage(
  titlePanel("Fish Population and Water Parameter Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("fish_input", "Select Fish Name:", choices = sort(unique(fish_data$CommonName))),
      selectInput("park_input", "Select Park Code:", choices = sort(unique(fish_data$ParkCode))),
      selectInput("parameterCd", "Select Parameter:", choices = parameterList),
      dateRangeInput("dateRange", "Select Date Range:",
                     start = "2005-01-01", end = Sys.Date()),
      actionButton("getData", "Get Data")
    ),
    mainPanel(
      plotOutput("population_plot"),
      tableOutput("dataPreview")
    )
  )
)

########################### server logic #################################
server <- function(input, output) {
  data <- reactiveVal(NULL)
  aggregated_data <- reactiveVal(NULL)
  
  observeEvent(input$getData, {
    req(input$park_input, input$fish_input)
    
    # Filter fish data based on park and fish input
    filtered_data <- fish_data %>%
      filter(ParkCode == input$park_input & CommonName == input$fish_input)
    if (nrow(filtered_data) == 0) {
      showNotification("Selected fish data not found for the chosen parameters.", type = "warning")
      return()
    }    
    if (input$park_input == "OZAR") {
      filtered_data <- filtered_data %>%
        filter(!(Year %in% c(2010, 2013, 2015)))  # Corrected filtering by Year
    }
    
    ########### Group fish data by year and summarize fish count ##################
    aggregated_data_value <- filtered_data %>%
      group_by(Year) %>%
      summarise(Fish_Count = sum(NumObs)) %>%
      filter(Fish_Count > 2)   
    
    ########## Get the gage data ############################
    gages <- switch(input$park_input,
                    "OZAR" = OZAR,
                    "BUFF" = BUFF,
                    "PIPE" = PIPE,
                    "TAPR" = TAPR,
                    c())
    
    ######### add every gage for specific park ################    
    for(site in gages) {
      rawDailyData <- readNWISdv(
        siteNumber = site,
        parameterCd = input$parameterCd,
        startDate = input$dateRange[1],
        endDate = input$dateRange[2]
      )
      
      rawDailyData <- rawDailyData %>%
        mutate(Gage = site)
      
      all_discharge_data <- bind_rows(all_discharge_data, rawDailyData)
    }
    
    ################ Process water parameter data #########################
    parameter_column <- colnames(all_discharge_data)[4]  
    rawDailyData <- all_discharge_data %>%
      mutate(
        Date = as.Date(Date, format = "%Y-%m-%d"),
        Year = year(Date),
        Month = month(Date),
        Month_Year = paste(Month, Year, sep = '-'),
        Gage = Gage
      )
    
    # Create sequences for filtering rawDailyData
    sequences <- mapply(function(month, year) {
      months <- seq.int(1, month, by = 1)
      paste(months, year, sep = '-')
    }, filtered_data$Month, filtered_data$Year)
    
    sequences <- unlist(sequences)
    
    rawDailyData <- rawDailyData %>%
      filter(Month_Year %in% sequences) %>%
      group_by(Year, Gage) %>%
      summarise(Annual_average = mean(.data[[parameter_column]], na.rm = TRUE))
    
    aggregated_data(aggregated_data_value)
    data(rawDailyData)
  })
  
  ################### Plot ##########################################
  output$dataPreview <- renderTable({
    req(aggregated_data())
    req(data())
    
    table_data <- aggregated_data() %>%
      left_join(data(), by = "Year") 
    table_data$Year <- formatC(table_data$Year, digits = 4)
    head(table_data, n = 70)
  })
  
  output$population_plot <- renderPlot({
    req(data())
    req(aggregated_data())
    
    aggregated_data_value <- aggregated_data()
    rawDailyData <- data()
    
    # Set up the plot
    par(mar = c(5, 4, 4, 4) + 0.3)  # Increase right margin to make room for secondary y-axis
    plot(aggregated_data_value$Year, aggregated_data_value$Fish_Count,
         type = "b", col = "blue",
         ylab = "Fish Count", xlab = "Year",
         xaxt = "n")
    axis(side = 1, at = aggregated_data_value$Year, labels = aggregated_data_value$Year)   
    par(new = TRUE)
    plot(rawDailyData$Year, rawDailyData$Annual_average,
         type = "b", col = "red", axes = FALSE,
         xlab = "", ylab = "")
    axis(side = 4)
    mtext("Annual Average", side = 4, line = 3)
    legend("topleft", legend = c("Fish Count", "Annual Average"),
           col = c("blue", "red"), pch = c(1, 1))
  })
}

################### Run the application ###################################
shinyApp(ui = ui, server = server)
