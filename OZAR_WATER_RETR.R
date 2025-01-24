#### load libraries ####
library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(EflowStats)
library(ggplot2)

## Create variable named OZAR which contains the Gauge station ID
OZAR <- c("07064440", "07064533", "07066510", "07067000", "07067500", "07068000", "07065200", "07065495", "07066000")

# Create a lookup for month abbreviations to numeric format
month_lookup <- c(
  JAN = "01", FEB = "02", MAR = "03", APR = "04", MAY = "05", JUN = "06",
  JUL = "07", AUG = "08", SEP = "09", OCT = "10", NOV = "11", DEC = "12"
)

gage_data <- read.csv("OZAR_reach_and_gage_locations.csv")

gage_data <- gage_data %>%
  dplyr:: select(LocationID, NearestGageID) %>%
  mutate(
    site_no = paste0( "0", as.character(NearestGageID))
  ) %>%
  dplyr::select(LocationID, site_no)

d <- read.csv("~/work/fish project/Fish data/main data/HTLN_FishCommunities_FishCountsThru_2023_Cleaned.csv")

d <- d %>% mutate( 
  PeriodID = gsub(
    "OZARRMFISH|OZARRMFish|OZRSSprngs|OZRSSPRNGS|EFMOStfish|PERIStfish|
       BUFFRMFISH|BUFFrmfish|WICRStfish|GWCAStfish|HEHOStfish|PIPEShiner|
       TAPRShiner|BUFFRMFISH|HOMEShiner|HOSPStfish", 
    "", PeriodID
  ), # standardize any issues with format
  PeriodID = gsub("Sept", "Sep", PeriodID, ignore.case = TRUE),
  PeriodID = gsub("July", "Jul", PeriodID, ignore.case = TRUE),
  PeriodID = gsub("MAY", "May", PeriodID, ignore.case = TRUE),
  PeriodID = gsub("OCT", "Oct", PeriodID, ignore.case = TRUE),
  # Extract `Year`, `Month`, and `Day` from `PeriodID`
  Year = as.integer(substr(PeriodID, 1, 4))
) %>%
  filter(ParkCode == "OZAR") 

d <- full_join(d, gage_data, by = "LocationID" ) %>%
  mutate(
    key = paste0(as.character(site_no), "-", as.character(Year))
  )

####################################################################################
########################                     #######################################
######################## 15 months TAPR      #######################################
########################                     #######################################
####################################################################################

##### Create list that holds all the data collection dates
start_year <- (min(d$Year))-1 

last_year <- (max(d$Year))

daily_list <-  list()


#TAPR_COLLECTION
# Loop through each gage in the BUFF list
for(gage in OZAR ) { 
  
  
  start_date <- paste0(as.character(start_year), "-10-01")
  end_date <- paste0(as.character(last_year), "-09-30")
  
  # Fetch and process data
  rawDailyData <- readNWISdata(
    service = "dv",                   # Service for daily values
    siteNumber = gage,                # Gage ID
    parameterCd = "00060",            # Parameter code for discharge (cfs)
    startDate = start_date,         # Start date
    endDate = end_date            # End date
  ) %>% 
    renameNWISColumns() %>%
    mutate(group = paste(site_no, dateTime, sep = "-"))
  
  daily_list[[gage]] <- rawDailyData
}
# Append to the main data frame
daily_data <- bind_rows(daily_list)



discharge <- daily_data %>%
  mutate(
    dateTime = as.Date(dateTime),
    year_val = year(dateTime),
    month_val = month(dateTime),
    day_val = day(dateTime),
    jul_val = yday(dateTime),
    wy_val = if_else(month(dateTime) >= 10, year_val + 1, year_val),
    # Calculate the start of the water year
    wy_start = as.Date(if_else(month_val >= 10, 
                               paste0(year_val, "-10-01"), 
                               paste0(year_val - 1, "-10-01"))),
    # Calculate water year day
    wy_day = as.integer(difftime(dateTime, wy_start, units = "days")) + 1
  )

# calcualte monthly discharge
monthly_data <- discharge %>%
  group_by(site_no, month_val, year_val) %>%
  summarize(mean_discharge = mean(Flow, na.rm = TRUE))

# Convert numeric month to month name
monthly_data <- monthly_data %>%
  mutate(month_name = factor(month.name[month_val], levels = month.name))

#calculate annual mean discharge 
annual_data <- discharge %>%
  group_by(year_val, site_no) %>%
  summarize(mean_discharge = mean(Flow, na.rm = TRUE))

#plot of annual discharge for each site
ggplot(annual_data, aes(x = as.numeric(year_val), y = mean_discharge)) +
  geom_line(color = "darkgreen") +
  labs(title = "Annual Mean Discharge Trends", x = "Year", y = "Mean Discharge (cfs)") +
  theme_minimal() +
  facet_wrap(~site_no)

# plot of monthly mean discharge for each site
ggplot(monthly_data, aes(x=month_name, y = mean_discharge)) +
  geom_line(color = "darkgreen") +
  facet_wrap(~site_no)



# Initialize an empty data frame to store results
result_df <- data.frame(
  station = character(),
  water_year = numeric(),
  annual_discharge = numeric(),
  short_term_change = numeric(),
  days_over_7xmedian = numeric(),
  high_flow_duration = numeric(),
  fall_rate = numeric(),
  low_discharge = numeric(),
  low_flow_duration = numeric(),
  key = character(),
  stringsAsFactors = FALSE
)

# Loop through each station
for (station in unique(discharge$site_no)) {
  # Filter data for the current station
  station_data <- discharge[discharge$site_no == station, ]
  
  # Calculate the long-term average flow for the station
  long_mean <- mean(station_data$Flow, na.rm = TRUE)
  
  long_median <- median(station_data$Flow, na.rm = TRUE)
   print(long_median)
  
  Q75 <- quantile(station_data$Flow, probs = 0.75, na.rm = TRUE)
  Q25 <- quantile(station_data$Flow, probs = 0.25, na.rm = TRUE)
  # Loop through each water year within the station
  for (water_year in unique(station_data$wy_val)) {
    # Filter data for the current water year
    water_year_data <- station_data[station_data$wy_val == water_year, ]
    
    # Calculate the annual average flow for the water year
    annual_mean <- mean(water_year_data$Flow, na.rm = TRUE)
    
    # Calculate the short-term change as a percentage
    short_term_change <- ((annual_mean / long_mean) * 100) - 100
    
    # Count the number of days over Q75
    days_over_Q75 <- sum(water_year_data$Flow >= (Q75), na.rm = TRUE)
    
    low_flow <- min(water_year_data$Flow, na.rm = TRUE)
    
 print(days_over_Q75)
    # Initialize variables to track consecutive days and event durations
    event_durations <- c() 
    current_consecutive_days <- c()
    flow_rate <- c()
    flow_changes <- c()
    drought_event_durations <- c() 
    current_drought_consecutive_days <- c()
    # a loop that goes through each row of the water_year_data set; i is for
    # the current row and seq_len(nrow()) give a sequence of numbers starting from
    #the first row to the total number of rows
    for (i in seq_len(nrow(water_year_data))){
      day <- water_year_data$wy_day[i] # creates a variable for the current water day
      flow <- water_year_data$Flow[i] # creates a variables for the current flow
      
      if (flow >= Q75) {
        # If flow exceeds 7 X long_median, conditionally, if current_consecutive_days is 0 
        # or if current day is equal to the last day + 1. We do this by indexing the last item in the current_consecutive_days using
        # [length(current_consecutive_days)] +1. then that day is added to the current_consecutive_days vector
        if (length(current_consecutive_days) == 0 || day == current_consecutive_days[length(current_consecutive_days)] + 1) {
          # Add the day to the current sequence
          current_consecutive_days <- c(current_consecutive_days, day)
        } else {
          # If the day is not consecutive, 
          #then the length of the current_consecutive_days is addeded to event_durations.
          #current_consecutive_days is reset with the new day
          event_durations <- c(event_durations, length(current_consecutive_days))
          current_consecutive_days <- c(day)
        }
      } else {
        # if flow is lower than 7*long_median then the days in the current_consecutive_days
        # are added to event_durations and current_consecutive days is reset. 
        if (length(current_consecutive_days) > 0) {
          event_durations <- c(event_durations, length(current_consecutive_days))
          current_consecutive_days <- c()
        } 
      }
      if (flow <= Q25) {
        # If flow exceeds 7 X long_median, conditionally, if current_consecutive_days is 0 
        # or if current day is equal to the last day + 1. We do this by indexing the last item in the current_consecutive_days using
        # [length(current_consecutive_days)] +1. then that day is added to the current_consecutive_days vector
        if (length(current_drought_consecutive_days) == 0 || day == current_drought_consecutive_days[length(current_drought_consecutive_days)] + 1) {
          # Add the day to the current sequence
          current_drought_consecutive_days <- c(current_drought_consecutive_days, day) 
        } else {
          # If the day is not consecutive, 
          #then the length of the current_consecutive_days is addeded to event_durations.
          #current_consecutive_days is reset with the new day
          drought_event_durations <- c(drought_event_durations, length(current_drought_consecutive_days))
          current_drought_consecutive_days <- c(day)
        }
      } else {
        # if flow is lower than 7*long_median then the days in the current_consecutive_days
        # are added to event_durations and current_consecutive days is reset. 
        if (length(current_drought_consecutive_days) > 0) {
          drought_event_durations <- c(drought_event_durations, length(current_drought_consecutive_days))
          current_drought_consecutive_days <- c()
        }
      }
      
      if (length(flow_rate) == 0) {
        flow_rate <- c(flow)
      } else {
        # Compare the current flow to the last recorded flow
        if (flow < flow_rate[length(flow_rate)]) {
          # If the flow decreases, calculate the change and record it
          change <- flow - flow_rate[length(flow_rate)]
          flow_changes <- c(flow_changes, change)
        }
      }
      
      # Add the current flow to flow_rate for future comparisons
      flow_rate <- c(flow_rate, flow)
      
      # Add the final event duration if still in progress

    }
    
    # Calculate the median and mean duration of events
    median_duration <- median(event_durations)
    mean_duration <- mean(event_durations)
    mean_rate_change <- mean(flow_changes)
    mean_drought_duration <- mean(drought_event_durations)
    # Append the results for the current year to the result_df
    result_df <- rbind(result_df, data.frame(
      station = station,
      water_year = water_year,
      annual_mean = format(round(annual_mean,2)),
      short_term_change = format(round(short_term_change,2)),
      days_over_Q75 = days_over_Q75,
      high_flow_duration = round(mean_duration,0),
      fall_rate = round(mean_rate_change,2),
      low_discharge = round(low_flow, 2),
      low_flow_duration= round(mean_drought_duration,0),
      key = paste0(as.character(station), "-", as.character(water_year))
    )) 
  }
}

result_df$high_flow_duration[is.na(result_df$high_flow_duration)] <- 0
result_df$low_flow_duration[is.na(result_df$low_flow_duration)] <- 0

# View the resulting data frame
print(result_df)

d <- full_join(d, result_df, by = "key") %>%
  dplyr:: select(-site_no)

write.csv(d, "OZAR_fish_data.csv")
