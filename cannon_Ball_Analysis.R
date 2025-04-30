
#### load libraries ####
library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(EflowStats)
library(ggplot2)

## Create variable named TAPR which contains the Gauge station ID
TAPR <- c("07180400", "07182200", "07182250", "07182260")

# Create a lookup for month abbreviations to numeric format
month_lookup <- c(
  JAN = "01", FEB = "02", MAR = "03", APR = "04", MAY = "05", JUN = "06",
  JUL = "07", AUG = "08", SEP = "09", OCT = "10", NOV = "11", DEC = "12"
)

d <- read.csv("HTLN_FishCommunities_FishCountsThru_2023_Cleaned.csv")

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
  Year = as.integer(substr(PeriodID, 11, 14))) %>%
filter(ParkCode == "TAPR") 




####################################################################################

##### Create list that holds all the data collection dates
start_year <- (min(d$Year))-1 

last_year <- (max(d$Year))

daily_list <-  list()


#TAPR_COLLECTION
# Loop through each gage in the BUFF list
for(gage in TAPR ) { 

    
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
       wy_val = if_else(month(dateTime) >= 10, year_val + 1, year_val)
       )
 

monthly_data <- discharge %>%
   group_by(site_no, month_val, year_val) %>%
   summarize(mean_discharge = mean(Flow, na.rm = TRUE))

# Convert numeric month to month name
monthly_data <- monthly_data %>%
  mutate(month_name = factor(month.name[month_val], levels = month.name))


annual_data <- discharge %>%
  group_by(year_val, site_no) %>%
  summarize(mean_discharge = mean(Flow, na.rm = TRUE))


ggplot(annual_data, aes(x = as.numeric(year_val), y = mean_discharge)) +
  geom_line(color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Annual Mean Discharge Trends", x = "Year", y = "Mean Discharge (cfs)") +
  theme_minimal() +
  facet_wrap(~site_no)
 
ggplot(monthly_data, aes(x=month_name, y = mean_discharge)) +
  geom_line(color = "darkgreen") +
  facet_wrap(~site_no)

 
discharge <- discharge %>%
  dplyr:: filter(!(site_no %in% c("07182200", site_no == "07182260")))


# data set for variables
result_df <- data.frame(
  station = character(),
  water_year = numeric(),
  flow_means = numeric(),
  flow_long_mean = numeric(),
  flow_Q50 = numeric(),
  flow_long_Q50 = numeric(),
  flow_Q75 = numeric(),
  flow_long_Q75 = numeric(),
  flow_Q25 = numeric(),
  flow_long_Q25 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each station
for (station in unique(discharge$site_no)) {
  # Filter data for the current station
  station_data <- discharge[discharge$site_no == station, ]
  
  # Calculate the long-term average flow for the station
  long_mean <- mean(station_data$Flow, na.rm = TRUE)
  Q50 <- quantile(station_data$Flow, probs = .5, na.rm = TRUE)
  Q75 <- quantile(station_data$Flow, probs = 0.75, na.rm = TRUE)
  Q25 <- quantile(station_data$Flow, probs = .25, na.rm = TRUE)
  
  
  # Loop through each water year within the station
  for (water_year in unique(station_data$wy_val)) {
    # Filter data for the current water year
    water_year_data <- station_data[station_data$wy_val == water_year, ]
    
    # Calculate the annual average flow for the water year
    annual_mean <- mean(water_year_data$Flow, na.rm = TRUE)
    
    annual_median <- quantile(water_year_data$Flow, probs = .5, na.rm = TRUE)
    annual_Q75 <- quantile(water_year_data$Flow, probs = 0.75, na.rm = TRUE)
    annual_Q25 <- quantile(water_year_data$Flow, probs = .25, na.rm = TRUE)
   
    # Append the results to the result_df
    result_df <- rbind(result_df, data.frame(
      station = station,
      water_year = water_year,
      flow_means = annual_mean,
      flow_long_mean = long_mean,
      flow_Q50 = annual_median,
      flow_long_Q50 = Q50,
      flow_Q75 = annual_Q75,
      flow_long_Q75 = Q75,
      flow_Q25 = annual_Q25,
      flow_long_Q25 = Q25

    ))
  }
}

# View the resulting data frame
print(result_df)

summary_table <- result_df %>%
  group_by(station) %>%
  summarise(
    mean_flow_means = mean(flow_means),
    mean_flow_Q50 = mean(flow_Q50),
    mean_flow_Q75 = mean(flow_Q25),
    mean_flow_Q25 = mean(flow_Q75),
    Q75_flow_means = quantile(flow_means),
    mean_flow_Q50 = mean(flow_Q50),
    mean_flow_Q75 = mean(flow_Q25),
    mean_flow_Q25 = mean(flow_Q75))

  