
library(dataRetrieval)
library(tidyverse)
library(lubridate)


# Define lists of gages
OZAR <- c("07064440", "07064533", "07066000", "07065495", "07065200", "07066510", "07068000", "07067500", "07066510")
BUFF <- c("07055646", "07055660", "07055680", "07055780", "07056000", "07056700", "07055790", "07055792", "07055875", "07056515")
PIPE <- c("06482430")
TAPR <- c("07180400", "07182200", "07182250", "07182260")


####################################################################################
#########################                   ########################################
######################### Discharge at BUFF ########################################
#########################                   ########################################
####################################################################################
    
    # Fetch data for the current gage and year

      fetch_data <- function(gage, year) {
        start_date <- paste0(year, "-01-01")
        end_date <- paste0(year, "-12-31")
        
        rawDailyData <- tryCatch({
          readNWISdv(
            siteNumber = gage,
            parameterCd = "00060",  
            startDate = start_date,
            endDate = end_date
          ) %>%
            select(-1) %>%
            renameNWISColumns() %>%
            mutate(dateTime = parse_date_time(dateTime, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd")),
                   Date = as.Date(dateTime))
        }, error = function(e) {
          NULL
        })
        
        rawDailyData
      }
############################################## Data clean up #######################
      BUFF_discharge_all_years_data <- map_dfr(BUFF, function(gage) {
        map_dfr(2005:2023, function(year) {
          fetch_data(gage, year)
        }, .id = "year")
      }, .id = "gage")
      
      BUFF_discharge_all_years_data <- BUFF_discharge_all_years_data %>%
        mutate(Date = as.Date(Date))
      
# Function to calculate flood days for each dataframe
filter_flood_days <- function(df) {
  df %>% filter(flood == "TRUE")
}
find_flood_days <-  filter_flood_days %>%
    group_by(Date) %>% 
    mutate(flood = case_when(
      site_no == "07055646" & Flow >= 6500 ~ "TRUE",
      site_no == "07055660" & Flow >= 1600 ~ "TRUE",
      site_no == "07055680" & Flow >= 2000 ~ "TRUE",
      site_no == "07056000" & Flow >= 8000 ~ "TRUE",
      site_no == "07056700" & Flow >= 9370 ~ "TRUE",
      TRUE ~ "FALSE")) %>%
    filter(flood == TRUE)


flood_gages <- find_flood_days %>%
  group_by(Date) %>%
  summarise(gage_count = n_distinct(site_no))

number_of_flood_days <- flood_gages %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(flood_day_count = n_distinct(Date))
# Write to CSV

write.csv(final_data, file = "BUFF_discharge_data_compare.csv")

print(final_data)

####################################################################################
########################                     #######################################
######################## Gage Height at BUFF #######################################
########################                     #######################################
####################################################################################
fetch_data <- function(gage, year) {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  
  rawDailyData <- tryCatch({
    readNWISdata(
      siteNumber = gage,
      parameterCd = "00065",
      service = "uv",
      startDate = "2010-01-01",
      endDate = "2013-12-31"
    ) %>%
      select(-1) %>%
      renameNWISColumns() %>%
      mutate(dateTime = parse_date_time(dateTime, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd")),
             Date = as.Date(dateTime))
  }, error = function(e) {
    NULL
  })
  
  rawDailyData
}

################################ fetch the gage height ########################################################
BUFF_gage_all_years_data <- map_dfr(BUFF, function(gage) {
  map_dfr(2005:2023, function(year) {
    fetch_data(gage, year)
  }, .id = "year")
}, .id = "gage")

BUFF_gage_all_years_data <- BUFF_gage_all_years_data %>%
  mutate(Date = as.Date(Date))


#################### Function to calculate flood days for each dataframe ###################

find_flood_days <- BUFF_gage_all_years_data %>%
    group_by(site_no, Date) %>%
    mutate(flood = case_when(
      site_no == "07055646" & GH_Inst >= 8.79 ~ TRUE,
      site_no == "07055680" & GH_Inst >= 7.81 ~ TRUE,
      site_no == "07056000" & GH_Inst >= 11.11 ~ TRUE,
      site_no == "07056700" & GH_Inst >= 10.62 ~ TRUE,
      TRUE ~ FALSE))%>%
  filter(flood == TRUE)

flood_gages <- find_flood_days %>%
  group_by(Date) %>%
  summarise(gage_count = n_distinct(site_no))

number_of_flood_days <- flood_gages %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(flood_day_count = n_distinct(Date))

 
write.csv(final_data, file = "BUFF_gage_height_data_compare_4.csv")
#################################################################################
#######################                         #################################
####################### Get average gage height #################################
#######################                         #################################
#################################################################################
fetch_data <- function(gage, year) {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  
  rawDailyData <- tryCatch({
    readNWISdata(
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
  }, error = function(e) {
    NULL
  })
  
  rawDailyData
}

################################ fetch the mean gage height ########################################################
BUFF_gage_all_years_data <- map_dfr(BUFF, function(gage) {
  map_dfr(2005:2023, function(year) {
    fetch_data(gage, year)
  }, .id = "year")
}, .id = "gage")


# Calculate average daily water level

average_data <- BUFF_gage_all_years_data %>%
  group_by(site_no, Date) %>%
  mutate(average = mean(GH_Inst, na.rm = TRUE), .groups = 'drop')

####################### Make plot ##############################################
average_data <- average_data %>%
  mutate(Year = year(Date))%>%
  group_by(Date, site_no) %>%
  summarise(daily_value = mean(GH_Inst, NA.RM = TRUE), .groups = "drop")

thresholds <- data.frame(
  site_no = c("07055646", "07055680", "07056000", "07056700"),
  threshold = c(8.79, 7.81, 11.11, 10.62))


plot_data <- average_data %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  group_by(Month, Year, site_no) %>%
  summarise(monthly_value = mean(daily_value, NA.RM = TURE), .groups = "drop") %>%
  left_join(thresholds, by = "site_no") %>%
  mutate(exceeds_threshold = monthly_value >= threshold)

ggplot(plot_data, aes(x = Month, y = monthly_value, color = site_no)) +
  geom_line() + 
  geom_point(data = plot_data %>% filter(exceeds_threshold), size = 3) +
  facet_wrap(~Year) +  
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),          
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Flood Days", x = "Date", y = "Daily Average Gage Height")

################################################################################  
  # Identify flood days
flood_days <- average_data %>%
    mutate(flood = case_when(
      site_no == "07055646" & average >= 8.79 ~ TRUE,
      site_no == "07055680" & average >= 7.81 ~ TRUE,
      site_no == "07056000" & average >= 11.11 ~ TRUE,
      site_no == "07056700" & average >= 10.62 ~ TRUE,
      TRUE ~ FALSE
      )) %>%
  filter(flood == TRUE)


flood_gages <- flood_days %>%
  group_by(Date) %>%
  summarise(gage_count = n_distinct(site_no))

number_of_flood_days_per_month <- flood_gages %>%
  mutate(Month = month(Date)) %>%
  mutate(Year = year(Date)) %>%
  group_by(Month, Year) %>%
  summarize( Monthly_flood = n())

# Ensure Month is a factor with levels in the correct order
number_of_flood_days_per_month <- number_of_flood_days_per_month %>%
  mutate(Month = factor(Month, levels = 1:12, labels = month.name))

# Plot with ggplot
ggplot(number_of_flood_days_per_month, aes(x = Month, y = Monthly_flood)) +
  geom_col() +
  facet_wrap(~ Year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: Rotate x-axis labels for better readability



number_of_flood_days <- flood_gages %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(flood_day_count = n_distinct(Date))


######################## Discharge at OZAR #########################################

####################################################################################
#######################                     ########################################
####################### Gage Height at OZAR ########################################
#######################                     ########################################
####################################################################################
fetch_data <- function(gage, year) {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  
  rawDailyData <- tryCatch({
    readNWISdata(
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
  }, error = function(e) {
    NULL
  })
  
  rawDailyData
}

################################ fetch the gage height ########################################################
OZAR_gage_all_years_data <- map_dfr(OZAR, function(gage) {
  map_dfr(2005:2023, function(year) {
    fetch_data(gage, year)
  }, .id = "year")
}, .id = "gage")

average_data <- OZAR_gage_all_years_data %>%
  group_by(site_no, Date) %>%
  mutate(average = mean(GH_Inst, na.rm = TRUE), .groups = 'drop')

# Check for any flood events per day
flood_days <- OZAR_gage_all_years_data %>%
  group_by(Date, site_no) %>%
  mutate(flood = case_when(
    site_no == "07064440" & GH_Inst >= 2 ~ TRUE,
    site_no == "07064533"& GH_Inst >=  4 ~ TRUE,
    site_no == "07067000" & GH_Inst >= 5 ~ TRUE,
    site_no == "07065200" & GH_Inst >= 4 ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(flood == TRUE)

flood_gages <- flood_days %>%
  group_by(Date) %>%
  summarise(gage_count = n_distinct(site_no))

number_of_flood_days <- flood_gages %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(flood_day_count = n_distinct(Date))



write.csv(number_of_flood_days, file = "OZAR_mean_gage_height_data.csv")
######################## Discharge at PIPE #########################################

####################################################################################
########################                     #######################################
######################## Gage Height at PIPE #######################################
########################                     #######################################
####################################################################################
fetch_data <- function(gage, year) {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  
  rawDailyData <- tryCatch({
    readNWISdata(
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
  }, error = function(e) {
    NULL
  })
  
  rawDailyData
}

################################ fetch the gage height ########################################################
PIPE_gage_all_years_data <- map_dfr(PIPE, function(gage) {
  map_dfr(2005:2023, function(year) {
    fetch_data(gage, year)
  }, .id = "year")
}, .id = "gage")

# Check for any flood events per day
flood_days <- PIPE_gage_height_data %>%
  group_by(Date, site_no) %>%
  mutate(flood = case_when(
     GH_Inst >= 16 ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filyer(flood == TRUE)

flood_gages <- flood_days %>%
  group_by(Date) %>%
  summarise(gage_count = n_distinct(site_no))

number_of_flood_days <- flood_gages %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(flood_day_count = n_distinct(Date))


write.csv(PIPE_data, file = "PIPE_data.csv")

####################################################################################
########################                   #########################################
######################## Discharge at TAPR #########################################
#######################                    #########################################
####################################################################################


gages <- TAPR  # Add more gages if needed
start_date <- "2005-01-01"
end_date <- "2024-01-01"
TAPR_discharge_data <- data.frame()

for (site in TAPR) {
  rawDailyData <- readNWISdv(
    siteNumber = site,
    parameterCd = "00060",
    startDate = start_date,
    endDate = end_date
  )
  
  rawDailyData <- rawDailyData %>%
    select(-1) %>%
    renameNWISColumns() %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
  
  threshold <- case_when(
    site == "07182250" ~ 15000,
    TRUE ~ NA_real_
  )
  
  rawDailyData <- rawDailyData %>% filter(Flow >= threshold)
  TAPR_discharge_data <- bind_rows(TAPR_discharge_data, rawDailyData)
}

# Check for any flood events per day
flood_days <- TAPR_discharge_data %>%
  group_by(Date) %>%
  summarise(FloodEvent = any(Flow >= 15000), .groups = 'drop')

# Filter for days with any flood event
flood_days <- flood_days %>% filter(FloodEvent)

# Summarize the number of flood days per year
final_data <- flood_days %>%
 
  summarise(FloodDays = n(), .groups = 'drop')

# Create a complete sequence of years
years <- data.frame(Year = seq(from = year(as.Date(start_date)), to = year(as.Date(end_date)), by = 1))

# Join with the flooding data and replace NA with 0
final_data <- left_join(years, final_data, by = "Year") %>%
  mutate(FloodDays = ifelse(is.na(FloodDays), 0, FloodDays))

# Write to CSV
write.csv(final_data, file = "TAPR_discharge_data.csv")

print(final_data)
####################################################################################
########################                     #######################################
######################## Gage Height at TAPR #######################################
########################                     #######################################
####################################################################################

####################################################################################
#####################                                 ##############################
##################### Get average gage height at TAPR ##############################
#####################                                 ##############################
####################################################################################
fetch_data <- function(gage, year) {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  
  rawDailyData <- tryCatch({
    readNWISdata(
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
  }, error = function(e) {
    NULL
  })
  
  rawDailyData
}

################################ fetch the mean gage height ########################################################
TAPR_gage_all_years_data <- map_dfr(TAPR, function(gage) {
  map_dfr(2005:2023, function(year) {
    fetch_data(gage, year)
  }, .id = "year")
}, .id = "gage")


average_data <- TAPR_gage_all_years_data %>%
  group_by(site_no, Date) %>%
  mutate(average = mean(GH_Inst, na.rm = TRUE), .groups = 'drop')


# Identify flood days
flood_days <- average_data %>%
  mutate(flood = case_when(
    average >= 32 ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(flood == TRUE)


flood_gages <- flood_days %>%
  group_by(Date) %>%
  summarise(gage_count = n_distinct(site_no))

number_of_flood_days <- flood_gages %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(flood_day_count = n_distinct(Date))
