library(tidyverse)


fish_data <- read.csv("C:/Users/jbrady/Desktop/fish project/HTLN_FishCommunities_FishCountsThru_2023_Cleaned.csv")
fish_data <- fish_data %>%
  mutate(PeriodID = gsub("OZARRMFISH|OZARRMFish|OZRSSprngs|OZRSSPRNGS|EFMOStfish|PERIStfish|BUFFRMFISH|BUFFrmfish|WICRStfish|GWCAStfish|HEHOStfish|PIPEShiner|TAPRShiner|BUFFRMFISH|HOMEShiner|HOSPStfish", "", PeriodID))


# run a view of data to see what is going on (view and glimpse); distinct / unique; filter the na
fish_data<- fish_data %>%
  mutate(PeriodID = gsub("Sept", "Sep", PeriodID, ignore.case = TRUE))

fish_data$PeriodID <- as.integer(substr(fish_data$PeriodID, 1, 4))

unique_common_names <- fish_data %>%
  filter(ParkCode == "OZAR") %>%
  select(CommonName) %>%
  distinct()

filtered_data <- fish_data %>% filter(PeriodID >= 2010 & PeriodID %% 2 == 0 & ParkCode == "OZAR" & CommonName == "Bigeye chub")

# Group data by year and summarize fish count
aggregated_data <- filtered_data %>%
  group_by(PeriodID) %>%
  summarize(Fish_Count = sum(NumObs))

ggplot(aggregated_data, aes(x = PeriodID, y = Fish_Count)) +
  geom_line() +
  labs(title = paste("Change in fish Population at Park Over Years"),
       x = "Year",
       y = "Fish Count") +
  scale_x_continuous(breaks = seq(ceiling(min(aggregated_data$PeriodID)), floor(max(aggregated_data$PeriodID)), by = 2)) +
  theme_minimal()
