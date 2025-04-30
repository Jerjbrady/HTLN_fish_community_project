####################### BUFF River Fish / Climate Model #####################
####################### By: Jeremy Brady      ###############################
####################### Date: 09/20/2024      ###############################

###################### load libraries #######################################
# update any installed R packages
update.packages(ask = FALSE, checkBuilt = TRUE)

# packages to install
pkgs <- c("dplyr", "gratia", "ggplot2",
          "marginaleffects", "tidybayes", "zoo",
          "viridis", "remotes", "brms")

# install those packages by setting Ncpus to number of CPU cores you have available
install.packages(pkgs, Ncpus = 2)

install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
check_cmdstan_toolchain()

library(cmdstanr)


theme_set(theme_classic(base_size = 12, base_family = 'serif') +
            theme(axis.line.x.bottom = linewidth(colour = "black",
                                                    size = 1),
                  axis.line.y.left = linewidth(colour = "black",
                                                  size = 1)))
options(ggplot2.discrete.colour = c("#A25050",
                                    "#8F2727",
                                    'darkred',
                                    "#630000"),
        ggplot2.discrete.fill = c("#A25050",
                                  "#8F2727",
                                  'darkred',
                                  "#630000"))
install.packages("mvgam")
library(dplyr)
library(stringr)
library(vegan)
library(lattice)
library(MASS)
library(DHARMa)
library(lme4)
library(ggplot2)
library(corrplot)
library(corrr)
library(mgcv)
library(mvgam)

############################# load data; be sure to change directory ###########
fish_count <- read.csv("BUFF_fish_data.csv")
e_data <- read.csv("HTLN_FishCommunities_ReachMeasurements_Cleaned.csv")
stream_data <- read.csv('HTLN_FishCommunities_BankMeasurementInfo_Cleaned.csv')

stream_data <- stream_data%>%
  dplyr::select(LocationID, EventID, BankVegCover_Percent, BankAngle_Degrees, BankSubstrate)
  



e_data[e_data == -999] <- NA


locationid <- c()
reproductive <- c()
fish_species <- c()

locationid <- append(locationid, unique(fish_count$LocationID))
reproductive <- append(reproductive, unique(fish_count$ReproductiveClassification))
fish <- append(fish_species, unique(fish_count$ScientificName))


e_data <- e_data %>%
  group_by(EventID, LocationID) %>%
  summarise(across(7:13, function(x) mean(x, na.rm = TRUE))) 
  
              


mainstem_reproductive_data <- fish_count %>%
  mutate(
    ReproductiveClassification = case_when(
      ReproductiveClassification == reproductive[1] ~ "Lithophilic spawner",
      ReproductiveClassification == reproductive[2] ~ "Non-lithophilic spawner",
      ReproductiveClassification == reproductive[3] ~ "Unknown",
      ReproductiveClassification == reproductive[4] ~ NA_character_  # Ensure NA is of character type
    )
  ) %>%
  # Drop rows where classification is NA or "Unknown"
  filter(!is.na(ReproductiveClassification), ReproductiveClassification != "Unknown") %>%
  # Exclude specific LocationIDs
  filter(Year %in% c(2008, 2009, 2010, 2013, 2015, 2017, 2019, 2021, 2023))%>%
  # Include only certain LocationIDs
  filter(LocationID %in% c(locationid[1], locationid[2], locationid[3], locationid[4], locationid[5], locationid[6]))



mainstem_reproductive_data <- mainstem_reproductive_data %>%
  group_by(Year, LocationID) %>%
  mutate(Total_Group = sum(NumObs, na.rm = TRUE)) %>%  # Calculate total NumObs for the group
  group_by(Year, LocationID, ReproductiveClassification, ScientificName) %>%
  reframe(
    Total = sum(NumObs, na.rm = TRUE),
    Relative_abun = Total / first(Total_Group),       # Use Total_Group for relative abundance
    annual_mean = first(annual_mean),                 # Assuming annual_mean is constant per group
    short_term_change = first(short_term_change),     # Similarly, for other variables
    days_over_Q75 = first(days_over_Q75),
    high_flow_duration = first(high_flow_duration),
    fall_rate = first(fall_rate),
    low_discharge = first(low_discharge),
    low_flow_duration = first(low_flow_duration),
    EventID = first(EventID)                                 # Ungroup the result
  ) 


mainstem_reproductive_data <- inner_join(e_data, mainstem_reproductive_data, 
                         by = c("LocationID" = "LocationID", "EventID" = "EventID")) 

mainstem_reproductive_data <- inner_join(stream_data, mainstem_reproductive_data,
                         by = c('LocationID', relationship = "many-to-many"))

series_data <- mainstem_reproductive_data %>%
  mutate(
    series = as.factor(ScientificName)
  ) 


for (i in fish) {
  for (j in 7:13) {
    col_name <- names(mainstem_reproductive_data)[j]
    
    # Filter the data for the current species
    filtered_data <- mainstem_reproductive_data %>% filter(ScientificName == i)
    
    # Create the plot
    p <- ggplot(filtered_data, aes(x = .data[[col_name]], y = qlogis(Relative_abun))) +
      geom_line() +
      facet_wrap(~ LocationID) +
      labs(title = paste("Relative Abundance vs", col_name, "for", i),
           y = col_name,
           x = "Relative Abundance")
    
    # Print the plot
    print(p)
  }
}
  

top_data <- series_data %>%
  dplyr::select(-LocationID) %>%
  group_by(Year, series) %>%
  summarise(Total = sum(Total), .groups = "drop" ) %>%
  group_by(Year) %>%
  mutate(
    Rel_Total = sum(Total)) %>%
  ungroup() %>%
  mutate(Rel_abund =
           Total /Rel_Total,
         time = as.numeric(factor(Year, levels = sort(unique(Year)))))




plot_mvgam_series(data = top_data, y = 'Rel_abund', series = 'all')


mainstem_litho_data <- mainstem_reproductive_data %>%
  filter(ReproductiveClassification == "Lithophilic spawner")

LEPCYA_litho_data <- mainstem_reproductive_data %>%
  filter(ScientificName == 'Lepomis cyanellus')

mainstem_non_litho_data <- mainstem_reproductive_data %>%
  filter(ReproductiveClassification == "Non-lithophilic spawner")

top_data_location <- series_data %>%
  group_by(Year, series, LocationID) %>%
  mutate(Total = sum(Total)) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(
    Rel_Total = sum(Total)) %>%
  ungroup() %>%
  mutate(Rel_abund =
           Total /Rel_Total,
         time = as.numeric(factor(Year, levels = sort(unique(Year)))))

d <- top_data_location %>%
  group_by(Year, LocationID) %>%
  reframe(
    series = series,
    mean_Total = mean(Total),
    relative_abun = mean(Relative_abun),       # Use Total_Group for relative abundance
    annual_mean = first(annual_mean),                 # Assuming annual_mean is constant per group
    short_term_change = first(short_term_change),     # Similarly, for other variables
    days_over_Q75 = first(days_over_Q75),
    high_flow_duration = first(high_flow_duration),
    fall_rate = first(fall_rate),
    low_discharge = first(low_discharge),
    low_flow_duration = first(low_flow_duration),
    EventID = first(EventID)                                 # Ungroup the result
  ) 

top_15_data_location %>% 
  dplyr::filter(series == 'Campostoma') %>%
  ggplot(aes(x = fall_rate, y = qlogis(Rel_abund))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 9),
              col = 'darkred', fill = "#A25050") +
  labs(title = 'DM',
       y = "logit(relative abundance)", 
       x = 'fall_rate') +
  facet_wrap( ~ LocationID)
  
top_15_data_location %>% 
  dplyr::filter(ScientificName == 'Etheostoma caeruleum') %>%
  ggplot(aes(x = low_discharge, y = qlogis(Rel_abund))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 9),
              col = 'darkred', fill = "#A25050") +
  labs(y = NULL, 
       x = 'low discharge') +
  facet_wrap(~LocationID)

mainstem_litho_data %>% 
  dplyr::filter(ScientificName == 'Etheostoma caeruleum') %>%
  ggplot(aes(x = low_flow_duration, y = qlogis(Relative_abun))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(title = 'DM',
       y = "logit(relative abundance)", 
       x = 'low_flow_duration') 
#################################################################################
  mainstem_non_litho_data %>% 
  dplyr::filter(ScientificName == 'Lepomis cyanellus') %>%
  ggplot(aes(x = high_flow_duration, y = qlogis(Relative_abun))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(y = NULL, 
       x = 'high flow duration')

mainstem_non_litho_data %>% 
  dplyr::filter(ScientificName == 'Lepomis cyanellus') %>%
  ggplot(aes(x = fall_rate, y = qlogis(Relative_abun))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(title = 'DM',
       y = "logit(relative abundance)", 
       x = 'fall_rate') +
  
  mainstem_non_litho_data %>% 
  dplyr::filter(ScientificName == 'Lepomis cyanellus') %>%
  ggplot(aes(x = low_discharge, y = qlogis(Relative_abun))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(y = NULL, 
       x = 'low discharge')

mainstem_non_litho_data %>% 
  dplyr::filter(ScientificName == 'Lepomis cyanellus') %>%
  ggplot(aes(x = low_flow_duration, y = qlogis(Relative_abun))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(title = 'DM',
       y = "logit(relative abundance)", 
       x = 'low_flow_duration') +
  
  mainstem_non_litho_data %>% 
  dplyr::filter(ScientificName == 'Lepomis cyanellus') %>%
  ggplot(aes(x = high_flow_duration, y = qlogis(Relative_abun))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(y = NULL, 
       x = 'high flow duration')
##### test for correlation ###

correlation <- litho_data %>%
  dplyr::select( 
    high_flow_duration, 
    fall_rate, 
    low_discharge,
    low_flow_duration,
    short_term_change) %>%
  distinct() 

cor_matrix <- correlate(correlation)

corrplot(correlation, method = "circle")

print(correlation)


############################################### model selection ################

mainstem_litho_data$LocationID <- as.factor(mainstem_litho_data$LocationID)
mainstem_litho_data$time <- as.factor(mainstem_litho_data$Year)
mainstem_litho_data$series <- as.factor(mainstem_litho_data$ScientificName)


LEPCYA_litho_data$LocationID <- as.factor(LEPCYA_litho_data$LocationID)
LEPCYA_litho_data$time <- as.factor(LEPCYA_litho_data$Year)


mainstem_litho_data <- mainstem_litho_data %>%
  mutate(scale(Year))
plot_mvgam_series(data = mainstem_litho_data, y = "Relative_abun", series = "all")
plot_mvgam_series(data = mainstem_litho_data, y = "Total", series = 1)
plot_mvgam_series(data = mainstem_litho_data, y = "Total", series = 2)



mainstem_non_litho_data$LocationID <- as.factor(mainstem_non_litho_data$LocationID)
mainstem_non_litho_data$time <- mainstem_non_litho_data$Year
mainstem_non_litho_data$series <- as.factor(mainstem_non_litho_data$ScientificName)

plot_mvgam_series(data = mainstem_non_litho_data, y = "Relative_abun", series = "all")
plot_mvgam_series(data = mainstem_non_litho_data, y = "Total", series = 1)
plot_mvgam_series(data = mainstem_non_litho_data, y = "Total", series = 2)




glimpse(mainstem_litho_data)


fish_m_1 <- gam(Relative_abundance ~ 
                  s(Year, K = 8) +
                  s(low_discharge, k = 25) + 
                  s(fall_rate, k = 20, by = time) +
                s(SpecificConductance_MicrosiemensPerCentimeter, k = 18),
                data = mainstem_litho_data, 
                method ='REML',
                family = betar(link = "logit"))
summary(fish_m_1)



plot(fish_m_1, all.terms = TRUE, pages = 1)

  
k.check(fish_m_1)
gam.check(fish_m_1)

fish_m_
1 <- gam(Total ~
                  Year +
                  low_discharge +
                  fall_rate +
                  s(WaterTemp_Celcius)+
                  s(scale(pH), k = 9) +
                  SpecificConductance_MicrosiemensPerCentimeter +
                  s(time, bs = "re", k = 8) +
                  s(LocationID, bs = "re", k = 20),
                data = mainstem_litho_data, 
                method ='REML',
                family = nb())
summary(fish_m_1)

plot(fish_m_1)


fish_m_2 <- lmer(Total ~ scale(low_discharge) +  scale(low_flow_duration) + scale(short_term_change) + scale(fall_rate) + (1|LocationID) + (1|Year),
                  data = mainstem_litho_data)
summary(fish_m_2)
residuals <- simulateResiduals(fittedModel = fish_m_2)
plot(residuals)

fish_m_3 <- lmer(Total ~ scale(low_discharge) +  scale(low_flow_duration) + scale(fall_rate) + (1|LocationID) + (1|Year),
                 data = mainstem_litho_data)

summary(fish_m_3)


fish_m_4 <- lmer(Total ~ scale(low_discharge) + scale(fall_rate) + (1|LocationID) + (1|Year),
                 data = mainstem_litho_data)

summary(fish_m_4)
fish_m_5 <- lmer(Total ~ scale(low_discharge) + (1|LocationID),
                 data = mainstem_litho_data)

summary(fish_m_5)
