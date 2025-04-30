####################### OZAR River Fish / Climate Model #####################
####################### By: Jeremy Brady      ###############################
####################### Date: 09/20/2024      ###############################

install.packages("mvgam") 
install.packages("gratia")
###################### load libraries #######################################
library(tidyr)
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
library(gratia)

############################# load data; be sure to change directory ###########
fish_count <- read.csv("OZAR_fish_data.csv")

locationid <- c()

reproductive <- c()

locationid <- append(locationid, unique(fish_count$LocationID))
reproductive <- append(reproductive, unique(fish_count$ReproductiveClassification))



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
  filter(!LocationID %in% c(
    "OZARRMFISHCT12", "OZARRMFISHCM04", "OZARRMFISHCM05",
    "OZARRMFISHCT01", "OZARRMFISHCT02", "OZARRMFISHCT03", "OZARRMFISHCT04",
    "OZARRMFISHCT06", "OZARRMFISHCT07", "OZARRMFISHCT08", "OZARRMFISHCT09",
    "OZARRMFISHCT11", "OZARRMFISHCT13", "OZARRMFISHCT15",
    "OZARRMFISHJT01", "OZARRMFISHJT02", "OZRSSPRNGSSITE03"
  )) %>%
  # Include only certain LocationIDs
  filter(LocationID %in% c(locationid[1], locationid[2], locationid[3], locationid[6], locationid[16], locationid[17]))

mainstem_reproductive_data <- mainstem_reproductive_data %>%
  group_by(Year, LocationID, ReproductiveClassification, ScientificName) %>%
  summarize(
    Total = sum(NumObs, na.rm = TRUE),  # Summarize NumObs
    annual_mean = first(annual_mean),   # Assuming annual_mean is constant per group
    short_term_change = first(short_term_change), # Similarly, for other variables
    days_over_Q75 = first(days_over_Q75),
    high_flow_duration = first(high_flow_duration),
    fall_rate = first(fall_rate),
    low_discharge = first(low_discharge),
    low_flow_duration = first(low_flow_duration),
    .groups = "drop"                   # Ungroup the result
  )

#mainstem_litho_data <- mainstem_reproductive_data %>%
  #(ReproductiveClassification == "Lithophilic spawner")

#mainstem_non_litho_data <- mainstem_reproductive_data %>%
  #filter(ReproductiveClassification == "Non-lithophilic spawner")


series_data <- mainstem_reproductive_data %>%
  mutate(
    series = as.factor(ScientificName)
  ) 

#top_15 <- series_data %>%
 # count(series, sort = TRUE) %>%
  #head(15) %>%
  #pull(series)
  
#top_15_data <- series_data %>%
  #filter(series %in% top_15)

top_15_data <- series_data %>%
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
  



plot_mvgam_series(data = top_15_data, y = 'Total', series = 'all')

top_15_data_location <- series_data %>%
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

top_15_data_location %>% 
  dplyr::filter(series == 'Campostoma') %>%
  ggplot(aes(x = Year, y = Rel_abund)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(title = 'Campostoma',
       y = "relative abundance", 
       x = 'Year') +
  facet_wrap(~LocationID)
  
  
  
  
  portal_ts %>% 
  dplyr::filter(species == 'DM') %>%
  ggplot(aes(x = ndvi_ma12, y = qlogis(rel_abund))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(y = NULL, 
       x = 'NDVI moving average')

# PP
portal_ts %>% 
  dplyr::filter(species == 'PP') %>%
  ggplot(aes(x = mintemp, y = qlogis(rel_abund))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(title = 'PP',
       y = "logit(relative abundance)", 
       x = 'Minimum temperature') +
  
  portal_ts %>% 
  dplyr::filter(species == 'PP') %>%
  ggplot(aes(x = ndvi_ma12, y = qlogis(rel_abund))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(y = NULL, 
       x = 'NDVI moving average')

# PB
portal_ts %>% 
  dplyr::filter(species == 'PB') %>%
  ggplot(aes(x = mintemp, y = qlogis(rel_abund))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(title = 'PB',
       y = "logit(relative abundance)",
       x = 'Minimum temperature') +
  
  portal_ts %>% 
  dplyr::filter(species == 'PB') %>%
  ggplot(aes(x = ndvi_ma12, y = qlogis(rel_abund))) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10),
              col = 'darkred', fill = "#A25050") +
  labs(y = NULL, 
       x = 'NDVI moving average')




















ggplot(litho_data, aes( x = annual_mean, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

ggplot(litho_data, aes( x = short_term_change, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

ggplot(litho_data, aes( x = days_over_Q75, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

ggplot(litho_data, aes( x = low_discharge, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

ggplot(litho_data, aes( x = fall_rate, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

ggplot(non_litho_data, aes( x = annual_mean, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

ggplot(non_litho_data, aes( x = Year, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

ggplot(non_litho_data, aes( x = short_term_change, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

ggplot(non_litho_data, aes( x = days_over_Q75, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

ggplot(non_litho_data, aes( x = high_flow_duration, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

ggplot(non_litho_data, aes( x = fall_rate, y = Total)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~LocationID, ncol = 3)

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
mainstem_litho_data$year_time <- as.factor(mainstem_litho_data$Year)
mainstem_litho_data$Year <- scale(mainstem_litho_data$Year)

mainstem_litho_data <- mainstem_litho_data %>%
  group_by(LocationID, year_time) %>%
  summarize(
    mean = mean(Total)
  )

ggplot(mainstem_litho_data, aes(y= Total, x = low_discharge))+
  geom_point()

ggplot(mainstem_litho_data, aes(y= mean_total, x = fall_rate, color = year_time))+
  geom_point()



fish_m_1 <- gam(Total ~
                  s(low_discharge, k = 5),
                data = mainstem_litho_data,
                method ='REML',
                family = nb())


fish_m_2 <- gam(Total ~ Year +
                  s(low_discharge, k = 18) +
                  s(fall_rate, k = 15) + 
                  high_flow_duration +
                  s(year_time, bs = "re", k = 10),
                data = mainstem_litho_data,
                method ='REML',
                family = nb())


summary(fish_m_1)
summary(fish_m_2)



coef(fish_m_1)

plot(fish_m_1, residuals = TRUE)

plot(fish_m_2, residuals = TRUE, pch = 1)



k.check(fish_m_1)
gam.check(fish_m_1)



fish_m_2 <- glmer(Total ~ scale(low_discharge) +  scale(low_flow_duration) + scale(short_term_change) + scale(fall_rate) + (1|LocationID) + (1|Year),
                  data = litho_data, family = "poisson")
summary(fish_m_2)

# Assuming your model is fitted using gamm() from mgcv
residuals_gamm <- residuals(fish_m_1$lme, type = "normalized")  # for gamm()
residuals_gamm <- residuals(fish_m_1$gam, type = "pearson")     # for smooths

qqnorm(residuals_gamm)
qqline(residuals_gamm, col = "red")

# removed max_discharge_5
fish_m_3 <- glmer(Total ~ scale(Year) +   (1|LocationID) + (1|Year),
                  family = "poisson", data = litho_data)

summary(fish_m_3 )
anova(nb_fish_1d, nb_fish_2d)
anova(nb_fish_2d, nb_fish_3d)


### check residuals
sim_res_2 <- simulateResiduals(fish_m_2)
plotQQunif(sim_res_2)

plot(sim_res_2)

fish_m_1 <- glmer(Total ~ scale(Year) + scale(high_flow_duration) + scale(fall_rate) + (1|LocationID) + (1|Year),
                  family = "poisson", data = non_litho_data)
summary(fish_m_1)

# removed dissovled oxygen due to high p- value

fish_m_2 <- glmer(Total ~ scale(Year) +  scale(fall_rate) + (1|LocationID) + (1|Year),
                  family = "poisson", data = non_litho_data)

summary(fish_m_2)
# removed max_discharge_5
fish_m_3 <- glmer(Total ~ scale(Year) +   (1|LocationID) + (1|Year),
                  family = "poisson", data = non_litho_data)
summary(fish_m_3)
