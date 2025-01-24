####################### OZAR River Fish / Climate Model #####################
####################### By: Jeremy Brady      ###############################
####################### Date: 09/20/2024      ###############################

install.packages("mgcv")
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
  group_by(Year, LocationID, ReproductiveClassification) %>%
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

mainstem_litho_data <- mainstem_reproductive_data %>%
  filter(ReproductiveClassification == "Lithophilic spawner")

mainstem_non_litho_data <- mainstem_reproductive_data %>%
  filter(ReproductiveClassification == "Non-lithophilic spawner")

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
mainstem_litho_data$Year <- as.factor(mainstem_litho_data$Year)

fish_m_1 <- gam(Total ~ s(low_discharge) + s(fall_rate) + s(short_term_change), data = mainstem_litho_data)
summary(fish_m_1)


plot(fish_m_1)
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
