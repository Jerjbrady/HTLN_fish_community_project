####################### TAPR River Fish / Climate Model #####################
####################### By: Jeremy Brady      ###############################
####################### Date: 09/20/2024      ###############################

install.packages("lme4")
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

############################# load data; be sure to change directory ###########
fish_count <- read.csv("TAPR_fish_data.csv")

locationid <- c()
reproductive <- c()
fish_species <- c()
fish_count <- fish_count %>%
  filter(!is.na(ScientificName))

locationid <- append(locationid, unique(fish_count$LocationID))
reproductive <- append(reproductive, unique(fish_count$ReproductiveClassification))
fish <- append(fish_species, unique(fish_count$ScientificName))

reproductive <- append(reproductive, unique(fish_count$ReproductiveClassification))

reproductive_data <- fish_count %>%
  mutate(
    ReproductiveClassification = case_when(
      ReproductiveClassification == reproductive[1] ~ "Lithophilic spawner",
      ReproductiveClassification == reproductive[2] ~ "Non-lithophilic spawner",
      ReproductiveClassification == reproductive[3] ~ "Unknown",
      ReproductiveClassification == reproductive[4] ~ NA
    )
  ) %>%
#drop rows where classification is NA or unknown
  filter(!is.na(ReproductiveClassification)) %>%
  filter(!ReproductiveClassification == "Unknown")

reproductive_data <- reproductive_data %>%
  group_by(Year, LocationID) %>%
  mutate(Total_Group = sum(NumObs, na.rm = TRUE)) %>%  # Calculate total NumObs for the group
  group_by(Year, LocationID, ReproductiveClassification, ScientificName) %>%
  reframe(
    Total = sum(NumObs, na.rm = TRUE),
    Relative_abun = Total / first(Total_Group),       # Use Total_Group for relative abundance
    annual_mean = first(annual_mean),                 # Assuming annual_mean is constant per group
    short_term_change = first(short_term_change),     # Similarly, for other variables
    days_over_Q75 = first(days_over_7xmedian),
    high_flow_duration = first(high_flow_duration),
    fall_rate = first(fall_rate),
    EventID = first(EventID)                                 # Ungroup the result
  ) 



litho_data <- reproductive_data %>%
  filter(ReproductiveClassification == "Lithophilic spawner")

non_litho_data <- reproductive_data %>%
  filter(ReproductiveClassification == "Non-lithophilic spawner")

reproductive_data <- reproductive_data %>%
  filter(!is.na(ScientificName))
print(fish)

for (i in fish) {
  for (j in 7:11) {
    col_name <- names(reproductive_data)[j]
    
    # Filter the data for the current species
    filtered_data <- reproductive_data %>% filter(ScientificName == i)
    
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



series_data <- reproductive_data %>%
  mutate(
    series = as.factor(ScientificName)
  ) 

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
         time = as.numeric(factor(Year, levels = sort(unique(Year))))) %>%
  group_by( series ) %>%
           mutate(
             year_count = n_distinct(Year)
           ) %>%
  filter(year_count > 5)




plot_mvgam_series(data = top_data, y = 'Rel_abund', series = 'all')

##### test for correlation ###

correlation <- litho_data %>%
  dplyr::select(annual_mean, 
                high_flow_duration, 
                fall_rate) %>%
  distinct() 

cor_matrix <- correlate(correlation)

corrplot(correlation, method = "circle")

print(correlation)


############################################### model selection ################

litho_data$LocationID <- as.factor(litho_data$LocationID)
litho_data$Year <- as.factor(litho_data$Year)

fish_m_1 <- gam(Total ~ s(fall_rate) + s(high_flow_duration) + s(LocationID, bs = "re") + s(Year, bs = "re"), family = poisson, data = litho_data)
summary(fish_m_1)

plot(fish_m_1)

# removed dissovled oxygen due to high p- value

fish_m_2 <- lmer(Total ~ scale(high_flow_duration) + scale(fall_rate) + (1|LocationID) + (1|Year),
                  data = litho_data)

summary(fish_m_2)
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
