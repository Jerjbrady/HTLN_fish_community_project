library(tidyr)
library(dplyr)
library(stringr)
library(vegan)
library(lattice)
library(MASS)
library(lme4)
library(glmmTMB)
library(ggplot2)

fish_count <- read.csv("HTLN_FishCommunities_FishCountsThru_2023_Cleaned.csv")
flood_frequency <- read.csv("BUFF_flood_frequency.csv")
data_2008 <- read.csv("flood_events_BUFF2008.csv")
data_2009 <- read.csv("flood_events_BUFF2009.csv") 
data_2010 <- read.csv("flood_events_BUFF2010.csv")
data_2012 <- read.csv("flood_events_BUFF2012.csv")
data_2013 <- read.csv("flood_events_BUFF2013.csv")
data_2015 <- read.csv("flood_events_BUFF2015.csv")
data_2017 <- read.csv("flood_events_BUFF2017.csv")
data_2019 <- read.csv("flood_events_BUFF2019.csv") 
data_2021 <- read.csv("flood_events_BUFF2021.csv")
data_2023 <- read.csv("flood_events_BUFF2023.csv")

reach_data <- read.csv("Reach_and_gage_locations.csv")

flood_data <- bind_rows(data_2008,data_2009,data_2010,data_2012,
                        data_2013,data_2015,data_2017,data_2019,
                        data_2021,data_2023)
month_lookup <- c(
  JAN = "01", FEB = "02", MAR = "03", APR = "04", MAY = "05", JUN = "06",
  JUL = "07", AUG = "08", SEP = "09", OCT = "10", NOV = "11", DEC = "12"
)
reach_data <- reach_data %>%
  dplyr::select(NearestGageID, LocationID) %>%
  rename(ReachID = LocationID,"site_no" = NearestGageID)

flood_data <- flood_data %>%
  filter(site_no != "7056000") %>%  
  mutate(Site_date = paste(site_no, Year)) 

fish_count$ReachID <- gsub(".*FISH.", "",fish_count$LocationID)

fish_count <- fish_count %>%
  mutate(
    PeriodID = gsub(
      "OZARRMFISH|OZARRMFish|OZRSSprngs|OZRSSPRNGS|EFMOStfish|PERIStfish|BUFFRMFISH|BUFFrmfish|WICRStfish|GWCAStfish|HEHOStfish|PIPEShiner|TAPRShiner|BUFFRMFISH|HOMEShiner|HOSPStfish", 
      "", PeriodID
    ),
    PeriodID = gsub("Sept", "Sep", PeriodID, ignore.case = TRUE),
    PeriodID = gsub("July", "Jul", PeriodID, ignore.case = TRUE),
    PeriodID = gsub("MAY", "May", PeriodID, ignore.case = TRUE),
    PeriodID = gsub("OCT", "Oct", PeriodID, ignore.case = TRUE),
    Year = as.integer(substr(PeriodID, 1, 4)),
    Month = toupper(substr(PeriodID, 5, 7)),
    Month = month_lookup[Month],
    Day = sprintf("%02d", as.integer(substr(PeriodID, 8, 9))),
    Date = paste0(Year, "-", Month, "-", Day),
    ChannelType = gsub("Main", "main", ChannelType)
  )

new_flood <- new_flood %>%
  mutate(
    Date = sub("^[^-]*-","", group))

new_flood <- merge(flood_frequency, reach_data, by = "site_no")


############################ non Lith ##############################################
BUFF_final_data_non <- fish_count %>%
  filter(ParkCode == 'BUFF') %>%
  mutate(ReproductiveClassification = str_extract(ReproductiveClassification, "^[^-]*-[^-]*")) %>%
  filter(ReproductiveClassification == "Non-lithophilic spawner  ") %>%
  group_by(ReachID, Year) %>%
  summarise(Total_Count = sum(NumObs)) 

BUFF_final_data_non <- merge(BUFF_final_data_non, reach_data, by = "ReachID") 

BUFF_final_data_non <- BUFF_final_data_non %>%
    mutate(Site_date = paste(NearestGageID, Year))
  
BUFF_final_data_non <- merge(BUFF_final_data_non, flood_data, by = "Site_date")

#######################  lith ###############################################
BUFF_final_data_lith <- fish_count %>%
  filter(ParkCode == 'BUFF') %>%
  mutate(ReproductiveClassification = str_extract(ReproductiveClassification, "^[^-]+")) %>%
  filter(ReproductiveClassification == "Lithophilic spawner ")%>%
  filter(CommonName == "Rainbow darter") %>%
  #filter(Year %in% c(2008, 2009, 2010, 2013, 2015, 2017, 2019, 2021, 2023)) %>%
  group_by(Year, ReachID) %>%
  summarise(Total_Count = sum(NumObs),
            across(Date, first, .names = "{.col}"))


BUFF_final_data_lith <- BUFF_final_data_lith %>%
  filter(ReachID %in% c("M01", "M02", "M03","M04","M05","M06", "T07", "T15", "T19", "T20"
                      ))


BUFF_final_data_lith <- merge(BUFF_final_data_lith, new_flood, by = c("ReachID", "Date")) 

BUFF_final_data_lith <- BUFF_final_data_lith %>%
  mutate(Site_date = paste(NearestGageID, Year))

BUFF_final_data_lith <- merge(BUFF_final_data_lith, flood_frequency, by = "Site_date")
  

########################## BUFF_litho  ###############################################



BUFF_data_for_GLM_lith <- BUFF_final_data_lith %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) 

BUFF_data_for_GLM_lith$Flood_Category <- as.factor(BUFF_data_for_GLM_lith$Flood_Category)
BUFF_data_for_GLM_lith$Flood_Category <- relevel(BUFF_data_for_GLM_lith$Flood_Category, ref = "None")

# Calculate the mean of the column
mean_total_count <- mean(BUFF_data_for_GLM_lith$Total_Count, na.rm = TRUE)

# Calculate the standard deviation of the column
sd_total_count <- sd(BUFF_data_for_GLM_lith$Total_Count, na.rm = TRUE)

# Display the results
mean_total_count
sd_total_count

species_richness_model <-glmer(
  Total_Count ~ scale(log(max_discharge)) +  (1 | ReachID) + (1 | Year),
  data = BUFF_final_data_lith,
  family = poisson(link = "log")
) 

summary(species_richness_model)

model_nb_time <- glmmTMB(
  Total_Count ~ Year + (1 + ReachID),
  data = BUFF_final_data_lith ,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)

model_nb <- glmmTMB(
  Total_Count ~ scale(log(max_gage)) + scale(log(max_discharge)) + max_days + (1 | ReachID) + (1 | Year),
  data = BUFF_final_data_lith ,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)

summary(model_nb)

ggplot(BUFF_final_data_lith , aes( x = Year, y = Total_Count)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  geom_line(aes(group = ReachID)) + facet_wrap(~ReachID,  ncol=5)

ggplot(BUFF_final_data_lith, aes( x = max_gage, y = Total_Count)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  geom_line(aes(group = ReachID)) + facet_wrap(~ReachID,  ncol=5)

ggplot(BUFF_data_for_GLM_lith, aes( x = Flood_Category, y = Total_Count)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  geom_line(aes(group = ReachID)) + facet_wrap(~ReachID,  ncol=5) +
  scale_x_discrete(limits = c("None", "Mild", "Moderate", "High"))



########################## BUFF_non-litho ##############################################


BUFF_data_for_GLM_non <- BUFF_final_data_non %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) 

BUFF_data_for_GLM_non$Flood_Category <- as.factor(BUFF_data_for_GLM_non$Flood_Category)
BUFF_data_for_GLM_non$Flood_Category <- relevel(BUFF_data_for_GLM_non$Flood_Category, ref = "None")

model_nb <- glmmTMB(
  Total_Count ~  scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category + (1| Year.x),
  data = BUFF_data_for_GLM_non,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)
summary(model_nb)

ggplot(BUFF_data_for_GLM_non, aes( x = Flood_Category, y = Total_Count)) +
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) +
  geom_line(aes(group = ReachID)) + facet_wrap(~ReachID,  ncol=5)

############################ main #######################################################
BUFF_main_data_lith <- BUFF_final_data_lith %>%
  filter(grepl("M", ReachID))


Main_BUFF_data_for_GLM_lith <- BUFF_main_data_lith %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) 

Main_BUFF_data_for_GLM_lith$Flood_Category <- as.factor(Main_BUFF_data_for_GLM_lith$Flood_Category)
Main_BUFF_data_for_GLM_lith$Flood_Category <- relevel(Main_BUFF_data_for_GLM_lith$Flood_Category, ref = "None")


model_nb <- glmmTMB(
  Total_Count ~  scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category + (1 | ReachID) + (1 | Year.x),
  data = Main_BUFF_data_for_GLM_lith ,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)
summary(model_nb)

############################ tibs ######################################################

BUFF_tib_data_lith <- BUFF_final_data_lith %>%
  filter(grepl("M", ReachID))


Tib_BUFF_data_for_GLM_lith <- BUFF_tib_data_lith %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) 

Tib_BUFF_data_for_GLM_lith$Flood_Category <- as.factor(Tib_BUFF_data_for_GLM_lith$Flood_Category)
Tib_BUFF_data_for_GLM_lith$Flood_Category <- relevel(Tib_BUFF_data_for_GLM_lith$Flood_Category, ref = "None")


model_nb <- glmmTMB(
  Total_Count ~  scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category + (1 | ReachID) + (1 | Year.x),
  data = Tib_BUFF_data_for_GLM_lith ,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)
summary(model_nb)





########################## BUFF_litho MO4 ###############################################


BUFF_data_for_GLM_lith_ordered <- BUFF_final_data_lith %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) %>%
  arrange(desc(Total_Count))

ggplot()


BUFF_data_for_GLM_lith_M4 <- BUFF_final_data_lith %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) %>%
  filter(ReachID == "M04")

BUFF_data_for_GLM_lith_M4$Flood_Category <- as.factor(BUFF_data_for_GLM_lith_M4$Flood_Category)
BUFF_data_for_GLM_lith_M4$Flood_Category <- relevel(BUFF_data_for_GLM_lith_M4$Flood_Category, ref = "None")



model_1 <- lmer(Total_Count ~ 1 + (1| ReachID), data = BUFF_data_for_GLM_lith_M4 , REML = FALSE)
 

null_model <- glmmTMB(Total_Count ~ 1 + (1 | ReachID) + (1 | Year.x),
                      data = BUFF_data_for_GLM_lith_M4,
                      family = nbinom2())

summary(null_model)
model_nb <- glmmTMB(
  Total_Count ~  scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category +  (1| Year.x),
  data = BUFF_data_for_GLM_lith_M4,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)
summary(model_nb)


########################## BUFF_non-litho MO4 ###############################################


BUFF_data_for_GLM_non_M4 <- BUFF_final_data_non %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) %>%
  filter(ReachID == "M04")


# Calculate the mean of the column
mean_total_count <- mean(BUFF_data_for_GLM_non_M4$Total_Count, na.rm = TRUE)

# Calculate the standard deviation of the column
sd_total_count <- sd(BUFF_data_for_GLM_non_M4$Total_Count, na.rm = TRUE)

# Display the results
mean_total_count
sd_total_count

BUFF_data_for_GLM_non_M4$Flood_Category <- as.factor(BUFF_data_for_GLM_non_M4$Flood_Category)
BUFF_data_for_GLM_non_M4$Flood_Category <- relevel(BUFF_data_for_GLM_non_M4$Flood_Category, ref = "None")

model_nb <- glmmTMB(
  Total_Count ~  scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category + (1| Year.x),
  data = BUFF_data_for_GLM_non_M4,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)
summary(model_nb)


########################## BUFF_litho T20 ###############################################



BUFF_data_for_GLM_lith_T20 <- BUFF_final_data_lith %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) %>%
  filter(ReachID == "T20")

BUFF_data_for_GLM_lith_T20$Flood_Category <- as.factor(BUFF_data_for_GLM_lith_T20$Flood_Category)
BUFF_data_for_GLM_lith_T20$Flood_Category <- relevel(BUFF_data_for_GLM_lith_T20$Flood_Category, ref = "None")

model_nb <- glmmTMB(
  Total_Count ~  scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category +  (1| Year.x),
  data = BUFF_data_for_GLM_lith_T20,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)

summary(model_nb)
########################## BUFF_litho T19 ###############################################


BUFF_data_for_GLM_lith_T19 <- BUFF_final_data_lith %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) %>%
  filter(ReachID == "T19")

BUFF_data_for_GLM_lith_T19$Flood_Category <- as.factor(BUFF_data_for_GLM_lith_T19$Flood_Category)
BUFF_data_for_GLM_lith_T19$Flood_Category <- relevel(BUFF_data_for_GLM_lith_T19$Flood_Category, ref = "None")

model_nb <- glmmTMB(
  Total_Count ~  scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category +  (1| Year.x),
  data = BUFF_data_for_GLM_lith_T19,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)

summary(model_nb)
########################## BUFF_litho T15 ###############################################


BUFF_data_for_GLM_lith_T15 <- BUFF_final_data_lith %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) %>%
  filter(ReachID == "T15")

BUFF_data_for_GLM_lith_T15$Flood_Category <- as.factor(BUFF_data_for_GLM_lith_T15$Flood_Category)
BUFF_data_for_GLM_lith_T15$Flood_Category <- relevel(BUFF_data_for_GLM_lith_T15$Flood_Category, ref = "None")

model_nb <- glmmTMB(
  Total_Count ~  scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category +  (1| Year.x),
  data = BUFF_data_for_GLM_lith_T15,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)

summary(model_nb)
########################## BUFF_litho MO4 ###############################################



BUFF_data_for_GLM_lith_T20 <- BUFF_final_data_lith %>%
  mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_Year = paste( ReachID, Year.x)) %>%
  filter(ReachID == "T20")

BUFF_data_for_GLM_lith_T20$Flood_Category <- as.factor(BUFF_data_for_GLM_lith_T20$Flood_Category)
BUFF_data_for_GLM_lith_T20$Flood_Category <- relevel(BUFF_data_for_GLM_lith_T20$Flood_Category, ref = "None")

model_nb <- glmmTMB(
  Total_Count ~  scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category +  (1| Year.x),
  data = BUFF_data_for_GLM_lith_T20,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)

summary(model_nb)
###################  models #########################



mixed_lmer_flood <- glmer(Total_Count ~ Flood_Category + (1|ReachID) + (1|Year.x), data = BUFF_data_for_GLM_lith, family = poisson)
summary(mixed_lmer_flood)
# Extract residuals
residuals_model <- residuals(mixed_lmer_flood, type = "pearson")

# Plot residuals
plot(fitted(mixed_lmer_flood), residuals_model)
abline(h = 0, col = "red")


mixed_lmer_transformed_discharge <-glmer(Total_Count ~ scale(log(Flow)) + (1|ReachID) + (1|Year.x), data = BUFF_data_for_GLM_lith, family = poisson)
summary(mixed_lmer_transformed_discharge)
# Extract residuals
residuals_model <- residuals(mixed_lmer_transformed_discharge, type = "pearson")

# Plot residuals
plot(fitted(mixed_lmer_transformed_discharge), residuals_model)
abline(h = 0, col = "red")

mixed_lmer_Height <- glmer(Total_Count ~ Max_Height + (1|ReachID) + (1|Year.x), data = BUFF_data_for_GLM_lith, family = poisson)
summary(mixed_lmer_Height)
# Extract residuals
residuals_model <- residuals(mixed_lmer_Height, type = "pearson")

# Plot residuals
plot(fitted(mixed_lmer_Height), residuals_model)
abline(h = 0, col = "red")

mixed_lmer_transformed_height <- glmer(Total_Count ~ scale(log(Max_Height)) + (1|ReachID) + (1|Year.x), data = BUFF_data_for_GLM_lith, family = poisson)
summary(mixed_lmer_transformed_height)
# Extract residuals
residuals_model <- residuals(mixed_lmer_transformed_height , type = "pearson")

# Plot residuals
plot(fitted(mixed_lmer_transformed_height), residuals_model)
abline(h = 0, col = "red")

mixed_lmer_two_predictors <- glmer(Total_Count ~ scale(log(Max_Height)) + scale(log(Flow)) + (1|ReachID) + (1|Year.x), data = BUFF_data_for_GLM_lith, family = poisson) 
summary(mixed_lmer_two_predictors)
# Extract residuals
residuals_model <- residuals(mixed_lmer_two_predictors, type = "pearson")

# Plot residuals
plot(fitted(mixed_lmer_two_predictors), residuals_model)
abline(h = 0, col = "red")

mixed_lmer_three_predictors <- glmer(Total_Count ~ scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category + (1|ReachID) + (1|Year.x), data = BUFF_data_for_GLM_lith, family = poisson) 
summary(mixed_lmer_three_predictors)

# Extract residuals
residuals_model <- residuals(mixed_lmer_three_predictors , type = "pearson")

# Plot residuals
plot(fitted(mixed_lmer_three_predictors ), residuals_model)
abline(h = 0, col = "red")
# Calculate overdispersion
overdispersion <- sum(residuals_model^2) / df.residual(mixed_lmer_three_predictors)

residual_deviance <- deviance(mixed_lmer_three_predictors)
df_residual <- df.residual(mixed_lmer_three_predictors)

# Calculate overdispersion
overdispersion <- residual_deviance / df_residual

# Extract residuals and fitted values
residuals_model <- residuals(model, type = "pearson")
fitted_values <- fitted(model)

# Residuals vs. Fitted Values Plot
ggplot(data = data.frame(fitted = fitted_values, residuals = residuals_model), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# Q-Q Plot of Residuals
qqnorm(residuals_model)
qqline(residuals_model, col = "red")



# Print exponentiate coefficients
print(exp_coefs)

model_summary <- capture.output(summary(model))

# Write to a text file
writeLines(model_summary, "model_summary.txt")

BUFF_final_data_non$Flood_Category <- relevel(BUFF_final_data_non$Flood_Category, ref = "None")

model_nb <- glmmTMB(
  Total_Count ~  scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category +  (1|ReachID) + (1| Year.x),
  data = BUFF_data_for_GLM_lith_M4,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)
model_nb <- glmmTMB(
  Total_Count ~ scale(log(Max_Height)) + scale(log(Flow)) + Flood_Category +  (1|ReachID) + (1|Year.x),
  data = BUFF_data_for_GLM_lith,
  family = nbinom2() # Use nbinom2() for negative binomial with log link
)
summary(model_nb)

dotwhisker::dwplot(model_nb, effects = "fixed") + geom_vline(xintercept = 0, lty = 2)

AIC(mixed_lmer_three_predictors, model_nb)

fitted_values_nb <- fitted(model_nb)
residuals_nb <- residuals(model_nb)
plot(fitted_values_nb, residuals_nb, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values for Negative Binomial Model")


########################## BUFF: non- lithophilic ################################################




BUFF_final_data_non <- merge(BUFF_final_data_non, reach_data, by = "ReachID") 

BUFF_final_data_non <- BUFF_final_data_non %>%
  mutate(Site_date = paste(NearestGageID, Year))

BUFF_final_data_non <- merge(BUFF_final_data_non, flood_data, by = "Site_date")

  BUFF_final_data_non <- BUFF_final_data_non %>%
    mutate(Flood_Category = case_when(
    flood_count == 0 ~ "None",
    flood_count > 0 & flood_count <= 5 ~ "Mild",
    flood_count > 5 & flood_count <= 10 ~ "Moderate",
    flood_count > 10 ~ "High"
  )) %>%
  mutate(Reach_year= paste( ReachID, Year.x))



BUFF_final_data_non$Flood_Category <- as.factor(BUFF_final_data_non$Flood_Category)





############################# Visualizations ###################################

interaction.plot(
  x.factor     = BUFF_data_for_GLM_lith$Flood_Category,
  trace.factor = BUFF_data_for_GLM_lith$ReachID,
  response     = BUFF_data_for_GLM_lith$Total_Count,
  fun          = mean,
  type         = "b",
  col          = c("black", "red", "green", "blue", "yellow", "cyan", "magenta", "gray", "orange", "purple", "brown", "pink", "sienna", "burlywood", "maroon", "navy", "lightgray", "darkgray", "gold", "salmon", 
                   "seagreen", "turquoise", "olivedrab", "violet", "aquamarine"),
  pch          = c(19, 17, 15, 8, 7, 5, 3, 2, 1, 6, 4, 9), # Example symbols
  fixed        = TRUE,
  leg.bty      = "o"
)


# Example code to create histograms with faceting
ggplot(OZAR_data_for_GLM,
       aes(log10(Total_Count), fill = ReproductiveClassification)) +
  geom_density(position = "dodge",
               alpha = .6)

ggplot(BUFF_data_for_GLM,
       aes(log10(Total_Count), fill = ReproductiveClassification)) +
  geom_density(position = "dodge",
               alpha = .6)


(prelim_plot <- ggplot(BUFF_data_for_GLM_lith, aes(x = Max_Height, y = Total_Count)) +
    geom_point() +
    geom_smooth(method = "lm"))

boxplot(Total_Count ~ ReachID, data = BUFF_data_for_GLM_lith_tibs)  

(colour_plot <- ggplot(BUFF_data_for_GLM_lith, aes(x = Max_Height, y = Total_Count, colour = ReachID)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))
(split_plot <- ggplot(aes(Max_Height, Total_Count), data = BUFF_data_for_GLM_lith) + 
    geom_point() + 
    facet_wrap(~ ReachID) + # create a facet for each mountain range
    xlab("Height") + 
    ylab("Total Count"))



library(ggplot2)

pd = position_dodge(.2)

ggplot(BUFF_data_for_GLM_lith, aes(x =    Year.x,
                y =    Total_Count,
                color = ReachID)) +
                
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  ylab("Mean calories per day")
