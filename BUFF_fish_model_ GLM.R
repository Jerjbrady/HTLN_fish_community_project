####################### BUFF River Fish / Climate Model #####################
####################### By: Jeremy Brady      ###############################
####################### Date: 09/20/2024      ###############################


###################### load libraries #######################################
library(tidyr)
library(dplyr)
library(stringr)
library(vegan)
library(lattice)
library(MASS)
library(DHARMa)
library(glmmTMB)
library(ggplot2)


############################# load data; be sure to change directory ###########
fish_count <- read.csv("HTLN_FishCommunities_FishCountsThru_2023_Cleaned.csv")

e_data <- read.csv("~/work/fish project/Fish data/main data/HTLN_FishCommunities_ReachMeasurements_Cleaned.csv")
fifteen_month <- read.csv("BUFF_flood_frequency_15_months.csv")
ten_month <- read.csv("BUFF_flood_frequency_10_months.csv")
five_month <- read.csv("BUFF_flood_frequency_5_months.csv")

USGS_gage_data <- read.csv("Reach_and_gage_locations.csv")
reach_and_location_key <- read.csv('Final_reach_location.csv')

################################################################################
###### we need to create a system to link the different data frames.############
###### we use ReachID and LocationID and Year as unique markers     ############
###### ReachID is the abbreviated name of the river followed by     ############
######  either a M or T then followed by a number. The M or T designate ########
###### if the reach is a main stem or tributary and the number represents ###### 
###### the unique identifier for the reach. High numbers represent  ############
###### downriver sites. LocationID contains the same data as reach ID ##########
###### but contains extra characters. Some of the data frames use locationID ###
###### to identify the reach and other data frames use reachID      ############
###### The following code unifies all the data frames to avoid mismatching #####
###### data                                                         ############
################################################################################

###Reach and location key links the column LocationID to the column ReachID ####

reach_and_location_key <- reach_and_location_key %>%
  dplyr::select(ReachID, LocationID) 


####### USGS_gage_data contains columns NearestGageID, ReachID, loctaionID, ########        
####### latitude and longitude. We use the table to link USGS gage station #####
####### data to the nearest reach change the name of columns. NearestGageID
####### is the USGS gage number, ReachID###################
 
USGS_gage_data <- USGS_gage_data %>%
 dplyr::select(NearestGageID, ReachID) %>%
  rename("site_no" = NearestGageID)

######## match fish_count data locationID to ReachID  ##########################
fish_count <- inner_join(fish_count, reach_and_location_key, 
                         by = "LocationID", relationship = "many-to-many") 
######### this code converts month abbreviations to numbers. We create a list ##
######### with the month and number conversion #################################
month_lookup <- c(
  JAN = "01", FEB = "02", MAR = "03", APR = "04", MAY = "05", JUN = "06",
  JUL = "07", AUG = "08", SEP = "09", OCT = "10", NOV = "11", DEC = "12"
)

###### Clean date data by taking date info from PeriodID. This code creates ####
###### new columns Year, Month, Day, Date, and ChannelType  ####################

fish_count <- fish_count %>%
  mutate(
    PeriodID = gsub(
      "OZARRMFISH|OZARRMFish|OZRSSprngs|OZRSSPRNGS|EFMOStfish|PERIStfish|
       BUFFRMFISH|BUFFrmfish|WICRStfish|GWCAStfish|HEHOStfish|PIPEShiner|
       TAPRShiner|BUFFRMFISH|HOMEShiner|HOSPStfish", 
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

######### change -999 to NA for computations. Group by EventID ################# 
######### and get the daily mean for environmental data   ######################


e_data[e_data == -999] <- NA

e_data <- e_data %>%
  group_by(EventID, LocationID) %>%
  summarise(across(7:13, function(x) mean(x, na.rm = TRUE)))

e_data <- inner_join(e_data, reach_and_location_key, 
             by = "LocationID", relationship = "many-to-many") 


############################### flood data cleaning ###########################################


fifteen_month <- fifteen_month %>%
  dplyr::select(-X) %>%
  mutate(
    Date = sub("^[^-]*-","", group),
    site_no = substr( group, 2, 8)) %>%
  distinct(group, .keep_all = TRUE)


ten_month <- ten_month %>%
  dplyr::select( -X) %>%
  mutate(
    Date = sub("^[^-]*-","", group),
    site_no = substr( group, 2, 8)) %>%
  distinct(group, .keep_all = TRUE)

five_month <- five_month %>%
  dplyr::select( -X) %>%
  mutate(
    Date = sub("^[^-]*-","", group),
    site_no = substr( group, 2, 8)) %>%
  distinct(group, .keep_all = TRUE)


##### merge the climate data to create one data set called model_data #########

model_data_step_1 <- merge(fifteen_month, ten_month, by = "group")
model_data_step_2 <- merge(model_data_step_1, five_month, by = "group")

model_data <- merge(model_data_step_2, USGS_gage_data, by = "site_no")

fish_pca <- fish_count %>%
  filter(ParkCode == 'BUFF') %>%
  filter(Year %in% c(2008, 2009, 2010, 2013, 2015, 2017, 2019, 2021, 2023)) %>%
  group_by(Year, ReachID, EventID) %>%
  summarise(
    total_count= sum(NumObs, na.rm = TRUE),
    date = first(Date))
names(fish_pca) <- tolower(names(fish_pca))
names(e_data) <- tolower(names(e_data))
names(model_data) <- tolower(names(model_data))

# Perform the merge using lowercase column names
fish_count_step_1 <- merge(fish_pca, e_data, by = c("reachid", "eventid"))

# Keep only reaches surveyed each year
fish_count_step_2 <- fish_count_step_1 %>%
  filter(reachid %in% c("BUFFM01", "BUFFM02", "BUFFM03", "BUFFM04", "BUFFM05", "BUFFM06", "BUFFT15", "BUFFT19", "BUFFT20"))

# Attach climate data to fish data
PCA_data <- merge(fish_count_step_2, model_data, by = c("reachid", "date"))

# Remove unnecessary columns after merging
PCA_data <- PCA_data %>%
  dplyr::select(-date.x, -site_no.x, -site_no.y, -date.y)


######### select rainbow darter for analysis and drop dates with a lack of data #####
      
BUFF_filtered_darter <- fish_count %>%
  filter(ParkCode == 'BUFF') %>%
  mutate(ReproductiveClassification = str_extract(ReproductiveClassification, "^[^-]+")) %>%
  filter(CommonName == "Rainbow darter") %>%
  filter(Year %in% c(2008, 2009, 2010, 2013, 2015, 2017, 2019, 2021, 2023)) %>%
  group_by(Year, ReachID, EventID) %>%
  summarise(
    total_count= sum(NumObs, na.rm = TRUE),
    date = first(Date))


# Convert column names to lowercase for both data frames
names(BUFF_filtered_darter) <- tolower(names(BUFF_filtered_darter))
names(e_data) <- tolower(names(e_data))
names(model_data) <- tolower(names(model_data))
# Perform the merge using lowercase column names
BUFF_darter_step_1 <- merge(BUFF_filtered_darter, e_data, by = c("reachid", "eventid"))


##### keep reaches that team surveyed for each year ######
BUFF_darter_step_2 <- BUFF_darter_step_1 %>%
  filter(reachid %in% c("BUFFM01", "BUFFM02", "BUFFM03","BUFFM04","BUFFM05","BUFFM06", "BUFFT15", "BUFFT19", "BUFFT20"
                      ))
###### attach climate data with fish_data
BUFF_darter <- merge(BUFF_darter_step_2, model_data, by = c("reachid", "date"))


BUFF_darter <- BUFF_darter %>%
  dplyr:: select(- date.x, - site_no.x, - site_no.y, - date.y)

# Assuming your table is called 'my_table'
write.csv(BUFF_darter, "BUFF_darter.csv", row.names = FALSE)


##### environmental data PCA ##########################################

e_data_scaled <- scale(BUFF_darter[,7:12 ])
# Remove rows with NA or infinite values
e_data_scaled_clean <- na.omit(e_data_scaled)

# Run PCA
e_pca_result <- prcomp(e_data_scaled_clean, center = TRUE, scale. = FALSE)


summary(e_pca_result)

# Biplot of the first two principal components
biplot(e_pca_result, scale = 0)

# View the loadings
e_pca_result$rotation

cor(e_data_scaled_clean)
pairs(e_data_scaled_clean)
# Standardize the data
data_scaled <- scale(BUFF_darter[, c("max_discharge_15", "max_discharge_10","max_discharge_5",
                                 "max_gage_15", "max_gage_10", "max_gage_5",
                                 "min_discharge_15","min_discharge_10", "min_discharge_5",
                                 "min_gage_15", "min_gage_10", "min_gage_5")])

# Perform PCA
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Check the proportion of variance explained by each PC
summary(pca_result)

# Biplot of the first two principal components
biplot(pca_result, scale = 0)

# View the loadings
pca_result$rotation

cor(data_scaled, method = "spearman")
# Base R pairwise comparison
pairs(data_scaled)


combined_data <- cbind(BUFF_darter[, 7:12], BUFF_darter[, c("max_discharge_15", "max_discharge_10", "max_discharge_5",
                                                            "max_gage_15", "max_gage_10", "max_gage_5",
                                                            "min_discharge_15", "min_discharge_10", "min_discharge_5",
                                                            "min_gage_15", "min_gage_10", "min_gage_5")])

# Scale the combined data
data_scaled_2 <- scale(combined_data)
# Remove rows with NA or infinite values
data_scaled_2 <- na.omit(data_scaled_2)

# Run PCA
pca_result_2 <- prcomp(data_scaled_2, center = TRUE, scale. = FALSE)


summary(pca_result_2)

# Biplot of the first two principal components
biplot(pca_result_2, scale = 0)

# View the loadings
pca_result_2$rotation

correlation <- combined_data %>%
  dplyr::select(min_gage_15, min_discharge_15, max_gage_15, max_discharge_15, max_discharge_5, watertemp_celcius, dissolvedoxygen_milligramsperliter, ph, specificconductance_microsiemenspercentimeter )

cor(correlation)
pairs(correlation)
############################################### model selection ################

nb_fish_1d <- glmmTMB(total_count ~ scale(year) + scale(watertemp_celcius) + 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(max_discharge_5) + scale(min_discharge_15) + 
                        scale(min_gage_15) + (1|reachid) + (1|year),
                        family = nbinom2, data = BUFF_darter)
summary(nb_fish_1d)

# removed dissovled oxygen due to high p- value

nb_fish_2d <- glmmTMB(total_count ~ scale(year) + scale(watertemp_celcius) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(max_discharge_5) + scale(min_discharge_15) + 
                        scale(min_gage_15) +
                        (1|reachid) + (1|year),
                        family = nbinom2, data = BUFF_darter)
summary(nb_fish_2d)
# removed max_discharge_5
nb_fish_3d <- glmmTMB(total_count ~ scale(year) + scale(watertemp_celcius) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(min_discharge_15) + 
                        scale(min_gage_15) +
                        (1|reachid) + (1|year),
                        family = nbinom2, data = BUFF_darter)

summary(nb_fish_3d)

#remove pH
nb_fish_4d <- glmmTMB(total_count ~ scale(year) + scale(watertemp_celcius) + 
                        scale(specificconductance_microsiemenspercentimeter) + 
                        scale(min_discharge_15) + 
                        scale(min_gage_15) + (1|reachid) + (1|year),
                        family = nbinom2, data = BUFF_darter)
summary(nb_fish_4d)

#remove min_gage_15
nb_fish_5d <- glmmTMB(total_count ~ scale(year) + scale(watertemp_celcius) + 
                        scale(specificconductance_microsiemenspercentimeter) + 
                        scale(min_discharge_15) +
                        (1|reachid) + (1|year),
                        family = nbinom2, data = BUFF_darter)
summary(nb_fish_5d)

# removeconductance
nb_fish_6d <- glmmTMB(total_count ~ scale(year) + scale(watertemp_celcius) + 
                        scale(min_discharge_15) + 
                        scale(min_gage_15) + 
                        (1|reachid) + (1|year),
                        family = nbinom2, data = BUFF_darter)
summary(nb_fish_6d)


anova(nb_fish_1d, nb_fish_2d)
anova(nb_fish_2d, nb_fish_3d)
anova(nb_fish_3d, nb_fish_4d)
anova(nb_fish_4d, nb_fish_5d)
anova(nb_fish_5d, nb_fish_6d)


## best model##
summary(nb_fish_5d)

### check residuals

sim_res <- simulateResiduals(nb_fish_5d)
plot(sim_res)

######### select ozark for analysis and drop dates with a lack of data #####

BUFF_filtered_ozark_bass <- fish_count %>%
  filter(ParkCode == 'BUFF') %>%
  mutate(ReproductiveClassification = str_extract(ReproductiveClassification, "^[^-]+")) %>%
  filter(CommonName == "Ozark bass") %>%
  filter(Year %in% c(2008, 2009, 2010, 2013, 2015, 2017, 2019, 2021, 2023)) %>%
  group_by(Year, ReachID, EventID) %>%
  summarise(
    total_count= sum(NumObs, na.rm = TRUE),
    date = first(Date))

# Convert column names to lowercase for both data frames
names(BUFF_filtered_ozark_bass) <- tolower(names(BUFF_filtered_ozark_bass))
# Perform the merge using lowercase column names
BUFF_ozark_bass_step_1 <- merge(BUFF_filtered_ozark_bass, e_data, by = c("reachid", "eventid"))


##### keep reaches that team surveyed for each year ######
BUFF_ozark_bass_step_2 <- BUFF_ozark_bass_step_1 %>%
  filter(reachid %in% c("BUFFM01", "BUFFM02", "BUFFM03","BUFFM04","BUFFM05","BUFFM06", "BUFFT15", "BUFFT19", "BUFFT20"
  ))
###### attach climate data with fish_data
BUFF_ozark_bass <- merge(BUFF_ozark_bass_step_2, model_data, by = c("reachid", "date"))


BUFF_ozark_bass <- BUFF_ozark_bass %>%
  dplyr:: select(- date.x, - site_no.x, - site_no.y, - date.y)



############################################### model selection ################



nb_fish_1b <- glmmTMB(total_count ~  scale(year) + scale(watertemp_celcius) + 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(max_discharge_5) + scale(min_discharge_15) + 
                        scale(min_gage_15) + (1|reachid) + (1|year),
                        family = nbinom2, data = BUFF_ozark_bass)
summary(nb_fish_1b)

#remove year
nb_fish_2b <- glmmTMB(total_count ~  scale(watertemp_celcius) + 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(max_discharge_5) + scale(min_discharge_15) + 
                        scale(min_gage_15) + (1|reachid) + (1|year),
                        family = nbinom2, data = BUFF_ozark_bass)
summary(nb_fish_2b)

#remove min_gage_15
nb_fish_3b <-  glmmTMB(total_count ~  scale(watertemp_celcius) + 
                         scale(dissolvedoxygen_milligramsperliter) + 
                         scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                         scale(max_discharge_5) + scale(min_discharge_15) + 
                          (1|reachid) + (1|year),
                       family = nbinom2, data = BUFF_ozark_bass)
summary(nb_fish_3b)

#remove min-discharge_15
nb_fish_4b <- glmmTMB(total_count ~  scale(watertemp_celcius) + 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(max_discharge_5) + 
                        (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_ozark_bass)
summary(nb_fish_4b)
# remove max_discharge_5
nb_fish_5b <- glmmTMB(total_count ~  scale(watertemp_celcius) + 
                         scale(dissolvedoxygen_milligramsperliter) + 
                         scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                         (1|reachid) + (1|year),
                       family = nbinom2, data = BUFF_ozark_bass)
summary(nb_fish_5b)

#remove water temperature
nb_fish_6b <- glmmTMB(total_count ~ 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_ozark_bass)
summary(nb_fish_6b)

# remove pH
nb_fish_7b <- nb_fish_6b <- glmmTMB(total_count ~ 
                                      scale(dissolvedoxygen_milligramsperliter) + 
                                      scale(specificconductance_microsiemenspercentimeter) +
                                      (1|reachid) + (1|year),
                                    family = nbinom2, data = BUFF_ozark_bass)
summary(nb_fish_7b)


anova(nb_fish_1b, nb_fish_2b)
anova(nb_fish_2b, nb_fish_3b)
anova(nb_fish_3b, nb_fish_4b)
anova(nb_fish_4b, nb_fish_5b)
anova(nb_fish_5b, nb_fish_6b)
anova(nb_fish_6b, nb_fish_7b)

residuals_nb_fish_7b <- residuals(nb_fish_7b, type = "pearson")
fitted_values <- fitted(nb_fish_7b)
plot(fitted_values, residuals_nb_fish_7b, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

hist(residuals_nb_fish_7b, main = "Histogram of Residuals", xlab = "Residuals")

#### best model ####
summary(nb_fish_7b)

### check residuals
sim_res_2 <- simulateResiduals(nb_fish_7b)
plotQQunif(sim_res_2)

plot(sim_res_2)

######### select stoneroller for analysis and drop dates with a lack of data #####

BUFF_filtered_stoneroller <- fish_count %>%
  filter(ParkCode == 'BUFF') %>%
  mutate(ReproductiveClassification = str_extract(ReproductiveClassification, "^[^-]+")) %>%
  filter(CommonName == "Stoneroller spp.") %>%
  filter(Year %in% c(2008, 2009, 2010, 2013, 2015, 2017, 2019, 2021, 2023)) %>%
  group_by(Year, ReachID, EventID) %>%
  summarise(
    total_count= sum(NumObs, na.rm = TRUE),
    date = first(Date))

# Convert column names to lowercase for both data frames
names(BUFF_filtered_stoneroller) <- tolower(names(BUFF_filtered_stoneroller))
# Perform the merge using lowercase column names
BUFF_stoneroller_step_1 <- merge(BUFF_filtered_stoneroller, e_data, by = c("reachid", "eventid"))


##### keep reaches that team surveyed for each year ######
BUFF_stoneroller_2 <- BUFF_stoneroller_step_1 %>%
  filter(reachid %in% c("BUFFM01", "BUFFM02", "BUFFM03","BUFFM04","BUFFM05","BUFFM06", "BUFFT15", "BUFFT19", "BUFFT20"
  ))
###### attach climate data with fish_data
BUFF_stoneroller <- merge(BUFF_stoneroller_2, model_data, by = c("reachid", "date"))


BUFF_stoneroller <- BUFF_stoneroller %>%
  dplyr::select(-date.x, -site_no.x, -site_no.y, -date.y) %>%
  mutate(reachid = as.factor(reachid))




############################################### model selection ################



nb_fish_1s <- glmmTMB(total_count ~  scale(year) + scale(watertemp_celcius) + 
                                      scale(dissolvedoxygen_milligramsperliter) + 
                                      scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                                      scale(max_discharge_5) + scale(min_discharge_15) + 
                                      scale(min_gage_15) + (1|reachid) + (1|year),
                                    family = nbinom2, data = BUFF_stoneroller)
summary(nb_fish_1s)

# remove min_gage_15
nb_fish_2s <- glmmTMB(total_count ~  scale(year) + scale(watertemp_celcius) + 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(max_discharge_5) + scale(min_discharge_15) + (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_stoneroller)
summary(nb_fish_2s)

# remove max_discharge_5
nb_fish_3s <- glmmTMB(total_count ~  scale(year) + scale(watertemp_celcius) + 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                         scale(min_discharge_15) + (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_stoneroller)
summary(nb_fish_3s)

# remove conductance 
nb_fish_4s <- glmmTMB(total_count ~  scale(year) + scale(watertemp_celcius) + 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(ph) + 
                        scale(min_discharge_15) + (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_stoneroller)
summary(nb_fish_4s)

# remove water temperature
nb_fish_5s <- glmmTMB(total_count ~  scale(year) +
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(ph) + 
                        scale(min_discharge_15) + (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_stoneroller)
summary(nb_fish_5s)

# remove min_discharge_15
nb_fish_6s <- glmmTMB(total_count ~  scale(year) +
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(ph) + 
                        (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_stoneroller)
summary(nb_fish_6s)

nb_fish_7s <- glmmTMB(total_count ~  scale(year) +
                        scale(ph) + 
                        (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_stoneroller)
summary(nb_fish_7s)

nb_fish_8s <- glmmTMB(total_count ~  scale(year) +
                        (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_stoneroller)


anova(nb_fish_1s, nb_fish_2s)
anova(nb_fish_2s, nb_fish_3s)
anova(nb_fish_3s, nb_fish_4s)
anova(nb_fish_4s, nb_fish_5s)
anova(nb_fish_5s, nb_fish_6s)
anova(nb_fish_6s, nb_fish_7s)
anova(nb_fish_7s, nb_fish_8s)


## best model##
summary(nb_fish_7s)


sim_res_3 <- simulateResiduals(nb_fish_7s)
plot(sim_res_3)

######### select Knobfin sculpin for analysis and drop dates with a lack of data #####


BUFF_filtered_minnow <- fish_count %>%
filter(CommonName == "Ozark minnow") %>%
  filter(Year %in% c(2008, 2009, 2010, 2013, 2015, 2017, 2019, 2021, 2023)) %>%
  group_by(Year, ReachID, EventID) %>%
  summarise(
    total_count= sum(NumObs, na.rm = TRUE),
    date = first(Date))

# Convert column names to lowercase for both data frames
names(BUFF_filtered_minnow) <- tolower(names(BUFF_filtered_minnow))
# Perform the merge using lowercase column names
BUFF_minnow_step_1 <- merge(BUFF_filtered_minnow, e_data, by = c("reachid", "eventid"))


##### keep reaches that team surveyed for each year ######
BUFF_minnow_step_2 <- BUFF_minnow_step_1 %>%
  filter(reachid %in% c("BUFFM01", "BUFFM02", "BUFFM03","BUFFM04","BUFFM05","BUFFM06", "BUFFT15", "BUFFT19", "BUFFT20"
  ))
###### attach climate data with fish_data
BUFF_minnow <- merge(BUFF_minnow_step_2, model_data, by = c("reachid", "date"))


BUFF_minnow <- BUFF_minnow %>%
  dplyr::select(-date.x, -site_no.x, -site_no.y, -date.y) %>%
  mutate(reachid = as.factor(reachid))




############################################### model selection ################



nb_fish_1m <- glmmTMB(total_count ~ scale(year) + scale(watertemp_celcius) + 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(max_discharge_5) + scale(min_discharge_15) + 
                        scale(min_gage_15) + (1|reachid) + (1|year),
                        family = nbinom2, data = BUFF_minnow)
summary(nb_fish_1m)
# remove year
nb_fish_2m <- glmmTMB(total_count ~  scale(watertemp_celcius) + 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(max_discharge_5) + scale(min_discharge_15) + 
                        scale(min_gage_15) + (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_minnow)
summary(nb_fish_2m)

#remove discharge_5
nb_fish_3m <- glmmTMB(total_count ~  scale(watertemp_celcius) + 
                        scale(dissolvedoxygen_milligramsperliter) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(min_discharge_15) + 
                        scale(min_gage_15) + (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_minnow)
summary(nb_fish_3m)

# remove dissolved oxygen
nb_fish_4m <- glmmTMB(total_count ~  scale(watertemp_celcius) + 
                        scale(specificconductance_microsiemenspercentimeter) + scale(ph) + 
                        scale(min_discharge_15) + 
                        scale(min_gage_15) + (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_minnow)

summary(nb_fish_4m)

# remove conductance
nb_fish_5m <- glmmTMB(total_count ~  scale(watertemp_celcius) + 
                        scale(ph) + 
                        scale(min_discharge_15) + 
                        scale(min_gage_15) + (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_minnow)
summary(nb_fish_5m)

# remove min-discharge_15
nb_fish_6m <- glmmTMB(total_count ~  scale(watertemp_celcius) + 
                        scale(ph) +
                        scale(min_gage_15) + (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_minnow)
summary(nb_fish_6m)

#remove water temperatire
nb_fish_7m <- glmmTMB(total_count ~
                        scale(ph) +
                        scale(min_gage_15) + (1|reachid) + (1|year),
                      family = nbinom2, data = BUFF_minnow)
summary(nb_fish_7m)




anova(nb_fish_1m, nb_fish_2m)
anova(nb_fish_2m, nb_fish_3m)
anova(nb_fish_3m, nb_fish_4m)
anova(nb_fish_4m, nb_fish_5m)
anova(nb_fish_5m, nb_fish_6m)
anova(nb_fish_6m, nb_fish_7m)

#### best model ###
summary(nb_fish_6m)


sim_res_4 <- simulateResiduals(nb_fish_6m)
plot(sim_res_4)
