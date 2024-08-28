
library(tidyr)
library(dplyr)
library(stringr)
library(vegan)


fish_count <- read.csv("HTLN_FishCommunities_FishCountsThru_2023_Cleaned.csv")

fish_count$ReachID <- gsub(".*FISH.", "",fish_count$LocationID)
fish_count <- fish_count %>%
  mutate(PeriodID = gsub("OZARRMFISH|OZARRMFish|OZRSSprngs|OZRSSPRNGS|EFMOStfish|PERIStfish|BUFFRMFISH|BUFFrmfish|WICRStfish|GWCAStfish|HEHOStfish|PIPEShiner|TAPRShiner|BUFFRMFISH|HOMEShiner|HOSPStfish", "", PeriodID)) %>%
  mutate(PeriodID = gsub("Sept", "Sep", PeriodID, ignore.case = TRUE)) %>%
  mutate(PeriodID = gsub("July", "JUL", PeriodID, ignore.case = TRUE)) %>%
  mutate(Year = as.integer(substr(PeriodID, 1, 4))) %>%
  mutate(Month = toupper(substr(PeriodID, 5, 7))) %>%
  mutate(Day = as.integer(substr(PeriodID, 8, 9)))

fish_count <- fish_count %>% 
  filter(ParkCode == "BUFF") %>%
  group_by( ReachID, CommonName) %>%
  summarise(count = sum(NumObs), .groups = 'drop')
  


fish_count<- fish_count %>%
  pivot_wider(names_from = CommonName,
              values_from = count,
              values_fill = list(count = 0)
  )


obs <- read.csv("observations_coordinates.csv")
obs <-obs %>%
  mutate(ReachID = ObservationCode)

buff <- "BUFF"


obs <- obs %>%
     mutate(ParkCode = case_when(
     str_detect(ReachID, buff) ~ "BUFF")) %>%
  filter(ParkCode == "BUFF") %>%
  select(ReachID, Latitude, Longitude) %>%
  arrange(desc(Longitude))

obs$ReachID <- sub("^.{4}", "", obs$ReachID)

fish_count <- full_join(obs, fish_count, by = "ReachID")


fish_count <- fish_count %>%
  arrange(desc(Longitude))

rownames(obs) <- obs$ReachID 
rownames(fish_count) <- fish_count$ReachID

obs <- obs %>%
  select(Latitude, Longitude)

fish_count <- fish_count %>%
  select(-ReachID, -Latitude, -Longitude)

write.csv(fish_count, "BUFF_fish_data_pivot.csv")
write.csv(obs, "BUFF_Reach_locations_EDA.csv")

plot(obs$Longitude, obs$Latitude, asp = 1, type = "n", main = "Reach Locations",
     xlab="X coordinate", ylab="ycoordinate")

lines(obs$Longitude, obs$Latitude, col = "light blue")

text(obs$Longitude, obs$Latitude, row.names(obs), cex =.5, col ="red")

par(mfrow=c(2,2))

scaling_factor <- 25

plot(obs$Longitude, obs$Latitude, asp = 1, col="brown", cex=fish_count$`Banded darter`/scaling_factor, main = "Banded Darter",
     xlab= "x coordinate", ylab = "y coordinate")
lines(obs$Longitude, obs$Latitude, col="light blue")

plot( obs$Longitude, obs$Latitude, asp = 1, col="brown", cex=fish_count$`Ozark chub`/scaling_factor, main = "Ozark Chub",
     xlab= "x coordinate", ylab = "y coordinate")
lines(obs$Longitude, obs$Latitude, col="light blue")

plot(obs$Longitude, obs$Latitude, asp = 1, col="brown", cex=fish_count$`Longnose gar`/scaling_factor, main = "Longnose Gar",
     xlab= "x coordinate", ylab = "y coordinate")
lines(obs$Longitude, obs$Latitude, col="light blue")

plot(obs$Longitude, obs$Latitude, asp = 1, col="brown", cex=fish_count$`Ozark shiner`/scaling_factor, main = "Ozark Shiner",
     xlab= "x coordinate", ylab = "y coordinate")
lines(obs$Longitude, obs$Latitude, col="light blue")



# Compute the number of Occurrences

#counts the number of reaches where each species is present
spe_pres <- apply(fish_count > 0, 2, sum)
#the output has species and the number of reaches where the species was present 
print(spe_pres)

sort(spe_pres)
spe_relf <- 100*spe_pres/nrow(fish_count)
round(sort(spe_relf), 1)
print(spe_relf)


par(mfrow = c(1,2),
    mar = c(5,4,5,2) + .1)

hist(spe_pres, main="Species Occurrences", right=FALSE, las = 1, xlab = "Number of occurrences", ylab = "Number of species",
     breaks = seq(0,36, by = 6), col = "bisque")
hist(spe_relf, main = "Species Relative Frequencies", right = FALSE,
     las = 1, xlab = "Frequency of occurrences (%)", ylab = "Number of species",
     breaks = seq(0,100, by = 10), col = "bisque")

#compare reaches: species richness
#compute the number of species at each site
site_pres <- apply(fish_count > 0 , 1, sum)
sort(site_pres)

par(mfrow = c(1,2))

plot(site_pres, las = 1, col = "white", main = "Species Richness and Reach Location",
     xlab = "Positions of sites along the river", ylab = "Species Richness")
text(site_pres, row.names(fish_count), cex = .8, col = "red")

plot(obs$Longitude, obs$Latitude, asp = 1, main = "Map of Species Richness", pch = 21,
     col = "white", bg = "brown", cex=5*site_pres/max(site_pres),
     xlab = "X coordinate", ylab = "y coordinate")
text(obs$Longitude, obs$Latitude, labels = row.names(fish_count), pos = 4, offset = 1, cex = .5)
lines(obs$Longitude, obs$Latitude, col = "light blue")

#species richness
N0 <- rowSums(fish_count > 0)
#Shannon entropy (low entropy means there is less uncertainty in 
#predicting the species of a randomly selected individual- one species may dominate the community.
#High entropy indicates greater uncertainty and higher diversity)
H <- diversity(fish_count)
#Shannon diversity number ( how many equally abundant species would produce the shannon entropy value; higher indicates high diversity 
# ex: N1 of 3 means that diversity of the community is equivalent to having ~ 3 equally abundant species)
N1 <- exp(H)
#Simpson diversity number( the probability two randomly selected individuals belong to different species 0 = infinite diversity 1 = one species)
N2 <- diversity(fish_count, "simpson")
# Pielou evenness ( value between 0 and 1 and indicates how evenly the individuals are distributed among the species present )
J <- H/log(N0)
#shannon's evenness (higher value indicates higher degree of evenness)
E1 <- N1/N0
#Simpson evenness ( a higher value indicates that the species in the community are relatively evenly distributed)
E2 <- N2/N0

div <- data.frame(N0,H,N1,N2,E1,E2,J)
round(div,3)
write.csv(div, file = "BUFF_SPECIES_RICHNESS_TABLE.csv")

