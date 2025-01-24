# Load libraries
install.packages("sf")
install.packages("sp")
install.packages("geosphere")
library(dplyr)
library(tidyr)
library(sf)
library(sp)
library(geosphere)

# Load data
gage_data <- read.csv("Final_gage_location.csv")
reach_coords <- read.csv("OZAR_coordinates.csv")


OZAR_gage_data <- gage_data %>%
   filter(ParkCode == "OZAR") 


reach_coords <- reach_coords %>%
  #mutate(LocationID = gsub(".*FISH.", "", LocationID)) %>%
  filter(ParkCode == "OZAR") %>% drop_na()


# convert to spatial point

gages_sf <- st_as_sf(OZAR_gage_data, coords = c("Longitude", "Latitude"), crs = 4326)

reach_locations_sf <- st_as_sf(reach_coords, coords = c( "Longitude","Latitude"), crs = 4326)

# distance matrix

distance_matrix <- st_distance(reach_locations_sf, gages_sf)

print(distance_matrix)

# find nearest gage for each reach
nearest_gage_index <- apply(distance_matrix, 1, which.min)
nearest_gage_id <- OZAR_gage_data$GaugeSiteNo[nearest_gage_index]

nearest_gage_index
nearest_gage_id

reach_gage_mapping <- data.frame(
  LocationID = reach_coords$LocationID,
  NearestGageID = nearest_gage_id
)

final_data <- merge(reach_gage_mapping, reach_coords, by = "LocationID")

write.csv(final_data, "OZAR_reach_and_gage_locations.csv")
