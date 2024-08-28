install.packages("vegan")
library (vegan)
library(tidyr)
library(dplyr)

fish_reach <- read.csv("HTLN_FishCommunities_FishCountsThru_2023_Cleaned.csv")

fish_reach$Reach <- gsub(".*FISH", "",fish_reach$LocationID)
fish_reach <- fish_reach %>%
  mutate(PeriodID = gsub("OZARRMFISH|OZARRMFish|OZRSSprngs|OZRSSPRNGS|EFMOStfish|PERIStfish|BUFFRMFISH|BUFFrmfish|WICRStfish|GWCAStfish|HEHOStfish|PIPEShiner|TAPRShiner|BUFFRMFISH|HOMEShiner|HOSPStfish", "", PeriodID)) %>%
  mutate(PeriodID = gsub("Sept", "Sep", PeriodID, ignore.case = TRUE)) %>%
  mutate(PeriodID = gsub("July", "JUL", PeriodID, ignore.case = TRUE)) %>%
  mutate(Year = as.integer(substr(PeriodID, 1, 4))) %>%
  mutate(Month = toupper(substr(PeriodID, 5, 7))) %>%
  mutate(Day = as.integer(substr(PeriodID, 8, 9)))


########################### FILTER OZAR DATA ################################
OZAR_fish_reach <- fish_reach %>% 
  filter(ParkCode == "OZAR") %>%
  group_by(ParkCode, Year, Reach, CommonName) %>%
  summarise(count = sum(NumObs), .groups = 'drop') %>%
  select(ParkCode, Year, CommonName, Reach, count)


  OZAR_fish_reach <- OZAR_fish_reach %>%
    pivot_wider(names_from = CommonName,
              values_from = count,
              values_fill = list(count = 0)
              )
               ############################################
               #  ANALYSIS OF SIMILARITY (ANOSIM) OZAR    #
               ############################################
OZAR_reach_dist <- vegdist(OZAR_fish_reach[, 4:ncol(OZAR_fish_reach)])

attach(OZAR_fish_reach)

OZAR_reach_anosim <- anosim(OZAR_reach_dist, Reach)

summary (OZAR_reach_anosim)


#################### FILTER BUFF DATA ###################################
BUFF_fish_reach <- fish_reach %>% 
  filter(ParkCode == "BUFF") %>%
  group_by(ParkCode, Year, Reach, CommonName) %>%
  summarise(count = sum(NumObs), .groups = 'drop') %>%
  select(ParkCode, Year, CommonName, Reach, count)


BUFF_fish_reach <- BUFF_fish_reach %>%
  pivot_wider(names_from = CommonName,
              values_from = count,
              values_fill = list(count = 0)
            )
              
               ############################################
               # ANALYSIS OF SIMILARITY (ANOSIM) BUFF     #
               ############################################
BUFF_reach_dist <- vegdist(BUFF_fish_reach[, 4:ncol(BUFF_fish_reach)])

attach(BUFF_fish_reach)

BUFF_reach_anosim <- anosim(BUFF_reach_dist, Reach)

summary (BUFF_reach_anosim)


########################### FILTER PIPE DATA ################################
PIPE_fish_reach <- fish_reach %>% 
  filter(ParkCode == "PIPE") %>%
  group_by(ParkCode, Year, Reach, CommonName) %>%
  summarise(count = sum(NumObs), .groups = 'drop') %>%
  select(ParkCode, Year, CommonName, Reach, count)


PIPE_fish_reach <- PIPE_fish_reach %>%
  pivot_wider(names_from = CommonName,
              values_from = count,
              values_fill = list(count = 0)
  )
empty_rows <- rowSums(PIPE_fish_reach[, 4:ncol(PIPE_fish_reach)]) == 0
empty_rows
PIPE_fish_reach_cleaned <- PIPE_fish_reach[!empty_rows, ]
PIPE_fish_reach <- Reach[!empty_rows]
PIPE_fish_reach_cleaned <- PIPE_fish_reach_cleaned %>%
  select(-`No Fish`)

                   ############################################
                   #  ANALYSIS OF SIMILARITY (ANOSIM) PIPE    #
                   ############################################
PIPE_reach_dist <- vegdist(PIPE_fish_reach_cleaned[, 4:ncol(PIPE_fish_reach_cleaned)])

attach(PIPE_fish_reach_cleaned)

PIPE_reach_anosim <- anosim(PIPE_reach_dist, Reach)

summary (PIPE_reach_anosim)



######################## FILTER TAPR DATA ########################################
TAPR_fish_reach <- fish_reach %>% 
  filter(ParkCode == "TAPR") %>%
  group_by(ParkCode, Year, Reach, CommonName) %>%
  summarise(count = sum(NumObs), .groups = 'drop') %>%
  select(ParkCode, Year, CommonName, Reach, count)


TAPR_fish_reach <- TAPR_fish_reach %>%
  pivot_wider(names_from = CommonName,
              values_from = count,
              values_fill = list(count = 0)
  )

# Check for empty rows (rows with all species values as 0)
empty_rows <- rowSums(TAPR_fish_reach[, 4:ncol(TAPR_fish_reach)]) == 0
empty_rows
TAPR_fish_reach_cleaned <- TAPR_fish_reach[!empty_rows, ]
TAPR_fish_reach <- Reach[!empty_rows]
                  ############################################
                  #     ANALYSIS OF SIMILARITY (ANOSIM) TAPR #
                  ############################################
TAPR_reach_dist <- vegdist(TAPR_fish_reach_cleaned[, 4:ncol(TAPR_fish_reach_cleaned)])

attach(TAPR_fish_reach_cleaned)

TAPR_reach_anosim <- anosim(TAPR_reach_dist, Reach)

summary (TAPR_reach_anosim)
