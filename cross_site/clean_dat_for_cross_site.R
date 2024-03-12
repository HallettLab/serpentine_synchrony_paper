## Clean data for cross site analyses
library(tidyverse)
theme_set(theme_classic())

# Read in Data ####
## JR
#JR <- read.csv("cross_site/data_CW/JR_cover2023.csv")
JRc <- read.csv("cross_site/data_CW/JR_cover2023control.csv")

## KC
KC <- read.csv("cross_site/data_CW/KC_VegData_1991_2023_MK_Feb1.csv")

# Clean Data ####
## JR
## need to ID missing quadrats
## shouldn't be any NAs
## filter to control treatment

## remove first 2 useless cols
#JR <- JR[,-c(1:2)]

ggplot(JRc, aes(x=quadID)) +
  geom_bar() +
  facet_wrap(~year)
## 2021 and 2022 are missing several quad IDs (in version of data Jake sent thru slack)
## no missing quads in control only data from Jasmin

#quads <- unique(JR$quadID)
#quads2022 <- unique(JR[JR$year == 2022,]$quadID) ## missing c and g treatments
#quads2021 <- unique(JR[JR$year == 2021,]$quadID) ## has quads from all treatments

#unique(JR[JR$year == 2021,]$treatment)
#unique(JR[JR$year == 2022,]$treatment)

JR.nas <- JRc %>%
  filter(is.na(cover))
## good, no missing cover data

unique(JRc$species)
not.sp <- c("BARE", "ROCK", "GOPHER", "THATCH")

JR.clean <- JRc %>%
  select(-X) %>%
  filter(!species %in% not.sp)
  
## KC

KC.clean <- KC %>% 
  select(-GOPHER, -NON.ANN.GRASS, -BARE, -LITTER, -ROCK, -MOSS, -COWPIE) %>%
  pivot_longer(cols = c(8:191), names_to = "species", values_to = "cover") %>% ## pivot to long format
  filter(!X %in% c("Original Order")) %>%
  mutate(year = X.1, ## change col names
           Transect1 = X.2,
           Quadrat.old = X.3,
           Quadrat.new = X.4,
           Transect2 = X.5,
           Quadrat2 = X.6) %>%
  select(-(X:X.10)) %>% ## get rid of empty cols
  filter(Transect1 %in% c("KCFlat")) %>% #, "KCN12" ## select only flat transect
  mutate(cover = ifelse(cover == "", "0", cover)) %>% ## fill in missing values with 0's as this should be the same thing
  filter(!species %in% c("UNK.ANN.GRASS", "UNK.BUN","UNKNOW")) ## get rid of unknown species

KC.clean$cover <- as.numeric(KC.clean$cover) ## change cover to numeric after filling 0's

unique(KC.clean$species)
## could make lists of the various species

## need to end up with JR & KC timeseries in one df
## would be useful to ID all the overlapping species

# Clean Env ####
rm(JR.nas, JRc, KC, not.sp)
