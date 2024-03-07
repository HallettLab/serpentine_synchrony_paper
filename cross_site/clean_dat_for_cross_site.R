## Clean data for cross site analyses

library(tidyverse)
theme_set(theme_classic())

# Read in Data ####
## JR
JR <- read.csv("cross_site/data_CW/JR_cover2023.csv")

## KC
KC <- read.csv("cross_site/data_CW/KC_VegData_1991_2023_MK_Feb1.csv")

# Clean up ####
## JR
## need to ID missing quadrats
## shouldn't be any NAs
## filter to control treatment

## remove first 2 useless cols
#JR <- JR[,-c(1:2)]

ggplot(JR, aes(x=quadID)) +
  geom_bar() +
  facet_wrap(~year)
## 2021 and 2022 are missing several quad IDs

quads <- unique(JR$quadID)
quads2022 <- unique(JR[JR$year == 2022,]$quadID) ## missing c and g treatments
quads2021 <- unique(JR[JR$year == 2021,]$quadID) ## has quads from all treatments

unique(JR[JR$year == 2021,]$treatment)
unique(JR[JR$year == 2022,]$treatment)

JR.nas <- JR %>%
  filter(is.na(cover))
## good, no missing cover data

unique(JR$species)
not.sp <- c("BARE", "ROCK", "GOPHER", "THATCH")

JR.clean <- JR %>%
  select(-X, -`...5`) %>%
  filter(!species %in% not.sp)
  
## KC
## fill in missing values with 0's as this should be the same thing
KC.clean <- KC %>% 
  #mutate(orig.order = X, 
       #  year = X.1, 
       #  Transect1 = X.2,
       #  Quadrat.old = X.3,
       #  Quadrat.new = X.4,
       #  Transect2 = X.5,
       #  Quadrat2 = X.6) %>%
  #select(-(X:X.6)) %>%
 # filter(!BARE %in% c("Bare ")) %>%
  select(-GOPHER, -NON.ANN.GRASS, -BARE, -LITTER, -ROCK, -MOSS, -COWPIE) %>%
  pivot_longer(cols = c(8:191), names_to = "species", values_to = "cover") %>%
  filter(!X %in% c("Original Order")) %>%
  mutate(year = X.1, 
           Transect1 = X.2,
           Quadrat.old = X.3,
           Quadrat.new = X.4,
           Transect2 = X.5,
           Quadrat2 = X.6) %>%
  select(-(X:X.10)) %>%
  filter(Transect1 %in% c("KCFlat", "KCN12"))

unique(KC.clean$species)
## could make lists of the various species

## need to end up with JR & KC timeseries in one df
## would be useful to ID all the overlapping species