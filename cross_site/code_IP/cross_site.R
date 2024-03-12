## cross site
library(wsyn)
library(tidyverse)
## get data into correct format
## missing data in 1999 for KC and 2020 for JR

JR <- read.csv("data_CW/JR_cover2023.csv")

JR.jas <- read.csv("data/JRcover-ppt-2023.csv")

KC <- read.csv("data_CW/KC_VegData_1991_2023_MK_Feb1.csv") %>%
  mutate(orig.order = X, 
         year = X.1, 
         Transect1 = X.2,
         Quadrat.old = X.3,
         Quadrat.new = X.4,
         Transect2 = X.5,
         Quadrat2 = X.6) %>%
  select(-(X:X.6)) %>%
  filter(!BARE %in% c("Bare "))

BRHO <- KC %>%
  select(year, Quadrat.new, Transect2, BRO.HOR) %>%
  filter(Transect2 %in% c("KCFlat", "KCN12")) %>%
  pivot_wider(names_from = "year", values_from = BRO.HOR)






## are there years or times when certain plots are not sampled?

## what to do with missing data? input 0? functions don't run with NAs

## N x T matrix
    ## rows = sampling locations (N)
    ## columns = evenly spaced times during which sampling was conducted

## JR flat
## Nward 12 degree slope might be most similar
## KC flat; KC N12

## Phasors vs. mean fields

## drivers - 

species <- unique(JR$species)

for (i in 1:species)

# Format #### 
BRMO.wide <- JR %>%
  filter(species == "BRMO") %>%
  select(-1, -2) %>%
  pivot_wider(names_from = "year", values_from = cover) %>%
  mutate(`2020` = 0) %>%
  select(6:42)

BRMO.matrix <- as.matrix(BRMO.wide)
times <- as.numeric(colnames(BRMO.wide))

BRMO.clean <- cleandat(BRMO.matrix, times, clev=1)

dat <- as.matrix(BRMO.clean$cdat)

test <- wt(dat, times)

t1<-wpmf(dat, times)

plotmag(t1) ## this worked



## why are there NAs in BRMO 2021 & 2022?

## eventually put in a for-loop for the desired sp and then automatically save the sp?

VUMI.wide <- JR %>%
  filter(species == "VUMI") %>%
  select(-1, -2) %>%
  pivot_wider(names_from = "year", values_from = cover) %>%
  mutate(`2020` = 0) %>%
  select(6:42,46,43:45)

BRMO.matrix <- as.matrix(BRMO.wide)
times <- as.numeric(colnames(BRMO.wide))

cleandat(BRMO.matrix, times, 2)








## BRHO, PLER, LOMU, Vulpia, Lasthenia, Calycadenia, Microseris





