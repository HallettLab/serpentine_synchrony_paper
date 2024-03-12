library(tidyverse)

JR <- read.csv("cross_site/data_CW/JR_cover2023.csv")

PLER <- JR[,-c(1:2)] %>%
  filter(species == "PLER", treatment == "c") %>%
  group_by(year) %>%
  summarise(mean.cover = mean(cover))

