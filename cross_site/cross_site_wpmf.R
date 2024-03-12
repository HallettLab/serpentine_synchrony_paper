# Purpose: 
## Calc wpmf across sites for same species

# Set up Env ####
library(wsyn)
library(tidyverse)
library(ggfortify)
library(ggpubr)

theme_set(theme_classic())

source("cross_site/clean_dat_for_cross_site.R")

## Notes
## get data into correct format
## missing data in 1999 for KC and 2020 for JR

## do averages of species at each site
## do wpmf between sites for each species

# Calc WPMF for main species ####
## harmonize species names
main.species <- c("BRMO", "PLER", "LOMU", "LACA", "LAPL", "MIDO", "VUMI")

JR.mainsp <- JR.clean %>%
  filter(species %in% main.species) %>%
  mutate(species = ifelse(species == "BRMO", "BRHO", species))
  
main.species.KC <- c("BRO.HOR", "PLA.ERE", "FES.PER", "LAY.PLA", "LAS.CAL", "MIC.DOU", "FES.MIC")

KC.mainsp <- KC.clean %>%
  filter(species %in% main.species.KC) %>%
  mutate(species2 = paste0(substr(species, start = 1, stop = 2), substr(species, start = 5, stop = 6)),
         species3 = ifelse(species2 == "FEPE", "LOMU",
                           ifelse(species2 == "FEMI", "VUMI", species2))) %>%
  select(-species, -species2) %>%
  mutate(species = species3) %>%
  select(-species3)

species <- unique(KC.mainsp$species)

## try in a loop
for(i in 1:length(species)) {
  
  ## select species
  sp <- species[i]
  
  ## JR
  tmp.avgJR <- JR.mainsp %>%
    filter(species == sp) %>%
    group_by(year) %>%
    summarise(mean.cov = mean(cover))
  
  ## create filler data for 2020
  tmpfillJR <- data.frame(year = 2020, mean.cov = median(tmp.avgJR$mean.cov))
  
  ## put together 
  tmp.avgJR2 <- rbind(tmp.avgJR, tmpfillJR) %>%
    arrange(year) %>%
    mutate(site = "JR") %>%
    filter(year > 1990) %>%
    pivot_wider(names_from = "year", values_from = "mean.cov")
  
  ## KC
  tmp.avgKC <- KC.mainsp %>%
    filter(species == sp) %>%
    group_by(year) %>%
    summarise(mean.cov = mean(cover))
  
  ## create filler data for 1999
  tmpfillKC <- data.frame(year = 1999, mean.cov = median(tmp.avgKC$mean.cov))
  
  ## put together 
  tmp.avgKC2 <- rbind(tmp.avgKC, tmpfillKC) %>%
    arrange(year) %>%
    mutate(site = "KC") %>%
    pivot_wider(names_from = "year", values_from = "mean.cov")
  
  ## join sites
  both <- rbind(tmp.avgJR2, tmp.avgKC2)
  
  ## vis
  both.long <- both %>%
    pivot_longer(cols = 2:34, names_to = "year", values_to = "cover")
  
  both.long$year <- as.numeric(both.long$year)
  
  ggplot(both.long, aes(x=year, y=cover)) +
    geom_line() +
    facet_wrap(~site, ncol = 1, nrow = 2) + 
    ggtitle(sp) + ylab("mean site cover")
    
  ggsave(paste0("cross_site/prelim_figs/", sp, "_cross_site_timeseries.png"), width = 6, height = 3.5)
  
  ## put in matrix format
  both.matrix <- as.matrix(both[,-1])
  
  ## save a vector of years
  times <- as.numeric(colnames(both[,-1]))
  
  ## clean using wsyn cleaning function
  both.clean <- as.matrix(cleandat(both.matrix, times, clev=1)$cdat)
  
  ## do wavelet phasor mean field analysis
  temp_wpmf <- wpmf(both.clean, times, sigmethod = "fft")
  
  ## wavelet mean field
  temp_wmf <- wmf(both.clean, times)
  
  ## power
  pow <- power(temp_wmf)

  ## save figures
  png(paste0("cross_site/prelim_figs/", sp, "_cross_site_wpmf.png"), width = 480, height =480)
  plotmag(temp_wpmf)
  dev.off()
  
  png(paste0("cross_site/prelim_figs/", sp, "_cross_site_wmf.png"), width = 480, height =480)
  plotmag(temp_wmf)
  dev.off()
  
  png(paste0("cross_site/prelim_figs/", sp, "_cross_site_wmf_power.png"), width = 600, height =400)
  plot(log(1/pow$timescales),pow$power,type='l',lty="solid",xaxt="n",
       xlab="Timescales",ylab="Power")
  xlocs<-c(min(pow$timescales),pretty(pow$timescales,n=8))
  graphics::axis(side=1,at=log(1/xlocs),labels=xlocs)
  dev.off()
  
  
}
