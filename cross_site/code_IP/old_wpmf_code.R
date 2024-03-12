

BRMO.avg <- JR.clean %>%
  filter(species == "BRMO") %>%
  group_by(year) %>%
  summarise(mean.cov = mean(cover))

## create filler data for 2020
tmpJRBRMO <- data.frame(year = 2020, mean.cov = median(BRMO.avg$mean.cov))

## put together 
BRMO.avg2 <- rbind(BRMO.avg, tmpJRBRMO) %>%
  arrange(year) %>%
  mutate(site = "JR") %>%
  filter(year > 1990) %>%
  pivot_wider(names_from = "year", values_from = "mean.cov")

### KC
BRHO.avg <- KC.clean %>%
  filter(species == "BRO.HOR") %>%
  group_by(year) %>%
  summarise(mean.cov = mean(cover))

## create filler data for 1999
tmpKCBRHO <- data.frame(year = 1999, mean.cov = median(BRHO.avg$mean.cov))

## put together 
BRHO.avg2 <- rbind(BRHO.avg, tmpKCBRHO) %>%
  arrange(year) %>%
  mutate(site = "KC") %>%
  pivot_wider(names_from = "year", values_from = "mean.cov")

## join sites
BRHO.both <- rbind(BRMO.avg2, BRHO.avg2)

BRHO.matrix <- as.matrix(BRHO.both[,-1])
times <- as.numeric(colnames(BRHO.both[,-1]))

BRHO.clean <- as.matrix(cleandat(BRHO.matrix, times, clev=1)$cdat)

brho <- wpmf(BRHO.clean, times, sigmethod = "quick")

plotmag(brho)


unique(JR.clean$species)

unique(KC.clean$species)






BRMO.matrix <- as.matrix(BRMO.wide)
times <- as.numeric(colnames(BRMO.wide))

BRMO.clean <- as.matrix(cleandat(BRMO.matrix, times, clev=1)$cdat)

brho <- wpmf(BRMO.clean, times, sigmethod = "quick")

png("cross_site/prelim_figs/JR_brmo_1983_2019_wpmf.png", width = 480, height =480)
plotmag(brho) ## this worked
dev.off()


## VUMI ####
VUMI.wide <- JR %>%
  filter(species == "VUMI") %>%
  select(-1, -2) %>%
  pivot_wider(names_from = "year", values_from = cover) %>%
  # mutate(`2020` = 0) %>%
  select(6:42) ## take up until 2019 as it seems like we're missing data from 2021 & 2022 here

VUMI.matrix <- as.matrix(VUMI.wide)
times <- as.numeric(colnames(VUMI.wide))

VUMI.clean <- as.matrix(cleandat(VUMI.matrix, times, clev=1)$cdat)

vumi<-wpmf(VUMI.clean, times, sigmethod = "quick")

plotmag(vumi) ## this worked

## PLER ####
PLER.wide <- JR %>%
  filter(species == "PLER") %>%
  select(-1, -2) %>%
  pivot_wider(names_from = "year", values_from = cover) %>%
  # mutate(`2020` = 0) %>%
  select(6:42) ## take up until 2019 as it seems like we're missing data from 2021 & 2022 here

PLER.matrix <- as.matrix(PLER.wide)
times <- as.numeric(colnames(PLER.wide))

PLER.clean <- as.matrix(cleandat(PLER.matrix, times, clev=1)$cdat)

pler<-wpmf(PLER.clean, times, sigmethod = "quick")

png("cross_site/prelim_figs/JR_pler_1983_2019_wpmf.png", width = 480, height =480)
plotmag(pler) ## this worked
dev.off()
# KC ####
## BRHO
BRHO.wide.KC <- kc_long %>%
  select(-Gopher...., -Nonnative.Annual.Grass) %>% ## need to take these vals out otherwise end up with more than one row per unique quadrat
  filter(species == "Bromus.hordeaceus", Transect %in% c("KCFlat")) %>%
  pivot_wider(names_from = "Year", values_from = cover) %>%
  select(7:38) %>%
  setnafill(fill=0,cols=c(1:32))

BRHO.wide.KC.matrix <- as.matrix(BRHO.wide.KC)

test <- apply(BRHO.wide.KC.matrix, MARGIN=1, median)

BRHO.wide.KC$`1999` <- test ## missing 1999

BRHO.wide.KC <- BRHO.wide.KC %>%
  select(`1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`,`2016`, `2017`, `2018`, `2019`, `2020`, `2021`,`2022`, `2023`)

BRHO.matrix.KC <- as.matrix(BRHO.wide.KC)
times <- as.numeric(colnames(BRHO.wide.KC))

BRHO.clean.KC <- as.matrix(cleandat(BRHO.matrix.KC, times, clev=1)$cdat)

brho.kc<-wpmf(BRHO.clean.KC, times, sigmethod = "quick")

## save
png("cross_site/prelim_figs/KC_brmo_1991_2023_wpmf.png", width = 480, height =480)
plotmag(brho.kc)
dev.off()

## PLER
PLER.wide.KC <- kc_long %>%
  select(-Gopher...., -Nonnative.Annual.Grass) %>% ## need to take these vals out otherwise end up with more than one row per unique quadrat
  filter(species == "Plantago.erecta", Transect %in% c("KCFlat")) %>%
  pivot_wider(names_from = "Year", values_from = cover) %>%
  select(7:38) %>%
  setnafill(fill=0,cols=c(1:32))

PLER.wide.KC.matrix <- as.matrix(PLER.wide.KC)

test <- apply(PLER.wide.KC.matrix, MARGIN=1, median)

PLER.wide.KC$`1999` <- test ## missing 1999

PLER.wide.KC <- PLER.wide.KC %>%
  select(`1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`,`2016`, `2017`, `2018`, `2019`, `2020`, `2021`,`2022`, `2023`)

PLER.matrix.KC <- as.matrix(PLER.wide.KC)
times <- as.numeric(colnames(PLER.wide.KC))

PLER.clean.KC <- as.matrix(cleandat(PLER.matrix.KC, times, clev=1)$cdat)

pler.kc<-wpmf(PLER.clean.KC, times, sigmethod = "quick")

## save
png("cross_site/prelim_figs/KC_pler_1991_2023_wpmf.png", width = 480, height =480)
plotmag(pler.kc)
dev.off()


