###### Jasper Ridge data
library(data.table)
library(tidyverse)
library(wsyn)


# Read in Data ####
monthly_rain<-read.csv("cross_site/Monthly_precip.csv") ##### a csv I made with rainfall data for JR and KC
kc_long<-read.csv("cross_site/KC_split.csv")#### a modified version of the KC data in long format that also split the slope and aspect data out from each transect

######## Coherence with growing season rain
### filter by site and growing season
jr_rain<-monthly_rain%>%separate("Date",into = c("Year","Month"),sep="-")%>%
  filter(Site=="JR")%>%
  filter(Month%in%c("09","10","11","12","01","02","03","04"))

jr_rain$Year<-as.numeric(jr_rain$Year)
jr_rain$season <- with(jr_rain, ifelse(Month%in%c("09","10","11","12"), Year+1, Year))

jr_growing_mean<-jr_rain%>%group_by(season)%>%
  summarise(mean_rain=mean(Precipitation))%>%
  ungroup()%>%
  pivot_wider(names_from = "season",values_from = "mean_rain")

jr_growing_mean<-jr_growing_mean[,-43]### drops 2024
jr_growing_mean<-jr_growing_mean[,-1]### drops 1982, which is when I thought the dataset started but it's actually 1983
jr_growing_mean<-as.matrix(jr_growing_mean)

#### now do KC
kc_rain<-monthly_rain%>%separate("Date",into = c("Year","Month"),sep="-")%>%
  filter(Site=="KC")%>%
  filter(Month%in%c("09","10","11","12","01","02","03","04"))

kc_rain$Year<-as.numeric(kc_rain$Year)
kc_rain$season <- with(kc_rain, ifelse(Month%in%c("09","10","11","12"), Year+1, Year))

kc_growing_mean<-kc_rain%>%group_by(season)%>%
  summarise(mean_rain=mean(Precipitation))%>%
  pivot_wider(names_from = "season",values_from = "mean_rain")

kc_growing_mean<-kc_growing_mean[,-34] ### drops 2024
kc_growing_mean<-as.matrix(kc_growing_mean)

######## Do coherence with growing season rain for  key species 
##### Comparing JR to KC Flat
wave_dat<-kc_long%>%
  filter(species %in% c("Plantago.erecta","Bromus.hordeaceus","Calycadenia.multiglandulosa","Festuca.microstachys","Festuca.perennis","Lasthenia.californica","Layia.platyglossa","Microseris.douglasii"))%>%
  group_by(Year,slope,aspect,species)%>%
  summarise(mean_cover=mean(cover))%>%
setnafill(fill=0,cols=c("mean_cover"))

kc_BRHO<-wave_dat%>%
  filter(species%in%c("Bromus.hordeaceus"))%>%
  filter(aspect=="Flat")%>%
  pivot_wider(names_from ="Year" ,values_from ="mean_cover")%>%
  add_column("1999"=0,.after="1998")#### there wasn't any sampling in 1999 so I added 0 as the cover for that year

kc_PLER<-wave_dat%>%filter(species%in%c("Plantago.erecta"))%>%
  filter(aspect=="Flat")%>%
  pivot_wider(names_from ="Year" ,values_from ="mean_cover")%>%
  add_column("1999"=0,.after="1998")

kc_FEMI<-wave_dat%>%filter(species%in%c("Festuca.microstachys"))%>%
  filter(aspect=="Flat")%>%
  pivot_wider(names_from ="Year" ,values_from ="mean_cover")%>%
  add_column("1999"=0,.after="1998")

kc_FEPE<-wave_dat%>%filter(species%in%c("Festuca.perennis"))%>% #### I think FEPE is actually LOMU at KC
  filter(aspect=="Flat")%>%
  pivot_wider(names_from ="Year" ,values_from ="mean_cover")%>%
  add_column("1999"=0,.after="1998")

kc_LACA<-wave_dat%>%filter(species%in%c("Lasthenia.californica"))%>%
  filter(aspect=="Flat")%>%
  pivot_wider(names_from ="Year" ,values_from ="mean_cover")%>%
  add_column("1999"=0,.after="1998")

kc_LAPL<-wave_dat%>%filter(species%in%c("Layia.platyglossa"))%>%
  filter(aspect=="Flat")%>%
  pivot_wider(names_from ="Year" ,values_from ="mean_cover")%>%
  add_column("1999"=0,.after="1998")

kc_MIDO<-wave_dat%>%filter(species%in%c("Microseris.douglasii"))%>%
  filter(aspect=="Flat")%>%
  pivot_wider(names_from ="Year" ,values_from ="mean_cover")%>%
  add_column("1999"=0,.after="1998")

##### Coherence testing for KC BRHO

times<-(1991:2023) #### set as the length of your timeseries

kc_BRHO<-kc_BRHO[-c(1:3)]
kc_BRHO<-as.matrix(kc_BRHO)##### data needs to be a matrix for wavelets

### these lines are the actual wavelet analyses
x<-cleandat(kc_growing_mean,times,1)$cdat ### cleaning the rainfall data , 1 means the time series are individually demeaned 
y<-cleandat(kc_BRHO,times,1)$cdat### same for cover data
res<-coh(dat1=x,dat2=y,times=times,norm="powall",### this is the part actually doing the coherence test
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)### plots the coherence figure, the black lines are the 95 and 99 percent confidence intervals while the red line is your data
            ### if the red line is above either black lines that usually indicates statistically significant coherence over that timescale
### KC PLER
kc_PLER<-kc_PLER[-c(1:3)]
kc_PLER<-as.matrix(kc_PLER)

x<-cleandat(kc_growing_mean,times,1)$cdat
y<-cleandat(kc_PLER,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)

### KC CAMU
####### No cover over flat transects at KC

##### KC FEMI
kc_FEMI<-kc_FEMI[,-c(1:3)]
kc_FEMI<-as.matrix(kc_FEMI)

x<-cleandat(kc_growing_mean,times,1)$cdat
y<-cleandat(kc_FEMI,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)

##### KC FEPE
kc_FEPE<-kc_FEPE[,-c(1:3)]
kc_FEPE<-as.matrix(kc_FEPE)

x<-cleandat(kc_growing_mean,times,1)$cdat
y<-cleandat(kc_FEPE,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)

##### KC LACA
kc_LACA<-kc_LACA[,-c(1:3)]
kc_LACA<-as.matrix(kc_LACA)

x<-cleandat(kc_growing_mean,times,1)$cdat
y<-cleandat(kc_LACA,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)

##### KC LAPL
kc_LAPL<-kc_LAPL[,-c(1:3)]
kc_LAPL<-as.matrix(kc_LAPL)

x<-cleandat(kc_growing_mean,times,1)$cdat
y<-cleandat(kc_LAPL,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)

##### KC MIDO
kc_MIDO<-kc_MIDO[,-c(1:3)]
kc_MIDO<-as.matrix(kc_MIDO)

x<-cleandat(kc_growing_mean,times,1)$cdat
y<-cleandat(kc_MIDO,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)
res<-bandtest(res,c(6,8)) ##### bandtest allows you to define a timescale in the time series and test for significant coherence
plotmag(res)

######### JR Coherence of selected species, MIDO, CAMU, LACA, LAPL, VUMI, LOMU
jr<-read.csv("JR_cover2023.csv")
times<-(1983:2023)
###BRHO and rainfall
brho_jr<-jr%>%filter(species=="BRMO")%>%
  group_by(year)%>%
  summarise(mean_cover=mean(cover))%>%
  ungroup()%>%
  pivot_wider(names_from ="year" ,values_from ="mean_cover")%>%
  add_column("2020"=0,.after="2019")
brho_jr<-as.matrix(brho_jr)

x<-cleandat(jr_growing_mean,times,1)$cdat
y<-cleandat(brho_jr,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)
res<-bandtest(res,c(19,21))

##### PLER and rain
pler_jr<-jr%>%filter(species=="PLER")%>%
  group_by(year)%>%
  summarise(mean_cover=mean(cover))%>%
  ungroup()%>%
  pivot_wider(names_from ="year" ,values_from ="mean_cover")%>%
  add_column("2020"=0,.after="2019")
pler_jr<-as.matrix(pler_jr)

x<-cleandat(jr_growing_mean,times,1)$cdat
y<-cleandat(jrpl_w,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)
res<-bandtest(res,c(3,4)) 
get_bandp(res)

### MIDO
mido_jr<-jr%>%filter(species=="MIDO")%>%
  group_by(year)%>%
  summarise(mean_cover=mean(cover))%>%
  ungroup()%>%
  pivot_wider(names_from ="year" ,values_from ="mean_cover")%>%
  add_column("2020"=0,.after="2019")

mido_jr<-as.matrix(mido_jr)
### MIDO and rainfall
x<-cleandat(jr_growing_mean,times,1)$cdat
y<-cleandat(mido_jr,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)
res<-bandtest(res,c(11,15)) 
get_bandp(res)

######### CAMU
camu_jr<-jr%>%filter(species=="CAMU")%>%
  group_by(year)%>%
  summarise(mean_cover=mean(cover))%>%
  ungroup()%>%
  pivot_wider(names_from ="year" ,values_from ="mean_cover")%>%
  add_column("2020"=0,.after="2019")

camu_jr<-as.matrix(camu_jr)
### CAMU and growing season rainfall
x<-cleandat(jr_growing_mean,times,1)$cdat
y<-cleandat(camu_jr,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)
res<-bandtest(res,c(11,20)) 
get_bandp(res)

##### LACA
laca_jr<-jr%>%filter(species=="LACA")%>%
  group_by(year)%>%
  summarise(mean_cover=mean(cover))%>%
  ungroup()%>%
  pivot_wider(names_from ="year" ,values_from ="mean_cover")%>%
  add_column("2020"=0,.after="2019")

laca_jr<-as.matrix(laca_jr)
### LACA and growing season rainfall
x<-cleandat(jr_growing_mean,times,1)$cdat
y<-cleandat(laca_jr,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)

####### LAPL
lapl_jr<-jr%>%filter(species=="LAPL")%>%
  group_by(year)%>%
  summarise(mean_cover=mean(cover))%>%
  ungroup()%>%
  pivot_wider(names_from ="year" ,values_from ="mean_cover")%>%
  add_column("2020"=0,.after="2019")

lapl_jr<-as.matrix(lapl_jr)
### LAPL and growing season rainfall
x<-cleandat(jr_growing_mean,times,1)$cdat
y<-cleandat(lapl_jr,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)
res<-bandtest(res,c(5,7)) 
get_bandp(res)

########LOMU
lomu_jr<-jr%>%filter(species=="MIDO")%>%
  group_by(year)%>%
  summarise(mean_cover=mean(cover))%>%
  ungroup()%>%
  pivot_wider(names_from ="year" ,values_from ="mean_cover")%>%
  add_column("2020"=0,.after="2019")

lomu_jr<-as.matrix(lomu_jr)

### LOMU and growing season rainfall
x<-cleandat(jr_growing_mean,times,1)$cdat
y<-cleandat(lomu_jr,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)

#### VUMI
vumi_jr<-jr%>%filter(species=="MIDO")%>%
  group_by(year)%>%
  summarise(mean_cover=mean(cover))%>%
  ungroup()%>%
  pivot_wider(names_from ="year" ,values_from ="mean_cover")%>%
  add_column("2020"=0,.after="2019")

vumi_jr<-as.matrix(vumi_jr)

### LAPL and growing season rainfall
x<-cleandat(jr_growing_mean,times,1)$cdat
y<-cleandat(vumi_jr,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)

######## Coherence between cover of PLER and BRHO 
####### Jasper ridge vs KC
#### Edit jr data to be on same timescale as KC data
jrbr_w2<-jrbr_w[,-c(1:8)]
jrpl_w2<-jrpl_w[,-c(1:8)]

###BRHO
times<-(1991:2023)
x<-cleandat(jrbr_w2,times,1)$cdat
y<-cleandat(kc_BRHO,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",# compute coherence between x and y
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)###### sig coherence

### PLER
x<-cleandat(jrpl_w2,times,1)$cdat
y<-cleandat(kc_PLER,times,1)$cdat
res<-coh(dat1=x,dat2=y,times=times,norm="powall",# compute coherence between x and y
         sigmethod="fftsurrog1",nrand=1000,
         f0=0.5)
plotmag(res)###### no coherence