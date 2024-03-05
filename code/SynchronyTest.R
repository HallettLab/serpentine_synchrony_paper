# Synchrony project: team Jeli
# Authors: Jeremy Colins and Lisa Buche
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---- 1. Setup ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import packages
library(forecast) # for ARIMA function
library(lmtest) # for ARIMA eigen value
library(ppcor) # for variable correlation
library(tsvr) # for synchrony 
library(tidyverse) # for synchrony 
library(ggplot2) 
library(ggthemes)
library(ggpattern)
library(wesanderson)
library(colorspace)
library(broom)

# Import data
#JR
JR_cover <- read_csv("~/Documents/Projects/Synchrony/data/JRcover-ppt-2023.csv")
JR_cover <-JR_cover[,-1] # remove first column with no information
head(JR_cover)
names(JR_cover)
specieslist.JR <- names(JR_cover)
specieslist.JR <- specieslist.JR [!specieslist.JR %in% c("ID","year","plot", "fppt",
                                          "gppt", "tcover")]
# interesting species accoridng to Jake
specieslist.JR.short <-c("BRMO","PLER","CAMU","MIDO",
                         "LOMU","LACA","VUMI","LAPL")
# Filter and aggregate by year
JR_cover <- JR_cover %>%
  gather(specieslist.JR, key="species",value="abundance") %>%
  dplyr::filter(species %in% specieslist.JR.short) %>%
  aggregate(abundance ~ species +  year, sum) %>% # this a simplification but maybe later worth investigating
  spread(species, abundance)

# Quick visualisation

JR_cover_plot <- JR_cover %>%
  gather(specieslist.JR.short, key="species",value="abundance")%>%
  ggplot(aes(y=abundance, x=year)) +
  geom_path(aes(color=species)) +
  theme_bw() +
  scale_color_colorblind() +
  labs(title="JR cover of main species over time")

ggsave("~/Documents/Projects/Synchrony/JR_cover_plot.pdf",
       plot = JR_cover_plot)

# to play with the species display
JR_cover_plot <- plotly::ggplotly(JR_cover_plot) 
JR_cover_plot

#KC
KC_cover <- read_excel("~/Documents/Projects/Synchrony/data/KC_VegData_1991_2023_MK_Feb1.xlsx")
head(KC_cover)
names(KC_cover) <- tolower(names(KC_cover)) # remove capital letters
specieslist.KC <- names(KC_cover)
specieslist.KC <- specieslist.KC[!specieslist.KC %in% c("year",
                                                        "original order","transect1",
                                                        "quadrat_old", "quadrat_new",
                                                        "transect2", "quadrat2",
                                                        "gopher +/-","litter","rock",
 
                                                                                                               "bare")]
# interesting species accoridng to Jake
specieslist.KC.short <- c("bromus hordeaceus","plantago erecta",
                    "microseris douglasii","lasthenia californica",
                    "festuca perennis","festuca microstachys","layia platyglossa"  )

# Filter and aggregate by year
KC_cover <- KC_cover %>%
  gather(specieslist.KC, key="species",value="abundance") %>%
  mutate(abundance = as.numeric(abundance)) %>%
  dplyr::filter(species %in% specieslist.KC.short) %>%
  aggregate(abundance ~ species + year, sum) %>%# this a simplification but maybe later worth investigating
  mutate(species = case_when(species =="bromus hordeaceus" ~ "BRHO",
                             species =="plantago erecta"~ "PLER",
                             species =="microseris douglasii" ~ "MIDO",
                             species =="lasthenia californica"~ "LACA",
                             species =="festuca perennis"~ "FEPE",
                             species =="festuca microstachys"~ "FEMI",
                             species =="layia platyglossa" ~ "LAPL" )) %>%
  spread(species, abundance) 

# I know there is a way to replace the name by the code faster but I am not very good with syntax stuff
specieslist.KC.short <- c("BRHO","PLER","MIDO","LACA","FEPE","FEMI","LAPL")

# Quick visualisation

KC_cover_plot <- KC_cover %>%
  gather(specieslist.KC.short, key="species",value="abundance")%>%
  ggplot(aes(y=abundance, x=year)) +
  geom_path(aes(color=species)) +
  theme_bw() +
  scale_color_colorblind() +
  labs(title="KC cover of main species over time")

ggsave("~/Documents/Projects/Synchrony/KC_cover_plot.pdf",
       plot = KC_cover_plot)
#to play with the species abundance in r
KC_cover_plot <- plotly::ggplotly(KC_cover_plot)
KC_cover_plot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---- 2. Synchrony test ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create all combinaison of pair possible to test for synchrony between them
specieslist.JR.comb <- as.data.frame(expand.grid(specieslist.JR.short,specieslist.JR.short)) %>%
  mutate(Var1 = as.character(Var1),
         Var2 = as.character(Var2))

specieslist.KC.comb <- as.data.frame(expand.grid(specieslist.KC.short,specieslist.KC.short)) %>%
  mutate(Var1 = as.character(Var1),
         Var2 = as.character(Var2))

# Find synchrony long and short for all species pair in JR

df.synchrony.JR <- NULL
for ( i in 1:nrow(specieslist.JR.comb)){
  df.synchrony.i <- t(as.matrix(data.frame(Ni= JR_cover[,specieslist.JR.comb$Var1[i]],
                                          Nj =JR_cover[,specieslist.JR.comb$Var2[i]]))) 
  # you can have more than two species at a time, but it is a start I guess
  

  tryCatch( { vr.trial <- tsvreq_classic(df.synchrony.i); print(res) }
            , error = function(e) {an.error.occured <<- TRUE})
  if(exists("vr.trial")){
    aggresShort <- aggts(vr.trial, vr.trial$ts[vr.trial$ts<4])[[3]]
    aggresLong <- aggts(vr.trial, vr.trial$ts[vr.trial$ts>=4])[[3]]
    if(aggresShort>1|aggresLong> 1){
      synchrony.significance <- "Long and short synchrony"
      if(aggresShort>1 & aggresLong < 1){
        synchrony.significance <-"Short synchrony"
      }
      if(aggresShort<1 & aggresLong > 1){
        synchrony.significance <-"Long synchrony"
      }
    }else{synchrony.significance <- "No"}
  }else{
    aggresLong <- NA
    aggresShort <- NA
    synchrony.significance <- NA
  }


df.synchrony.i <- data.frame( couple = paste(specieslist.JR.comb$Var1[i],
                                             specieslist.JR.comb$Var2[i],sep=""),
                              aggresShort= aggresShort,
                              aggresLong= aggresLong,
                              synchrony.significance=synchrony.significance)
df.synchrony.JR  <- bind_rows(df.synchrony.JR,df.synchrony.i)
}

write_csv(df.synchrony.JR,
          "~/Documents/Projects/Synchrony/results/df.synchrony.JR")
# Find synchrony long and short for all species pair in KC

df.synchrony.KC <- NULL
for ( i in 1:nrow(specieslist.KC.comb)){
  df.synchrony.i <- t(as.matrix(data.frame(Ni= KC_cover[,specieslist.KC.comb$Var1[i]],
                                           Nj =KC_cover[,specieslist.KC.comb$Var2[i]]))) 
  # you can have more than two species at a time, but it is a start I guess
  
  
  tryCatch( { vr.trial <- tsvreq_classic(df.synchrony.i); print(res) }
            , error = function(e) {an.error.occured <<- TRUE})
  if(exists("vr.trial")){
    aggresShort <- aggts(vr.trial, vr.trial$ts[vr.trial$ts<4])[[3]]
    aggresLong <- aggts(vr.trial, vr.trial$ts[vr.trial$ts>=4])[[3]]
    if(aggresShort>1|aggresLong> 1){
      synchrony.significance <- "Long and short synchrony"
      if(aggresShort>1 & aggresLong < 1){
        synchrony.significance <-"Short synchrony"
      }
      if(aggresShort<1 & aggresLong > 1){
        synchrony.significance <-"Long synchrony"
      }
    }else{synchrony.significance <- "No"}
  }else{
    aggresLong <- NA
    aggresShort <- NA
    synchrony.significance <- NA
  }
df.synchrony.i <- data.frame( couple = paste(specieslist.KC.comb$Var1[i],
                                             specieslist.KC.comb$Var2[i],sep=""),
                              aggresShort= aggresShort,
                              aggresLong= aggresLong,
                              synchrony.significance=synchrony.significance)
df.synchrony.KC  <- bind_rows(df.synchrony.KC,df.synchrony.i)
}
write_csv(df.synchrony.KC,
          "~/Documents/Projects/Synchrony/results/df.synchrony.KC")

