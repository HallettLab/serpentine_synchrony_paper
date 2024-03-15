# Synchrony project: team Jeli
# Authors: Jeremy Collings and Lisa Buche
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
library(wsyn)
library(readxl)

# Import data
#JR
JR_cover <- read_csv("JR_cover2023.csv")
JR_cover <-JR_cover[,-c(1,2)] # remove first column with no information
head(JR_cover)
names(JR_cover)

# interesting species accoridng to Jake
specieslist.JR.short <-c("BRMO","PLER","CAMU","MIDO",
                         "LOMU","LACA","VUMI","LAPL", 
                         "LOSU")

JR_cover <- JR_cover %>% 
  filter(species %in% specieslist.JR.short) %>%
  aggregate(cover ~ species + year, median) %>%
  spread(species, cover)

# Quick visualisation

JR_cover_plot <- JR_cover %>%
  gather(specieslist.JR.short, key="species",value="abundance")%>%
  ggplot(aes(y=abundance, x=year)) +
  geom_path(aes(color=species)) +
  theme_bw() +
  scale_color_colorblind() +
  labs(title="JR cover of main species over time")

ggsave("figures/JR_cover_plot.pdf",
       plot = JR_cover_plot)

# to play with the species display
JR_cover_plot <- plotly::ggplotly(JR_cover_plot) 
JR_cover_plot

#KC
KC_cover <- read_excel("KC_VegData_1991_2023_MK_Feb1.xlsx")
names(KC_cover) <- as.character(KC_cover[1,])
KC_cover <- KC_cover[-1,]
head(KC_cover)
names(KC_cover) <- tolower(names(KC_cover)) # remove capital letters
specieslist.KC <- names(KC_cover)[15:198]

# interesting species accoridng to Jake
specieslist.KC.short <- c("acmispon wrangelianus", "bromus hordeaceus","plantago erecta",
                          "microseris douglasii","lasthenia californica",
                          "festuca perennis","festuca microstachys","layia platyglossa"  )

# Filter and aggregate by year
KC_cover <- KC_cover %>%
  gather(specieslist.KC, key="species",value="abundance") %>%
  mutate(abundance = as.numeric(abundance)) %>%
  dplyr::filter(species %in% specieslist.KC.short) %>%
  aggregate(abundance ~ species + year, median) %>%# this a simplification but maybe later worth investigating
  mutate(species = case_when(species == "acmispon wrangelianus" ~ "LOSU",
                             species =="bromus hordeaceus" ~ "BRHO",
                             species =="plantago erecta"~ "PLER",
                             species =="microseris douglasii" ~ "MIDO",
                             species =="lasthenia californica"~ "LACA",
                             species =="festuca perennis"~ "FEPE",
                             species =="festuca microstachys"~ "FEMI",
                             species =="layia platyglossa" ~ "LAPL" )) %>%
  spread(species, abundance) 

# I know there is a way to replace the name by the code faster but I am not very good with syntax stuff
specieslist.KC.short <- c("LOSU","BRHO","PLER","MIDO","LACA","FEPE","FEMI","LAPL")

# Quick visualisation

KC_cover_plot <- KC_cover %>%
  gather(specieslist.KC.short, key="species",value="abundance") %>%
  ggplot(aes(y=abundance, x=year, color = species, group = species)) +
  geom_path() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 300)) + 
  scale_color_colorblind() +
  labs(title="KC cover of main species over time")

# let's fill in 1999

KC_cover <- rbind.data.frame(KC_cover[1:8,], 
                             c(1999,apply(KC_cover[-1], 2, mean, na.rm = TRUE)), 
                             KC_cover[9:32,])

KC_cover_plot <- KC_cover %>%
  gather(specieslist.KC.short, key="species",value="abundance") %>%
  ggplot(aes(y=abundance, x=year, color = species, group = species)) +
  geom_path() +
  theme_bw() +
  scale_color_colorblind() +
  theme(axis.text.x = element_text(angle = 300)) + 
  labs(title="KC cover of main species over time")

ggsave("figures/KC_cover_plot.pdf",
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
                                               specieslist.JR.comb$Var2[i],sep="_"),
                                species1=specieslist.JR.comb$Var1[i],
                                species2=specieslist.JR.comb$Var2[i],
                                aggresShort= aggresShort,
                                aggresLong= aggresLong,
                                synchrony.significance=synchrony.significance)
  df.synchrony.JR  <- bind_rows(df.synchrony.JR,df.synchrony.i)
}
head(df.synchrony.JR)

write_csv(df.synchrony.JR,
          "results/df.synchrony.JR.csv")
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
                                               specieslist.KC.comb$Var2[i],sep="_"),
                                species1=specieslist.KC.comb$Var1[i],
                                species2=specieslist.KC.comb$Var2[i],
                                aggresShort= aggresShort,
                                aggresLong= aggresLong,
                                synchrony.significance=synchrony.significance)
  df.synchrony.KC  <- bind_rows(df.synchrony.KC,df.synchrony.i)
}
write_csv(df.synchrony.KC,
          "results/df.synchrony.KC.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---- 3. Visualizing Synchrony ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df.synchrony.JR <- read.csv("results/df.synchrony.JR.csv")
df.synchrony.KC <- read.csv("results/df.synchrony.KC.csv")

long.df.synchrony.JR <- pivot_longer(df.synchrony.JR, 
                                     cols = c("aggresShort", "aggresLong"), 
                                     names_to = c("timescale"), 
                                     values_to = c("varRatio"))

long.df.synchrony.KC <- pivot_longer(df.synchrony.KC, 
                                     cols = c("aggresShort", "aggresLong"), 
                                     names_to = c("timescale"), 
                                     values_to = c("varRatio"))

JR_VarRatio_hist <-ggplot(data = long.df.synchrony.JR[which(long.df.synchrony.JR$varRatio != 2), ], 
       aes(x = varRatio, fill = timescale)) +
  geom_histogram(position = "identity", alpha = .5) + 
  xlab("Variance Ratio") + 
  theme_classic(base_size = 15) + ylab("Frequency") + 
  geom_vline(xintercept = 1, linetype = "dashed", size = 1) + 
  scale_fill_manual(values = c("#FFBE0B", "#7340A0"), 
                    name = "Timescale", labels = c("Long", "Short")) 

ggsave("figures/JR_VarRatio_hist.pdf",
       plot = JR_VarRatio_hist )

JR_VarRatio <- ggplot(data = long.df.synchrony.JR[which(long.df.synchrony.JR$varRatio != 2), ], 
                      aes(x = timescale, y = varRatio)) +
  geom_violin() + geom_jitter() + 
  xlab("Timescale") + 
  theme_classic(base_size = 15) + ylab("Variance Ratio") +
  scale_x_discrete(labels = c("Long", "Short")) + 
  geom_hline(yintercept = 1, linetype = "dashed", size = 1)

ggsave("figures/JR_VarRatio.pdf",
       plot = JR_VarRatio)

table(long.df.synchrony.JR$varRatio[which(long.df.synchrony.JR != 2)] > 1, 
      long.df.synchrony.JR$timescale[which(long.df.synchrony.JR != 2)])

# generally less synchrony, but maybe marginally more at short timescales??

KC_VarRatio_hist <- ggplot(data = long.df.synchrony.KC[which(long.df.synchrony.KC$varRatio != 2), ], 
       aes(x = varRatio, fill = timescale)) +
  geom_histogram(position = "identity", alpha = .5) + 
  xlab("Variance Ratio") + 
  theme_classic(base_size = 15) + ylab("Frequency") + 
  geom_vline(xintercept = 1, linetype = "dashed", size = 1) + 
  scale_fill_manual(values = c("#FFBE0B", "#7340A0"), 
                    name = "Timescale", labels = c("Long", "Short")) 

ggsave("figures/KC_VarRatio_hist.pdf",
       plot = KC_VarRatio_hist)

KC_VarRatio <- ggplot(data = long.df.synchrony.KC[which(long.df.synchrony.KC$varRatio != 2), ], 
                      aes(x = timescale, y = varRatio)) +
  geom_violin() + geom_jitter() + 
  xlab("Timescale") + 
  theme_classic(base_size = 15) + ylab("Variance Ratio") +
  scale_x_discrete(labels = c("Long", "Short")) + 
  geom_hline(yintercept = 1, linetype = "dashed", size = 1)

ggsave("figures/KC_VarRatio.pdf",
       plot = KC_VarRatio)

table(long.df.synchrony.KC$varRatio[which(long.df.synchrony.KC != 2)] > 1, 
      long.df.synchrony.KC$timescale[which(long.df.synchrony.KC != 2)])

# generally much more synchrony, especially at shorter timescales

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---- 4. Networks ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(igraph)

##### Jasper Ridge -----

####### Short -----

# Create data
df.synchrony.JR.matrix.short <- df.synchrony.JR %>%
  filter(species1 != species2) %>%
  select(species1,species2,aggresShort) %>%
  spread(species2,aggresShort) %>%
  column_to_rownames("species1")%>%
  as.matrix()

network.JR.short.pos <- unname(abs(df.synchrony.JR.matrix.short))
network.JR.short <- graph_from_adjacency_matrix(network.JR.short.pos>0)
E(network.JR.short)$weight <- as.numeric(network.JR.short.pos[!is.na(network.JR.short.pos)])
widths <- E(network.JR.short)$weight*2
color.mat <-  as.numeric(df.synchrony.JR.matrix.short[!is.na(df.synchrony.JR.matrix.short)])
color.mat[which(color.mat > 1)] <- 1
color.mat[which(color.mat < 1)] <- 3
E(network.JR.short)$lty <- color.mat
 
pdf(file = "figures/JR.short.network.pdf", width = 6, height = 6)

plot(network.JR.short , layout=layout.circle,
     main="JR short synchrony",
     vertex.label = colnames(df.synchrony.JR.matrix.short),
     vertex.frame.color = "transparent",
     vertex.label.family="Helvetica",
     #vertex.label.cex = 1,
     #vertex.label.dist= 2,
     #vertex.label.degree = c(pi/2,pi/2,-pi/2),
     vertex.label.color = "black",
     vertex.color = "grey80",
     vertex.size = 30,
     edge.width = widths,
     edge.color = "black",
     edge.arrow.size = 0.5,
     edge.curved = TRUE)

dev.off()
####### Long -----

# Create data
df.synchrony.JR.matrix.long <- df.synchrony.JR %>%
  filter(species1 != species2) %>%
  select(species1,species2,aggresLong) %>%
  spread(species2,aggresLong) %>%
  column_to_rownames("species1")%>%
  as.matrix()

network.JR.long.pos <- unname(abs(df.synchrony.JR.matrix.long))
network.JR.long <- graph_from_adjacency_matrix(network.JR.long.pos>0)
E(network.JR.long)$weight <- as.numeric(network.JR.long.pos[!is.na(network.JR.long.pos)])
widths <- E(network.JR.long)$weight*2
color.mat <-  as.numeric(df.synchrony.JR.matrix.long[!is.na(df.synchrony.JR.matrix.long)])
color.mat[which(color.mat > 1)] <- 1
color.mat[which(color.mat < 1)] <- 3
E(network.JR.long)$lty <- color.mat

pdf(file = "figures/JR.long.network.pdf", width = 6, height = 6)

plot(network.JR.long , layout=layout.circle,
     main="JR long synchrony",
     vertex.label = colnames(df.synchrony.JR.matrix.long),
     vertex.frame.color = "transparent",
     vertex.label.family="Helvetica",
     #vertex.label.cex = 1,
     #vertex.label.dist= 2,
     #vertex.label.degree = c(pi/2,pi/2,-pi/2),
     vertex.label.color = "black",
     vertex.color = "grey80",
     vertex.size = 30,
     edge.width = widths,
     edge.color = "black",
     edge.arrow.size = 0.5,
     edge.curved = TRUE)

dev.off()

##### Kirby Canyon -----

####### Short -----

# Create data
df.synchrony.KC.matrix.short <- df.synchrony.KC %>%
  filter(species1 != species2) %>%
  select(species1,species2,aggresShort) %>%
  spread(species2,aggresShort) %>%
  column_to_rownames("species1")%>%
  as.matrix()

network.KC.short.pos <- unname(abs(df.synchrony.KC.matrix.short))
network.KC.short <- graph_from_adjacency_matrix(network.KC.short.pos>0)
E(network.KC.short)$weight <- as.numeric(network.KC.short.pos[!is.na(network.KC.short.pos)])
widths <- E(network.KC.short)$weight*2
color.mat <-  as.numeric(df.synchrony.KC.matrix.short[!is.na(df.synchrony.KC.matrix.short)])
color.mat[which(color.mat > 1)] <- 1
color.mat[which(color.mat < 1)] <- 3
E(network.KC.short)$lty <- color.mat

pdf(file = "figures/KC.short.network.pdf", width = 6, height = 6)

plot(network.KC.short , layout=layout.circle,
     main="KC short synchrony",
     vertex.label = colnames(df.synchrony.KC.matrix.short),
     vertex.frame.color = "transparent",
     vertex.label.family="Helvetica",
     #vertex.label.cex = 1,
     #vertex.label.dist= 2,
     #vertex.label.degree = c(pi/2,pi/2,-pi/2),
     vertex.label.color = "black",
     vertex.color = "grey80",
     vertex.size = 30,
     edge.width = widths,
     edge.color = "black",
     edge.arrow.size = 0.5,
     edge.curved = TRUE)

dev.off()

####### Short -----

# Create data
df.synchrony.KC.matrix.long <- df.synchrony.KC %>%
  filter(species1 != species2) %>%
  select(species1,species2,aggresLong) %>%
  spread(species2,aggresLong) %>%
  column_to_rownames("species1")%>%
  as.matrix()

network.KC.long.pos <- unname(abs(df.synchrony.KC.matrix.long))
network.KC.long <- graph_from_adjacency_matrix(network.KC.long.pos>0)
E(network.KC.long)$weight <- as.numeric(network.KC.long.pos[!is.na(network.KC.long.pos)])
widths <- E(network.KC.long)$weight*2
color.mat <-  as.numeric(df.synchrony.KC.matrix.long[!is.na(df.synchrony.KC.matrix.long)])
color.mat[which(color.mat > 1)] <- 1
color.mat[which(color.mat < 1)] <- 3
E(network.KC.long)$lty <- color.mat

pdf(file = "figures/KC.long.network.pdf", width = 6, height = 6)

plot(network.KC.long , layout=layout.circle,
     main="KC long synchrony",
     vertex.label = colnames(df.synchrony.KC.matrix.long),
     vertex.frame.color = "transparent",
     vertex.label.family="Helvetica",
     #vertex.label.cex = 1,
     #vertex.label.dist= 2,
     #vertex.label.degree = c(pi/2,pi/2,-pi/2),
     vertex.label.color = "black",
     vertex.color = "grey80",
     vertex.size = 30,
     edge.width = widths,
     edge.color = "black",
     edge.arrow.size = 0.5,
     edge.curved = TRUE)

dev.off()

# Jeremy wasn't sure what this code was for
alphamat.pos <- unname(abs(df.synchrony.JR.matrix.short))
g <- igraph::graph_from_adjacency_matrix(alphamat.pos  > 0)
E(g)$weight <- as.numeric(alphamat.pos )
widths <- E(g)$weight*2
color.mat <-  as.numeric(df.synchrony.JR.matrix.short)
color.mat[which(color.mat > 1)] <- 1
color.mat[which(color.mat < 1)] <- 3
E(g)$lty <- color.mat
#widths[widths > 1] <- sqrt(widths)
                        plot(g,
                          #main = "Species interaction network",
                          #main = title,
                          #margin = c(0, -0.15, 0, -0.15),
                          #xlim = c(-1.25, 1.25), 
                         # ylim = c(-1.25, 1.25),
                          #vertex.label = c("Radish","Field   \nbean   ", "Tomato"),
                          vertex.label.family="Helvetica",   
                          #vertex.label.cex = 1,
                          #vertex.label.dist= 5,
                          #vertex.label.degree = c(pi/2,pi/2,-pi/2),
                          #vertex.label.color = "black",
                          vertex.size = 1,
                          #vertex.color = "grey80",
                          vertex.frame.color = "transparent",
                          edge.curved = TRUE,
                          edge.width = widths,
                          edge.arrow.size = 2,
                          #edge.arrow.mode = c(0, 2, 2,
                           #                   2, 0, 2,
                           #                   2, 2, 0),
                          edge.color = "black",
                          edge.loop.angle = 0.75)
                        
                        

