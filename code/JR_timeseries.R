rm(list=ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library(viridis)

JR_cover <- read.csv("JR_cover2023.csv")
JR_sp_names <- read.csv("JR_species names.csv")

JR_sp_names$GenusSpecies <- paste(
  JR_sp_names$Genus,
  JR_sp_names$Species, sep=" ")

JR_sp_names$Genus <- NULL
JR_sp_names$Species <- NULL

SpeciesCode <- join_by(species ==  SpeciesCode)
JR_cover <- left_join(JR_cover, JR_sp_names, SpeciesCode)

JR_cover$species <- NULL

JR_cover <- JR_cover %>%
  mutate(GenusSpecies = ifelse(GenusSpecies == "Escholtzia californica", "Eschscholzia californica", GenusSpecies)) %>%
  mutate(GenusSpecies = ifelse(GenusSpecies == "Bare Rock", "Bare", GenusSpecies)) %>%
  mutate(GenusSpecies = ifelse(GenusSpecies == "Bare Ground", "Bare", GenusSpecies))

focal_species <- c("Bromus mollis","Eschscholzia californica","Calycadenia multiglandulosa","Hemizonia luzulaefolia",
                   "Lasthenia californica","Lolium multiflorum","Microseris douglasii","Lotus subpinnatus","Plantago erecta","Vulpia microstachys")
focal_colors <- c("darkblue", "cornflowerblue", "cyan", "aquamarine","chartreuse","darkgoldenrod1","darkorange1","brown1","deeppink2","chocolate4")
grasses <- c("Bromus mollis","Lolium multiflorum","Vulpia microstachys")
grasses_colors <- c("darkblue","darkgoldenrod1","chocolate4")
herbs <- c("Calycadenia multiglandulosa","Eschscholzia californica","Hemizonia luzulaefolia","Lasthenia californica","Lous subpinnatus","Microseris douglasii","Plantago erecta")
herbs_colors <- c("cornflowerblue", "cyan", "aquamarine", "chartreuse","darkorange1","brown1","deeppink2")

# for loop for separate plots for each species
for(i in seq_along(focal_species)){
  GenusSpecies <- focal_species[i]
  color <- focal_colors[i]
  print(GenusSpecies)
  this.plant <- JR_cover[JR_cover$GenusSpecies == GenusSpecies,]
  this.plant <- filter(this.plant, treatment == "c")
  this.plant <- group_by(this.plant, year)
  this.plant <- mutate(this.plant, avg_cover = mean(cover))
  cover_plot <- ggplot(data= this.plant, aes(x = year, y = avg_cover)) +
    geom_line(aes(color=GenusSpecies), size = 1.5) +
    scale_color_manual(values = color) +
    coord_cartesian(ylim = c(0, 60)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    labs(title = GenusSpecies, x = "Year", y = "Mean Cover")
  
  ggsave(filename = sprintf("timeseries_figures/%s.pdf", GenusSpecies), plot=cover_plot,
         height = 8, width = 14)
}

# All focal species in the same plot
focal_cover <- JR_cover[JR_cover$GenusSpecies %in% focal_species,] %>%
  group_by(GenusSpecies, year) %>%
  summarize(avg_cover = mean(cover, na.rm=TRUE))

focal_cover_plot <- ggplot(data = focal_cover, aes(x = year, y = avg_cover)) +
  geom_line(aes(color = GenusSpecies, group = GenusSpecies), size = 1) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 10), 
        legend.position = "right") + 
  labs(x = "Year", y = "Mean Cover") +
  scale_color_manual(values = focal_colors)
ggsave(filename = "timeseries_figures/focal_cover_plot.pdf", plot = focal_cover_plot, height = 6, width = 10)

grasses_cover <- JR_cover[JR_cover$GenusSpecies %in% grasses,] %>%
  group_by(GenusSpecies, year) %>%
  summarize(avg_cover = mean(cover, na.rm=TRUE))

# Grasses plot
grasses_cover_plot <- ggplot(data = grasses_cover, aes(x = year, y = avg_cover)) +
  geom_line(aes(color = GenusSpecies, group = GenusSpecies), size = 1) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 10), 
        legend.position = "right") + 
  labs(x = "Year", y = "Mean Cover") +  
  scale_color_manual(values = grasses_colors)
ggsave(filename = "timeseries_figures/grasses_cover_plot.pdf", plot = grasses_cover_plot, height = 6, width = 10)

# Herbs plot
herbs_cover <- JR_cover[JR_cover$GenusSpecies %in% herbs,] %>%
  group_by(GenusSpecies, year) %>%
  summarize(avg_cover = mean(cover, na.rm=TRUE))

herbs_cover_plot <- ggplot(data = herbs_cover, aes(x = year, y = avg_cover)) +
  geom_line(aes(color = GenusSpecies, group = GenusSpecies), size = 1) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 10), 
        legend.position = "right") + 
  labs(x = "Year", y = "Mean Cover") +  
  scale_color_manual(values = herbs_colors)
ggsave(filename = "timeseries_figures/herbs_cover_plot.pdf", plot = herbs_cover_plot, height = 6, width = 10)

# Faceted all focal species plot
focal_cover$GenusSpecies <- factor(focal_cover$GenusSpecies, levels = focal_species)

faceted_cover_plot <- ggplot(data = focal_cover, aes(x = year, y = avg_cover)) +
  geom_line(aes(color = GenusSpecies, group = GenusSpecies), size = 1) + 
  scale_color_manual(values = focal_colors) +
  coord_cartesian(ylim = c(0, 60)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none",
        strip.text = element_text(face = "italic")) + 
  labs(x = "Year", y = "Mean Percent Cover") +
  facet_wrap(~ GenusSpecies, ncol=2, strip.position = "bottom")
ggsave(filename = "timeseries_figures/faceted_cover_plot.pdf", plot = faceted_cover_plot, height = 8, width = 8)
