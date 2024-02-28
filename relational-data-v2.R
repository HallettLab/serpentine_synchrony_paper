################################################################################
### WRANGLE: Relational data                                                 ### 
################################################################################

## OBJECTIVES:
# To learn what relational data are and why this is a useful data structure
# To learn how to join datasets using dplyr syntax
# To become comfortable developing workflows that integrate multiple data frames

# Lesson material adapted from: Paula Andrea Martinez, Timothée Poisot (eds): "Data Carpentry: SQL for Ecology lesson.
# "Version 2017.04.01, April 2017,
#http://www.datacarpentry.org/sql-ecology-lesson/

# You can learn more about the Portal Project here:
# https://portal.weecology.org/


## WHY USE A RELATIONAL DATABASE?
# Using a relational database serves several purposes
# 
# It keeps your data separate from your analysis
# This means there’s no risk of accidentally changing data when you analyze it
# If we get new data we can just rerun the script
# It’s fast, even for large amounts of data
# It improves quality control of data entry

## WHAT ARE RELATIONAL DATABASES?
# Relational databases store data in tables with fields (columns) and records (rows)
# Data in tables has types, and all values in a field have the same type (list of data types)
# Queries (or dplyr syntax) let us look up data or make calculations based on columns

## Characteristics: 
# Every row-column combination contains a single atomic value, i.e., not containing parts we might want to work with separately.
# One field per type of information
# No redundant information
# Split into separate tables with one table per class of information
# Needs an identifier in common between tables – shared column - to reconnect (known as a foreign key).


# Load the tidyverse
library(tidyverse)


# For practice with joins, generate two tibbles:
# If you forget what the different joins do, mess around with these practice tibbles! 
tibble1<-as_tibble(data.frame(x1=c(1,3,5,7,9),x2=c(2,4,6,8,10)))
tibble2<-as_tibble(data.frame(x2=c(4,6,8,10,12),x3=c(1,4,7,10,14)))

#joins can work without specifying the column if there is only one column they have in common
#Otherwise, you must specify using by="column_name".
#Most of the time, you will probably want an inner join as this retains only the data included in both datasets

#Inner join - both of these give the same thing in this case, since only one column is shared
inner_join(tibble1,tibble2)
inner_join(tibble1,tibble2,by="x2") 

#Left join
left_join(tibble1,tibble2,by="x2")

#Right join
right_join(tibble1,tibble2,by="x2")

#Full join
full_join(tibble1,tibble2,by="x2")

#Joins will (probably) be most useful with some categorical data...
tibble3<-as_tibble(data.frame(mammal=c("squirrel","dog","cat","bear","deer"),quantity=c(7,1,4,2,5)))
tibble4<-as_tibble(data.frame(mammal=c("squirrel","dog","cat","possum"),avgweight=c(2,25,8,2)))

inner_join(tibble3,tibble4,by="mammal")

left_join(tibble3,tibble4,by="mammal")

right_join(tibble3,tibble4,by="mammal")

full_join(tibble3,tibble4,by="mammal")


# Handy standard error function
calcSE <- function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

## Read in the data
surveys <- read_csv("surveys.csv")
species <- read_csv("species.csv")
plots <- read_csv("plots.csv")

## QUESTION: What are the primary keys of the plots, species and surveys data frames?
#plot:plot_id
#species:species_id
#surveys:record_id 


## EXERCISE: How has the hindfoot length and weight of Dipodomys genus changed over time?


# Which files have the data I need? 
species and survey

# What operations would I need to perform if I were doing these analyses by hand?
 




##############################################
## Example workflow to answer this question ##
##############################################

# join together survey and species data
sumdat <- inner_join(surveys, species, by="species_id") %>%
  
  # just retain Dipodomys records
  filter(genus == "Dipodomys") %>%
  
  # group by year
  group_by(year) %>%
  
  # calculate the replication number, mean and se of length and mean and se of weight
  summarize(repnum = n(), 
            meanlength = mean(hindfoot_length, na.rm=T), 
            selength = calcSE(hindfoot_length),
            meanweight = mean(weight, na.rm=T),
            seweight = calcSE(weight))

# graph length
ggplot(sumdat, aes(x = year, y= meanlength)) + geom_point() + geom_line() +
  geom_errorbar(aes(ymin = meanlength - selength, ymax = meanlength + selength)) 

# graph weight
ggplot(sumdat, aes(x = year, y= meanweight)) + geom_point() + geom_line() +
  geom_errorbar(aes(ymin = meanweight - seweight, ymax = meanweight + seweight))


ggplot(sumdat, aes(y = meanlength, x= meanweight)) + geom_point()
## QUESTION: Could the species identity be affecting these patterns? How would you check? 




##############################################
##             YOUR TURN                    ##
##############################################

## Within your group, use the imported files, join, and dplyr functions as needed to answer the following questions:
## Just write the code to generate the answer in a tibble; you do not need to copy the numbers from the tibble

## QUESTIONS: 
# Q1) How many plots from each type are there? Determine which file(s) you need and which operations to perform.
plots %>% group_by(plot_type) %>% count()

# Q2) How many specimens of each species were captured in each type of plot? Determine which file(s) you need and which operations to perform.
inner_join(plots, surveys) %>% group_by(plot_type, species_id) %>% count()

# Q3) What is the average weight of each taxa? Determine which file(s) you need and which operations to perform.
#Hint: find the average within each species first, then average species within each taxa these to find the average for the whole taxa
inner_join(species, surveys) %>% group_by(species_id) %>% mutate(spmean=mean(weight, na.rm=T)) %>% group_by(taxa) %>% summarize(mean(spmean, na.rm=T))




