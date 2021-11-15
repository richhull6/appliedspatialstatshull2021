## install/load necessary packages
# install/load data parsing packages
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(tidyr)){install.packages("tidyr")}
library(tidyr)
if(!require(data.table)){install.packages("data.table")}
library(data.table)
if(!require(stringr)){install.packages("stringr")}
library(stringr)
if(!require(lubridate)){install.packages("lubridate")}
library(lubridate)
# install/load map and raster packages
if(!require(sp)){install.packages("sp")}
library(sp)
if(!require(raster)){install.packages("raster")}
library(raster)
# install/load exact multinomial test packages
if(!require(EMT)){install.packages("EMT")}
library(EMT)
if(!require(purrr)){install.packages("purrr")}
library(purrr)

# load climate segment probabilities
climatesegmentprobs <- read.csv("climatesegmentprobs.csv", header = T)

# load model a
modela <- read.csv("modela.csv", header = T)

## Calculate counts of species per latitudinal segments
## Create new column with occurence values (1)
modela$occval <- 1
## Sum occurences of species within climatic segments for observed model
modela2 <- aggregate(modela$occval, by=list(climatesp=modela$climatesp), FUN=sum)
colnames(modela2)[2] <- "count"
## Split column with latitudinal segments and species names
modela3 <- data.frame(str_split_fixed(modela2$climatesp, "Divide", 2))
modela3$count <- modela2$count
colnames(modela3)[1] <- "climateSegment"
colnames(modela3)[2] <- "speciesName"

## Add 0's for species that do not occur in specific latitudinal segments
## Insert 0 for climate seg occurences with no records
climatesegsc <- data.frame(unique(modela3$climateSegment))
colnames(climatesegsc)[1] <- "climateSegment"
modela4 <- complete(modela3, climatesegsc, speciesName, fill = list(count = 0))
# Reorder data by climate segments
modela5 <- modela4[with(modela4, order(speciesName, climateSegment)), ]

## prepare data for exact multinomial test
## Group counts by species
modela6 <- subset(modela5, select = c(speciesName, count))
modela7 <- modela6 %>% group_by(speciesName) %>% summarize(speciesCount = list(count))
## Create list of vectors, with each vector containing counts of species
modela8 <- (as.vector(modela7$speciesCount))
## Count number of species
z <- as.numeric(length(modela8))
## Create empty vector for p-values
pvalues1 <- array(c(0), dim = c(1))

## Conduct Exact Multinomial Test
## Conduct exact multinomial test and output p-values for each species
for (i in 1:z){
  pvalues1[i] <- multinomial.test(modela8[[i]], climatesegmentprobs$WEIGHTEDCHANCE)$p.value}

## Set vector of p-values as dataframe, insert species names, and correct p-values for using same dataframe
## Set vector of p-values as dataframe
pvalues2 <- as.data.frame(pvalues1)
## Insert species names and pair with corresponding p-values
pvalues2$speciesName <- modela7$speciesName
## Correct p-values by false discovery rate
pvalues2$padj = p.adjust(pvalues2$pvalues1, method="fdr")
write.csv(pvalues2, "preliminaryresults.csv")




