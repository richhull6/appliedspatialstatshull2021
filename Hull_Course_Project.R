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
## Construct dataframe with a latitudinal segment for each species and a column of 0's
speciesnames <- data.frame(unique(modela$speciesName))
colnames(speciesnames)[1] <- "speciesName"
y <- length(speciesnames$speciesName)
climatesegs <- data.frame(unique(modela3$climateSegment))
colnames(climatesegs)[1] <- "climateSegs"
z <- length(climatesegs$climateSegs)
climatesegs2 <- do.call("rbind", replicate(y, climatesegs, simplify = FALSE))
speciesnames2 <- do.call("rbind", replicate(z, speciesnames, simplify = FALSE))
speciesnames3 <- data.frame(speciesnames2[order(speciesnames2$speciesName),])
colnames(speciesnames3)[1] <- "species"
climatesegs2$species <- speciesnames3$species
climatesegs2$zero <- 0
## Re-order counts of species within latitudinal segments by species and insert "l" in latitude column
modela4 <- data.frame(modela3[order(modela3$speciesName, modela3$climateSegment),])
modela4$climateseg2 <- paste(modela4$climateSegment,modela4$c, sep ="")
## Keep only desired columns and rename columns appropriately between climateseg2 and modela4
modela5 <- subset(modela4, select = c(speciesName, count, climateseg2))
colnames(modela5)[3] <- "climateseg"
colnames(climatesegs2) <- c("climateseg", "speciesName", "count")
## Insert 0 for climate seg occurences with no records
climatesegsc <- data.frame(unique(modela5$climateseg))
colnames(climatesegsc)[1] <- "climateseg"
modela6 <- complete(modela5, climatesegsc, speciesName, fill = list(count = 0))
# Reorder data by climate segments
modela7 <- modela6[with(modela6, order(speciesName, climateseg)), ]

## prepare data for exact multinomial test
## Count number of species
z <- length(speciesnames$speciesName)
## Group counts by species
modela8 <- subset(modela7, select = c(speciesName, count))
modela9 <- modela8 %>% group_by(speciesName) %>% summarize(speciesCount = list(count))
## Create empty vector for p-values
pvalues1 <- array(c(0), dim = z)
## Create list of vectors, with each vector containing counts of species
modela10 <- (as.vector(modela9$speciesCount))

## Conduct Exact Multinomial Test
## Conduct exact multinomial test and output p-values for each species
set.seed(1)
for(i in 1:z){
  pvalues1[i] = 
    multinomial.test(modela10[[i]], climatesegmentprobs$WEIGHTEDCHANCE, useChisq = TRUE)$p.value}

## Set vector of p-values as dataframe, insert species names, and correct p-values for using same dataframe
## Set vector of p-values as dataframe
pvalues2 <- as.data.frame(pvalues1)
## Insert species names and pair with corresponding p-values
pvalues2$speciesName <- modela9$speciesName
## Correct p-values by false discovery rate
pvalues2$padj = p.adjust(pvalues2$pvalues1, method="fdr")
write.csv(pvalues2, "preliminaryresults.csv")




