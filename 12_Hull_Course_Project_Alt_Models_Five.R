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
altmodels <- read.csv("altmodels.csv", header = T)

# load model a
sigmodela5 <- read.csv("sigmodela5.csv", header = T)

## subset each model into different dataframe
aClimateSegmenteClimateSegment <- subset(altmodels, MODELNAME %in% "a Climate Segment e Climate Segment")
bClimateSegmentfClimateSegment <- subset(altmodels, MODELNAME %in% "b Climate Segment f Climate Segment")

# format into list for multinomial test
## Group counts by species
sigmodela6 <- subset(sigmodela5, select = c(speciesName, count))
sigmodela7 <- sigmodela6 %>% group_by(speciesName) %>% summarize(speciesCount = list(count))
## Create list of vectors, with each vector containing counts of species
sigmodela8 <- as.vector(sigmodela7$speciesCount)
## Count number of species
z <- as.numeric(length(sigmodela8))
## Create empty vector for p-values
pvaluesaClimateSegmenteClimateSegment <- array(c(0), dim = z)
pvaluesbClimateSegmentfClimateSegment <- array(c(0), dim = z)

## Conduct Exact Multinomial Test
## Conduct exact multinomial test and output p-values for each species
for(i in 1:z){
  pvaluesaClimateSegmenteClimateSegment[i] <- multinomial.test(sigmodela8[[i]], aClimateSegmenteClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluesbClimateSegmentfClimateSegment[i] <- multinomial.test(sigmodela8[[i]], bClimateSegmentfClimateSegment$WEIGHTEDCHANCE)$p.value}

## Set vector of p-values as dataframe, insert species names, and correct p-values for using same dataframe
## Set vector of p-values as dataframe
pvaluesaClimateSegmenteClimateSegment2 <- as.data.frame(pvaluesaClimateSegmenteClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesaClimateSegmenteClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesaClimateSegmenteClimateSegment2$padj = p.adjust(pvaluesaClimateSegmenteClimateSegment2$pvaluesaClimateSegmenteClimateSegment, method="fdr")
## Add model name
pvaluesaClimateSegmenteClimateSegment2$model <- "a Climate Segment e Climate Segment"
## Rename original p-value column
colnames(pvaluesaClimateSegmenteClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesaClimateSegmenteClimateSegment2, "pvaluesaClimateSegmenteClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluesbClimateSegmentfClimateSegment2 <- as.data.frame(pvaluesbClimateSegmentfClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesbClimateSegmentfClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesbClimateSegmentfClimateSegment2$padj = p.adjust(pvaluesbClimateSegmentfClimateSegment2$pvaluesbClimateSegmentfClimateSegment, method="fdr")
## Add model name
pvaluesbClimateSegmentfClimateSegment2$model <- "b Climate Segment f Climate Segment"
## Rename original p-value column
colnames(pvaluesbClimateSegmentfClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesbClimateSegmentfClimateSegment2, "pvaluesbClimateSegmentfClimateSegment.csv")