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
aClimateSegmentdClimateSegment <- subset(altmodels, MODELNAME %in% "a Climate Segment d Climate Segment")
bClimateSegmenteClimateSegment <- subset(altmodels, MODELNAME %in% "b Climate Segment e Climate Segment")
cClimateSegmentfClimateSegment <- subset(altmodels, MODELNAME %in% "c Climate Segment f Climate Segment")

# format into list for multinomial test
## Group counts by species
sigmodela6 <- subset(sigmodela5, select = c(speciesName, count))
sigmodela7 <- sigmodela6 %>% group_by(speciesName) %>% summarize(speciesCount = list(count))
## Create list of vectors, with each vector containing counts of species
sigmodela8 <- as.vector(sigmodela7$speciesCount)
## Count number of species
z <- as.numeric(length(sigmodela8))
## Create empty vector for p-values
pvaluesaClimateSegmentdClimateSegment <- array(c(0), dim = z)
pvaluesbClimateSegmenteClimateSegment <- array(c(0), dim = z)
pvaluescClimateSegmentfClimateSegment <- array(c(0), dim = z)

## Conduct Exact Multinomial Test
## Conduct exact multinomial test and output p-values for each species
for(i in 1:z){
  pvaluesaClimateSegmentdClimateSegment[i] <- multinomial.test(sigmodela8[[i]], aClimateSegmentdClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluesbClimateSegmenteClimateSegment[i] <- multinomial.test(sigmodela8[[i]], bClimateSegmenteClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluescClimateSegmentfClimateSegment[i] <- multinomial.test(sigmodela8[[i]], cClimateSegmentfClimateSegment$WEIGHTEDCHANCE)$p.value}

## Set vector of p-values as dataframe, insert species names, and correct p-values for using same dataframe
## Set vector of p-values as dataframe
pvaluesaClimateSegmentdClimateSegment2 <- as.data.frame(pvaluesaClimateSegmentdClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesaClimateSegmentdClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesaClimateSegmentdClimateSegment2$padj = p.adjust(pvaluesaClimateSegmentdClimateSegment2$pvaluesaClimateSegmentdClimateSegment, method="fdr")
## Add model name
pvaluesaClimateSegmentdClimateSegment2$model <- "a Climate Segment d Climate Segment"
## Rename original p-value column
colnames(pvaluesaClimateSegmentdClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesaClimateSegmentdClimateSegment2, "pvaluesaClimateSegmentdClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluesbClimateSegmenteClimateSegment2 <- as.data.frame(pvaluesbClimateSegmenteClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesbClimateSegmenteClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesbClimateSegmenteClimateSegment2$padj = p.adjust(pvaluesbClimateSegmenteClimateSegment2$pvaluesbClimateSegmenteClimateSegment, method="fdr")
## Add model name
pvaluesbClimateSegmenteClimateSegment2$model <- "b Climate Segment e Climate Segment"
## Rename original p-value column
colnames(pvaluesbClimateSegmenteClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesbClimateSegmenteClimateSegment2, "pvaluesbClimateSegmenteClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluescClimateSegmentfClimateSegment2 <- as.data.frame(pvaluescClimateSegmentfClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluescClimateSegmentfClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluescClimateSegmentfClimateSegment2$padj = p.adjust(pvaluescClimateSegmentfClimateSegment2$pvaluescClimateSegmentfClimateSegment, method="fdr")
## Add model name
pvaluescClimateSegmentfClimateSegment2$model <- "c Climate Segment f Climate Segment"
## Rename original p-value column
colnames(pvaluescClimateSegmentfClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluescClimateSegmentfClimateSegment2, "pvaluescClimateSegmentfClimateSegment.csv")