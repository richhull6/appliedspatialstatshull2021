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
aClimateSegmentbClimateSegment <- subset(altmodels, MODELNAME %in% "a Climate Segment b Climate Segment")
bClimateSegmentcClimateSegment <- subset(altmodels, MODELNAME %in% "b Climate Segment c Climate Segment")
cClimateSegmentdClimateSegment <- subset(altmodels, MODELNAME %in% "c Climate Segment d Climate Segment")
dClimateSegmenteClimateSegment <- subset(altmodels, MODELNAME %in% "d Climate Segment e Climate Segment")
eClimateSegmentfClimateSegment <- subset(altmodels, MODELNAME %in% "e Climate Segment f Climate Segment")

# format into list for multinomial test
## Group counts by species
sigmodela6 <- subset(sigmodela5, select = c(speciesName, count))
sigmodela7 <- sigmodela6 %>% group_by(speciesName) %>% summarize(speciesCount = list(count))
## Create list of vectors, with each vector containing counts of species
sigmodela8 <- as.vector(sigmodela7$speciesCount)
## Count number of species
z <- as.numeric(length(sigmodela8))
## Create empty vector for p-values
pvaluesaClimateSegmentbClimateSegment <- array(c(0), dim = z)
pvaluesbClimateSegmentcClimateSegment <- array(c(0), dim = z)
pvaluescClimateSegmentdClimateSegment <- array(c(0), dim = z)
pvaluesdClimateSegmenteClimateSegment <- array(c(0), dim = z)
pvalueseClimateSegmentfClimateSegment <- array(c(0), dim = z)

## Conduct Exact Multinomial Test
## Conduct exact multinomial test and output p-values for each species
for(i in 1:z){
  pvaluesaClimateSegmentbClimateSegment[i] <- multinomial.test(sigmodela8[[i]], aClimateSegmentbClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluesbClimateSegmentcClimateSegment[i] <- multinomial.test(sigmodela8[[i]], bClimateSegmentcClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluescClimateSegmentdClimateSegment[i] <- multinomial.test(sigmodela8[[i]], cClimateSegmentdClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluesdClimateSegmenteClimateSegment[i] <- multinomial.test(sigmodela8[[i]], dClimateSegmenteClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvalueseClimateSegmentfClimateSegment[i] <- multinomial.test(sigmodela8[[i]], eClimateSegmentfClimateSegment$WEIGHTEDCHANCE)$p.value}

## Set vector of p-values as dataframe, insert species names, and correct p-values for using same dataframe
## Set vector of p-values as dataframe
pvaluesaClimateSegmentbClimateSegment2 <- as.data.frame(pvaluesaClimateSegmentbClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesaClimateSegmentbClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesaClimateSegmentbClimateSegment2$padj = p.adjust(pvaluesaClimateSegmentbClimateSegment2$pvaluesaClimateSegmentbClimateSegment, method="fdr")
## Add model name
pvaluesaClimateSegmentbClimateSegment2$model <- "a Climate Segment b Climate Segment"
## Rename original p-value column
colnames(pvaluesaClimateSegmentbClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesaClimateSegmentbClimateSegment2, "pvaluesaClimateSegmentbClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluesbClimateSegmentcClimateSegment2 <- as.data.frame(pvaluesbClimateSegmentcClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesbClimateSegmentcClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesbClimateSegmentcClimateSegment2$padj = p.adjust(pvaluesbClimateSegmentcClimateSegment2$pvaluesbClimateSegmentcClimateSegment, method="fdr")
## Add model name
pvaluesbClimateSegmentcClimateSegment2$model <- "b Climate Segment c Climate Segment"
## Rename original p-value column
colnames(pvaluesbClimateSegmentcClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesbClimateSegmentcClimateSegment2, "pvaluesbClimateSegmentcClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluescClimateSegmentdClimateSegment2 <- as.data.frame(pvaluescClimateSegmentdClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluescClimateSegmentdClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluescClimateSegmentdClimateSegment2$padj = p.adjust(pvaluescClimateSegmentdClimateSegment2$pvaluescClimateSegmentdClimateSegment, method="fdr")
## Add model name
pvaluescClimateSegmentdClimateSegment2$model <- "c Climate Segment d Climate Segment"
## Rename original p-value column
colnames(pvaluescClimateSegmentdClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluescClimateSegmentdClimateSegment2, "pvaluescClimateSegmentdClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluesdClimateSegmenteClimateSegment2 <- as.data.frame(pvaluesdClimateSegmenteClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesdClimateSegmenteClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesdClimateSegmenteClimateSegment2$padj = p.adjust(pvaluesdClimateSegmenteClimateSegment2$pvaluesdClimateSegmenteClimateSegment, method="fdr")
## Add model name
pvaluesdClimateSegmenteClimateSegment2$model <- "d Climate Segment e Climate Segment"
## Rename original p-value column
colnames(pvaluesdClimateSegmenteClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesdClimateSegmenteClimateSegment2, "pvaluesdClimateSegmenteClimateSegment.csv")

## Set vector of p-values as dataframe
pvalueseClimateSegmentfClimateSegment2 <- as.data.frame(pvalueseClimateSegmentfClimateSegment)
## Insert species names and pair with corresponding p-values
pvalueseClimateSegmentfClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvalueseClimateSegmentfClimateSegment2$padj = p.adjust(pvalueseClimateSegmentfClimateSegment2$pvalueseClimateSegmentfClimateSegment, method="fdr")
## Add model name
pvalueseClimateSegmentfClimateSegment2$model <- "e Climate Segment f Climate Segment"
## Rename original p-value column
colnames(pvalueseClimateSegmentfClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvalueseClimateSegmentfClimateSegment2, "pvalueseClimateSegmentfClimateSegment.csv")