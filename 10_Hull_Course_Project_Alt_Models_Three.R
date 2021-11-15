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
aClimateSegmentcClimateSegment <- subset(altmodels, MODELNAME %in% "a Climate Segment c Climate Segment")
bClimateSegmentdClimateSegment <- subset(altmodels, MODELNAME %in% "b Climate Segment d Climate Segment")
cClimateSegmenteClimateSegment <- subset(altmodels, MODELNAME %in% "c Climate Segment e Climate Segment")
dClimateSegmentfClimateSegment <- subset(altmodels, MODELNAME %in% "d Climate Segment f Climate Segment")

# format into list for multinomial test
## Group counts by species
sigmodela6 <- subset(sigmodela5, select = c(speciesName, count))
sigmodela7 <- sigmodela6 %>% group_by(speciesName) %>% summarize(speciesCount = list(count))
## Create list of vectors, with each vector containing counts of species
sigmodela8 <- as.vector(sigmodela7$speciesCount)
## Count number of species
z <- as.numeric(length(sigmodela8))
## Create empty vector for p-values
pvaluesaClimateSegmentcClimateSegment <- array(c(0), dim = z)
pvaluesbClimateSegmentdClimateSegment <- array(c(0), dim = z)
pvaluescClimateSegmenteClimateSegment <- array(c(0), dim = z)
pvaluesdClimateSegmentfClimateSegment <- array(c(0), dim = z)

## Conduct Exact Multinomial Test
## Conduct exact multinomial test and output p-values for each species
for(i in 1:z){
  pvaluesaClimateSegmentcClimateSegment[i] <- multinomial.test(sigmodela8[[i]], aClimateSegmentcClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluesbClimateSegmentdClimateSegment[i] <- multinomial.test(sigmodela8[[i]], bClimateSegmentdClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluescClimateSegmenteClimateSegment[i] <- multinomial.test(sigmodela8[[i]], cClimateSegmenteClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluesdClimateSegmentfClimateSegment[i] <- multinomial.test(sigmodela8[[i]], dClimateSegmentfClimateSegment$WEIGHTEDCHANCE)$p.value}

## Set vector of p-values as dataframe, insert species names, and correct p-values for using same dataframe
## Set vector of p-values as dataframe
pvaluesaClimateSegmentcClimateSegment2 <- as.data.frame(pvaluesaClimateSegmentcClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesaClimateSegmentcClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesaClimateSegmentcClimateSegment2$padj = p.adjust(pvaluesaClimateSegmentcClimateSegment2$pvaluesaClimateSegmentcClimateSegment, method="fdr")
## Add model name
pvaluesaClimateSegmentcClimateSegment2$model <- "a Climate Segment c Climate Segment"
## Rename original p-value column
colnames(pvaluesaClimateSegmentcClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesaClimateSegmentcClimateSegment2, "pvaluesaClimateSegmentcClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluesbClimateSegmentdClimateSegment2 <- as.data.frame(pvaluesbClimateSegmentdClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesbClimateSegmentdClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesbClimateSegmentdClimateSegment2$padj = p.adjust(pvaluesbClimateSegmentdClimateSegment2$pvaluesbClimateSegmentdClimateSegment, method="fdr")
## Add model name
pvaluesbClimateSegmentdClimateSegment2$model <- "b Climate Segment d Climate Segment"
## Rename original p-value column
colnames(pvaluesbClimateSegmentdClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesbClimateSegmentdClimateSegment2, "pvaluesbClimateSegmentdClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluescClimateSegmenteClimateSegment2 <- as.data.frame(pvaluescClimateSegmenteClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluescClimateSegmenteClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluescClimateSegmenteClimateSegment2$padj = p.adjust(pvaluescClimateSegmenteClimateSegment2$pvaluescClimateSegmenteClimateSegment, method="fdr")
## Add model name
pvaluescClimateSegmenteClimateSegment2$model <- "c Climate Segment e Climate Segment"
## Rename original p-value column
colnames(pvaluescClimateSegmenteClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluescClimateSegmenteClimateSegment2, "pvaluescClimateSegmenteClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluesdClimateSegmentfClimateSegment2 <- as.data.frame(pvaluesdClimateSegmentfClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesdClimateSegmentfClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesdClimateSegmentfClimateSegment2$padj = p.adjust(pvaluesdClimateSegmentfClimateSegment2$pvaluesdClimateSegmentfClimateSegment, method="fdr")
## Add model name
pvaluesdClimateSegmentfClimateSegment2$model <- "d Climate Segment f Climate Segment"
## Rename original p-value column
colnames(pvaluesdClimateSegmentfClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesdClimateSegmentfClimateSegment2, "pvaluesdClimateSegmentfClimateSegment.csv")