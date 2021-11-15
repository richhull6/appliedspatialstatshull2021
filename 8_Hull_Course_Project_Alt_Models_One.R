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
aClimateSegmentaClimateSegment <- subset(altmodels, MODELNAME %in% "a Climate Segment a Climate Segment")
bClimateSegmentbClimateSegment <- subset(altmodels, MODELNAME %in% "b Climate Segment b Climate Segment")
cClimateSegmentcClimateSegment <- subset(altmodels, MODELNAME %in% "c Climate Segment c Climate Segment")
dClimateSegmentdClimateSegment <- subset(altmodels, MODELNAME %in% "d Climate Segment d Climate Segment")
eClimateSegmenteClimateSegment <- subset(altmodels, MODELNAME %in% "e Climate Segment e Climate Segment")
fClimateSegmentfClimateSegment <- subset(altmodels, MODELNAME %in% "f Climate Segment f Climate Segment")

# format into list for multinomial test
## Group counts by species
sigmodela6 <- subset(sigmodela5, select = c(speciesName, count))
sigmodela7 <- sigmodela6 %>% group_by(speciesName) %>% summarize(speciesCount = list(count))
## Create list of vectors, with each vector containing counts of species
sigmodela8 <- as.vector(sigmodela7$speciesCount)
## Count number of species
z <- as.numeric(length(sigmodela8))
## Create empty vector for p-values
pvaluesaClimateSegmentaClimateSegment <- array(c(0), dim = z)
pvaluesbClimateSegmentbClimateSegment <- array(c(0), dim = z)
pvaluescClimateSegmentcClimateSegment <- array(c(0), dim = z)
pvaluesdClimateSegmentdClimateSegment <- array(c(0), dim = z)
pvalueseClimateSegmenteClimateSegment <- array(c(0), dim = z)
pvaluesfClimateSegmentfClimateSegment <- array(c(0), dim = z)

## Conduct Exact Multinomial Test
## Conduct exact multinomial test and output p-values for each species
for(i in 1:z){
  pvaluesaClimateSegmentaClimateSegment[i] <- multinomial.test(sigmodela8[[i]], aClimateSegmentaClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluesbClimateSegmentbClimateSegment[i] <- multinomial.test(sigmodela8[[i]], bClimateSegmentbClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluescClimateSegmentcClimateSegment[i] <- multinomial.test(sigmodela8[[i]], cClimateSegmentcClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluesdClimateSegmentdClimateSegment[i] <- multinomial.test(sigmodela8[[i]], dClimateSegmentdClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvalueseClimateSegmenteClimateSegment[i] <- multinomial.test(sigmodela8[[i]], eClimateSegmenteClimateSegment$WEIGHTEDCHANCE)$p.value}

for(i in 1:z){
  pvaluesfClimateSegmentfClimateSegment[i] <- multinomial.test(sigmodela8[[i]], fClimateSegmentfClimateSegment$WEIGHTEDCHANCE)$p.value}

## Set vector of p-values as dataframe, insert species names, and correct p-values for using same dataframe
## Set vector of p-values as dataframe
pvaluesaClimateSegmentaClimateSegment2 <- as.data.frame(pvaluesaClimateSegmentaClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesaClimateSegmentaClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesaClimateSegmentaClimateSegment2$padj = p.adjust(pvaluesaClimateSegmentaClimateSegment2$pvaluesaClimateSegmentaClimateSegment, method="fdr")
## Add model name
pvaluesaClimateSegmentaClimateSegment2$model <- "a Climate Segment a Climate Segment"
## Rename original p-value column
colnames(pvaluesaClimateSegmentaClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesaClimateSegmentaClimateSegment2, "pvaluesaClimateSegmentaClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluesbClimateSegmentbClimateSegment2 <- as.data.frame(pvaluesbClimateSegmentbClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesbClimateSegmentbClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesbClimateSegmentbClimateSegment2$padj = p.adjust(pvaluesbClimateSegmentbClimateSegment2$pvaluesbClimateSegmentbClimateSegment, method="fdr")
## Add model name
pvaluesbClimateSegmentbClimateSegment2$model <- "b Climate Segment b Climate Segment"
## Rename original p-value column
colnames(pvaluesbClimateSegmentbClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesbClimateSegmentbClimateSegment2, "pvaluesbClimateSegmentbClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluescClimateSegmentcClimateSegment2 <- as.data.frame(pvaluescClimateSegmentcClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluescClimateSegmentcClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluescClimateSegmentcClimateSegment2$padj = p.adjust(pvaluescClimateSegmentcClimateSegment2$pvaluescClimateSegmentcClimateSegment, method="fdr")
## Add model name
pvaluescClimateSegmentcClimateSegment2$model <- "c Climate Segment c Climate Segment"
## Rename original p-value column
colnames(pvaluescClimateSegmentcClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluescClimateSegmentcClimateSegment2, "pvaluescClimateSegmentcClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluesdClimateSegmentdClimateSegment2 <- as.data.frame(pvaluesdClimateSegmentdClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesdClimateSegmentdClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesdClimateSegmentdClimateSegment2$padj = p.adjust(pvaluesdClimateSegmentdClimateSegment2$pvaluesdClimateSegmentdClimateSegment, method="fdr")
## Add model name
pvaluesdClimateSegmentdClimateSegment2$model <- "d Climate Segment d Climate Segment"
## Rename original p-value column
colnames(pvaluesdClimateSegmentdClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesdClimateSegmentdClimateSegment2, "pvaluesdClimateSegmentdClimateSegment.csv")

## Set vector of p-values as dataframe
pvalueseClimateSegmenteClimateSegment2 <- as.data.frame(pvalueseClimateSegmenteClimateSegment)
## Insert species names and pair with corresponding p-values
pvalueseClimateSegmenteClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvalueseClimateSegmenteClimateSegment2$padj = p.adjust(pvalueseClimateSegmenteClimateSegment2$pvalueseClimateSegmenteClimateSegment, method="fdr")
## Add model name
pvalueseClimateSegmenteClimateSegment2$model <- "e Climate Segment e Climate Segment"
## Rename original p-value column
colnames(pvalueseClimateSegmenteClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvalueseClimateSegmenteClimateSegment2, "pvalueseClimateSegmenteClimateSegment.csv")

## Set vector of p-values as dataframe
pvaluesfClimateSegmentfClimateSegment2 <- as.data.frame(pvaluesfClimateSegmentfClimateSegment)
## Insert species names and pair with corresponding p-values
pvaluesfClimateSegmentfClimateSegment2$speciesName <- sigmodela7$speciesName
## Correct p-values by false discovery rate
pvaluesfClimateSegmentfClimateSegment2$padj = p.adjust(pvaluesfClimateSegmentfClimateSegment2$pvaluesfClimateSegmentfClimateSegment, method="fdr")
## Add model name
pvaluesfClimateSegmentfClimateSegment2$model <- "f Climate Segment f Climate Segment"
## Rename original p-value column
colnames(pvaluesfClimateSegmentfClimateSegment2)[1] <- "pvalues"
## write to csv
write.csv(pvaluesfClimateSegmentfClimateSegment2, "pvaluesfClimateSegmentfClimateSegment.csv")
