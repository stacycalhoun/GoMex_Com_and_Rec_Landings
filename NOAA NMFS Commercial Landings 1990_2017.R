#NOAA NMFS Commercial Fisheries Data
#Wrangling into the right shape to be used in GoMex model
#Stacy Calhoun; Created 4 June 2019

remove(list=ls())
assign("last.warning", NULL, envir = baseenv())
# Libraries ---------------------------------------------------------------

library(plyr)
#library(dplyr)
library(tidyverse)
library(readr)
library(purrr)
library(data.table)

# Reading csvs into one dataframe ---------------------------------------------------------

RawData <-
  list.files(pattern="*_LANDINGS.csv") %>% 
  map_df(~read_csv(.))

FishList = read.csv("FisheriesGrpList.csv", stringsAsFactors = F) #Contains FisheriesGrp column to be added later
# Wrangling ---------------------------------------------------------------

#Getting rid of value column and freshwater drum
RawData$`$` = NULL


RawData = subset(RawData, RawData$Species != "DRUM, FRESHWATER")


#Adding FisheriesGrp Column, changing characters to factors and renaming "Metric Tons" column for ease of use

ComFish = merge(RawData, FishList, by = "Species", all.x = T)


ComFish$Month = as.factor(ComFish$Month)

ComFish$FisheriesGrp = as.factor(ComFish$FisheriesGrp)

ComFish$State = as.factor(ComFish$State)

colnames(ComFish)[5] = "Metric_Tons"

#Removed unwanted species from dataframe based on mismatches between FishList and ComFish

Unwanted = anti_join(ComFish, FishList, by = "Species")

Unwanted_Species = unique(Unwanted$Species)

ComFish = ComFish[!ComFish$Species %in% Unwanted_Species, ]

# Query Chain Steps -------------------------------------------------------

#Step 1: LA-MS-AL commercial landings--setting filters for model domain

Com_Fish_Step1 = subset(ComFish, ComFish$State != "Texas" & ComFish$State !="Florida West Coast")

Com_Fish_Step1 = ddply(Com_Fish_Step1, .(FisheriesGrp, Year, Month, State), summarize,
                       SumOfMetric_Tons = sum(Metric_Tons)
                       )

#Steps 2 and 3: LA-MS-AL commercial landings--Step 2: estimate total, annual landings for each functional group; 
#Step 3: LA-MS-AL commercial landings--estimate annual tonnes/km2 for each functional group

Com_Fish_Step2and3 = ddply(Com_Fish_Step1, .(Year, FisheriesGrp), summarize,
                           domain = 42130, #This is from the shelf model just to run the code--Need to change!!!
                       AnnualSum_metric_tons = sum(SumOfMetric_Tons), 
                       Tonnes_km2 = AnnualSum_metric_tons/domain
                       )

#Step 3a: LA-MS-AL commercial landings--This steps incorporates inshore/offshore data from a different table. 
#Space holder to add-in in the future


#Step 4: LA-MS-AL commercial landings-- estimating long-term average, total annual landings for each functional group


Com_Fish_Step4 = ddply(Com_Fish_Step2and3, .(FisheriesGrp), summarize,
                       CountTonnes_km2 = length(Tonnes_km2),
                       AvgTonnes_km2 = mean(Tonnes_km2),
                       VarTonnes_km2 = var(Tonnes_km2),
                       StDevTonnes_km2 = sqrt(VarTonnes_km2)
                       )

