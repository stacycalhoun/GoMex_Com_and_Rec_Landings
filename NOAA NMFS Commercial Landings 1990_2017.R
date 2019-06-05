#NOAA NMFS Commercial Fisheries Data
#Wrangling into the right shape to be used in GoMex model
#Stacy Calhoun; Created 4 June 2019

remove(list=ls())
assign("last.warning", NULL, envir = baseenv())
# Libraries ---------------------------------------------------------------

library(plyr)
library(dplyr)
library(tidyverse)
library(readr)
library(purrr)
library(data.table)

# Reading csvs into one dataframe ---------------------------------------------------------

RawData <-
  list.files(pattern="*.csv") %>% 
  map_df(~read_csv(.))


# Wrangling ---------------------------------------------------------------

#Getting rid of value column and freshwater drum
RawData$`$` = NULL

RawData = subset(RawData, RawData$Species != "DRUM, FRESHWATER")

#Adding FisheriesGrp Column
RawData$FisheriesGrp = NA
RawData$FisheriesGrp[RawData$Species %like% "AMBERJACK"| RawData$Species %like% "BARRACUDAS" | RawData$Species %like% "BONITO" |
                       RawData$Species %like% "COBIA" | RawData$Species %like% "JACK" | RawData$Species %like% "MACKEREL" | RawData$Species %like% "POMPANO"] = 'Lg_pelagics'
RawData$FisheriesGrp[RawData$Species %like% "MENHADEN"] = "Menhaden"
RawData$FisheriesGrp[RawData$Species %like% "SHARK"] = "Sharks"
RawData$FisheriesGrp[RawData$Species %like% "BUTTER"] = "Butterfish"
RawData$FisheriesGrp[RawData$Species %like% "HAKE" | RawData$Species %like% "TRIGGER" | RawData$Species %like% "SCAMP"] = "Lg_demersals"
