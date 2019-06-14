#NOAA NMFS Recreational Discards Data
#Organizing data for use in GoMex ECOTRAN Model & translating Access queries
#Stacy Calhoun; Created 14 June 2019


remove(list=ls())
assign("last.warning", NULL, envir = baseenv())
# Libraries ---------------------------------------------------------------

library(plyr)
library(tidyverse)
library(readr)
library(purrr)
library(data.table)
library(readxl)

# Reading MRIP csvs into one dataframe and reading in functional group list ---------------------------------------------------------

RawData <-
  list.files(pattern="mrip_catch_wave*") %>% 
  map_df(~read_csv(.))

FishList = read.csv("RecFisheriesGrp.csv", stringsAsFactors = F)

Indiv_Wt = read_xlsx("MRFSS mean indiv weight by species_kg.xlsx")


# Data Wrangling and Subsetting -------------------------------------------

MRIP = subset(RawData, RawData$status == 'FINAL' & RawData$sub_reg == 7 & RawData$st_f == "LOUISIANA" | RawData$st_f == "ALABAMA" | RawData$st_f == "MISSISSIPPI", 
              select = c("status", "year", "wave", "sub_reg", "st_f", "common", "area_x",
                         "landing", "land_var", "wgt_ab1", "var_wab1", "miss_fish", "estrel", "estrlvar"))

MRIP = subset(MRIP, MRIP$area_x == 2)

MRIP_Func = merge(MRIP, FishList, by = "common", all.x = T)

MRIP_Func = merge(MRIP_Func, Indiv_Wt, by.x = "common", by.y = "COMMON (MRFSS)", all.x = T)

Unwanted = anti_join(MRIP_Func, FishList, by = "common")

Unwanted_Species = unique(Unwanted$common)

RecDis = MRIP_Func[!MRIP_Func$common %in% Unwanted_Species, ]



# Access Query Chain Steps ------------------------------------------------

#Step 1: Filtering data, calculating average individual weights
#Filtering done in subsetting line above.

RecDis$Tot_Har_AB1_No = with(RecDis, landing + miss_fish)
RecDis$Avg_WGT_AB1_kg = with(RecDis, wgt_ab1/Tot_Har_AB1_No)
