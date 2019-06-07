#NOAA MRIP Recreational Fisheries Estimate Data Wrangling and Access Query Rewrites
#Stacy Calhoun, Created 7 June 2019
#MRIP Estimates data accessed from https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads 
#page that links to https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/ 

remove(list=ls())
assign("last.warning", NULL, envir = baseenv())
# Libraries ---------------------------------------------------------------

library(plyr)
library(tidyverse)
library(readr)
library(purrr)
library(data.table)

# Reading MRIP csvs into one dataframe and reading in functional group list ---------------------------------------------------------

RawData <-
  list.files(pattern="mrip_catch_wave*") %>% 
  map_df(~read_csv(.))

FishList = read.csv("RecFisheriesGrp.csv", stringsAsFactors = F)


# Data Wrangling and Subsetting -------------------------------------------

MRIP = subset(RawData, RawData$st_f == "LOUISIANA" | RawData$st_f == "ALABAMA" | RawData$st_f == "MISSISSIPPI" & RawData$status == 'FINAL', 
              select = c("status", "year", "wave", "sub_reg", "st_f", "common", "area_x",
                                  "landing", "land_var", "wgt_ab1", "var_wab1", "miss_fish"))

MRIP_Func = merge(MRIP, FishList, by = "common", all.x = T)

Unwanted = anti_join(MRIP_Func, FishList, by = "common")

Unwanted_Species = unique(Unwanted$common)

MRIP_Func = MRIP_Func[!MRIP_Func$common %in% Unwanted_Species, ]



