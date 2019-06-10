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

MRIP = subset(RawData, RawData$status == 'FINAL' & RawData$sub_reg == 7 & RawData$st_f == "LOUISIANA" | RawData$st_f == "ALABAMA" | RawData$st_f == "MISSISSIPPI", 
              select = c("status", "year", "wave", "sub_reg", "st_f", "common", "area_x",
                                  "landing", "land_var", "wgt_ab1", "var_wab1", "miss_fish"))
MRIP = subset(MRIP, MRIP$area_x == 2)

MRIP_Func = merge(MRIP, FishList, by = "common", all.x = T)

Unwanted = anti_join(MRIP_Func, FishList, by = "common")

Unwanted_Species = unique(Unwanted$common)

MRIP_Func = MRIP_Func[!MRIP_Func$common %in% Unwanted_Species, ]


# Access Query Chain Steps ------------------------------------------------

#Step 1: filtering records was done above in the subsetting line. Converting total harvest from kg into tonnes.

Rec_Fish_Step1 = MRIP_Func

Rec_Fish_Step1$Harvest_tonnes = Rec_Fish_Step1$wgt_ab1*0.001

#Step 2: Defines areas of fishing areas and sum tonnes of functional group harvest per year.
# FishedArea = 263,186 km2 (area from LA-MS-AL 3 miles offshore to EEZ (200 miles offshore))


Rec_Fish_Step2 = ddply(Rec_Fish_Step1, .(year, FisheriesGrp), summarize,
                       Annual_Total_Harvest_tonnes = sum(Harvest_tonnes),
                       FishedArea = 263186
                       )

#Step 3: Estimate total harvest tonnes/km2 for each functional group

Rec_Fish_Step3 = ddply(Rec_Fish_Step2, .(year, FisheriesGrp), summarize,
                       Harvest_Tonnes_km2 = Annual_Total_Harvest_tonnes/FishedArea
                       )

