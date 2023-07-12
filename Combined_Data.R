#### IMPORT PACKAGES AND CREATE THEMES ####
library(readr)
# Tables/analysis
library(data.table)     # Fast data analysis
library(dplyr)          # Cleaning up data
library(lubridate)      # For dates
library(broom) 
# Plots
library(ggplot2)        # For plots
library(scales)         # Plot specifics
library(maps)           # Adding maps
library(patchwork)      # Combining plots
library(ggpubr) 

#For confidence intervals in plots
library(magrittr)
install.packages("gcookbook")
library(gcookbook)

#### DATA IMPORT ####
# data import 
#MADMF_Inshore_Age0 <- read_csv("OneDrive - Bowdoin College/GMRI REU/Data/R/MADMF_Inshore_Age0.csv")
#NEFSC_WGOM_Age1_SSB_fall <- read_csv("OneDrive - Bowdoin College/GMRI REU/Data/R/NEFSC_WGOM_Age1_SSB_fall.csv")
NEFSC_WGOM_Age1_SSB_spring <- read_csv("OneDrive - Bowdoin College/GMRI REU/Data/R/NEFSC_WGOM_Age1_SSB_spring.csv")
PrelimEst_SSB_R_WGOM <- read_csv("OneDrive - Bowdoin College/GMRI REU/Data/R/PrelimEstimates_SSB_R_byWGOMpop[4].csv")

# excel data import
library(readxl)
closure_timeline <- read_excel("OneDrive - Bowdoin College/GMRI REU/Data/R/closure timeline.xlsx")
closure_timeline <- closure_timeline %>% 
  rename("Year" = "Date Put in Effect",
         "Name" = "Closure Name")

#### DATA TRANSFORM ####

## MADMF data transform
# filter out spring_fall
#MADMF_Filtered_data <- filter(MADMF_Inshore_Age0, SEASON != "SPRING_FALL")
# label spawning group
#MADMF_Filtered_data <- MADMF_Filtered_data %>% 
#  mutate(Spawning_Group = if_else(SEASON == "FALL", "SPRING SPAWNING", "WINTER SPAWNING"))
# slect for needed columns
#MADMF_Filtered_data <- select(MADMF_Filtered_data, INDEX_NAME, SEASON, YEAR, IDX, Spawning_Group)
#MADMF_Filtered_data <- MADMF_Filtered_data %>% rename("Year" = "YEAR")

## break into 2 data tables (might not need)
#MADMF_Filtered_data_SPRING <- filter(MADMF_Filtered_data, Spawning_Group == "SPRING SPAWNING")
#MADMF_Filtered_data_WINTER <- filter(MADMF_Filtered_data, Spawning_Group == "WINTER SPAWNING")

## prelim_estimates transform R and SSB for WGOM populations
# rename columns
PrelimEst_SSB_R_WGOM <- PrelimEst_SSB_R_WGOM %>% rename("Year" = "year")

## timeline transform
# group closures into spring and winter spawning groups
closure_timeline <- closure_timeline %>% 
  mutate(Spawning_Group_Affected = if_else(Name == c("Spring Cod Conservation Zone","GOM Cod Spawning Protection Area","Spring Massachusetts Bay SPA") | Year == "2009", "SPRING SPAWNING", "WINTER SPAWNING"))
# create a duplicate of rolliing closures since they affect both spring and winter spawners
closure_timeline[nrow(closure_timeline) + 1,] <- list("GOM Cod Protection Areas (rolling closures)", 1998, NA, "SPRING SPAWNING")
closure_timeline[nrow(closure_timeline) + 1,] <- list("GOM Cod Protection Areas (rolling closures)", 1999, NA, "SPRING SPAWNING")
# slect for needed columns
closure_timeline <- select(closure_timeline, Name, Year, Spawning_Group_Affected) %>% rename("Closure_Name" = "Name")

## break into 2 data tables (maybe not doing this)
#closure_timeline_SPRING <- filter(closure_timeline, Spawning_Group == "SPRING SPAWNING")
#closure_timeline_WINTER <- filter(closure_timeline, Spawning_Group == "WINTER SPAWNING")

## NEFSC environemntal data transform
environmental_data <- select(NEFSC_WGOM_Age1_SSB_spring, Year, Heatwave, GSI)

#### DATA MERGE ####

FINAL_TABLE <- merge(PrelimEst_SSB_R_WGOM, environmental_data, by = "Year", all.x = TRUE) %>%
  merge(closure_timeline, by = "Year", all.x = TRUE)

#### CREATE TIMEBLOCKS ####

# create a timeblock dataframe
time_blocks <- data.frame(Year = 1981:2021,
                 Closures_SPRING = NA,
                 Closures_WINTER = NA,
                 Quota_MGT = NA)

# time_blocks if we decide to do 3 columns of management regimes
#time_blocks$Rolling_Closure[time_blocks$Year %in% 1981:1997] <- "No_Closure"
#time_blocks$Rolling_Closure[time_blocks$Year %in% 1998:2021] <- "Rolling_Closure"
#time_blocks$Winter_Spawning_Closure[time_blocks$Year %in% 1981:2002] <- "No_Spawning_Closure"
#time_blocks$Winter_Spawning_Closure[time_blocks$Year %in% 2003:2021] <- "Winter_Spawning_Closure"
#time_blocks$Spring_Spawning_Closure[time_blocks$Year %in% 1981:2008] <- "No_Spawning_Closure"
#time_blocks$Spring_Spawning_Closure[time_blocks$Year %in% 2009:2021] <- "Spring_Spawning_Closure"
#time_blocks$Quota_MGT[time_blocks$Year %in% 1981:2009] <- "Indirect_Controls"
#time_blocks$Quota_MGT[time_blocks$Year %in% 2010:2021] <- "Quotas"

# time_blocks if we decide to do 2 columns of management regimes (large df will 3 : 1 column for both, 1 for W, 1 for S)
time_blocks$Closures_SPRING[time_blocks$Year %in% 1981:1997] <- "No_Closure"
time_blocks$Closures_SPRING[time_blocks$Year %in% 1998:2008] <- "Rolling_Closure"
time_blocks$Closures_SPRING[time_blocks$Year %in% 2009:2021] <- "Spring_Spawning_Closure"

time_blocks$Closures_WINTER[time_blocks$Year %in% 1981:1997] <- "No_Closure"
time_blocks$Closures_WINTER[time_blocks$Year %in% 1998:2002] <- "Rolling_Closure"
time_blocks$Closures_WINTER[time_blocks$Year %in% 2003:2021] <- "Spring_Spawning_Closure"

time_blocks$Quota_MGT[time_blocks$Year %in% 1981:2009] <- "Indirect_Controls"
time_blocks$Quota_MGT[time_blocks$Year %in% 2010:2021] <- "Quotas"


## time_blocks if we decide to do more time blocks under one variable (MGT)
#time_blocks$MGT[time_blocks$Year %in% 1981:1997] <- "No_Closure"
#time_blocks$MGT[time_blocks$Year %in% 1998:2002] <- "Rolling_Closure"
#time_blocks$MGT[time_blocks$Year %in% 2003:2008] <- "WCCZ"
#time_blocks$MGT[time_blocks$Year %in% 2009:2011] <- "SCCZ"
#time_blocks$MGT[time_blocks$Year %in% 2012:2017] <- "GOM_cod_SPA"
#time_blocks$MGT[time_blocks$Year %in% 2018:2021] <- "Winter_and_Spring_MA_Bay_SPA"

#### FINAL DATA MERGE ####

FINAL_TABLE <- merge(FINAL_TABLE, time_blocks, by = "Year")

