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

#### Make a theme for plotting ####

# Make a plotting theme
REU_theme <- theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0))

#### Downloading metadata/data ####

# data import 
MADMF_Inshore_Age0 <- read_csv("OneDrive - Bowdoin College/GMRI REU/Data/R/MADMF_Inshore_Age0.csv")
NEFSC_WGOM_Age1_SSB_fall <- read_csv("OneDrive - Bowdoin College/GMRI REU/Data/R/NEFSC_WGOM_Age1_SSB_fall.csv")
NEFSC_WGOM_Age1_SSB_spring <- read_csv("OneDrive - Bowdoin College/GMRI REU/Data/R/NEFSC_WGOM_Age1_SSB_spring.csv")

# excel data import
library(readxl)
closure_timeline <- read_excel("OneDrive - Bowdoin College/GMRI REU/Data/R/closure timeline.xlsx")
closure_timeline <- closure_timeline %>% 
  rename("Year" = "Date Put in Effect",
         "Name" = "Closure Name")


#### Preliminary Recruitment and SSB Plotting ####

NEFSC_WGOM_Age1_SSB_fall_Age1 <- ggplot() + 
  geom_line(data = NEFSC_WGOM_Age1_SSB_fall, aes (x = Year, y = Age.1)) +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Name, color = Name)) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'Index of Abundance',
    title =  'NEFSC Fall Survey Age-1 Recruitment',
    color = 'Measurement'
  ) 

NEFSC_WGOM_Age1_SSB_fall_SSB1 <- ggplot() + 
  geom_line(data = NEFSC_WGOM_Age1_SSB_fall, aes (x = Year, y = SSB)) +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Name, color = Name)) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'kg/tow',
    title =  'NEFSC Fall Survey SSB',
    color = 'Measurement'
  ) 

NEFSC_WGOM_Age1_SSB_spring_Age1 <- ggplot() + 
  geom_line(data = NEFSC_WGOM_Age1_SSB_spring, aes (x = Year, y = Age.1)) +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Name, color = Name)) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'Index of Abundance',
    title =  'NEFSC Spring Survey Age-1 Recruitment',
    color = 'Measurement'
  ) 

NEFSC_WGOM_Age1_SSB_spring_SSB1 <- ggplot() + 
  geom_line(data = NEFSC_WGOM_Age1_SSB_spring, aes (x = Year, y = SSB)) +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Name, color = Name)) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'kg/tow',
    title =  'NEFSC Spring Survey SSB',
    color = 'Measurement'
  ) 

# MADMF spawning labeling

MADMF_Filtered_data <- filter(MADMF_Inshore_Age0, SEASON != "SPRING_FALL")
MADMF_Filtered_data <- MADMF_Filtered_data %>% 
  mutate(Spawning_Group = if_else(SEASON == "FALL", "SPRING SPAWNING", "WINTER SPAWNING"))

# winter and spring closure separate timelines

closure_timeline <- closure_timeline %>% 
  mutate(Spawning_Group = if_else(Name == c("Spring Cod Conservation Zone","GOM Cod Spawning Protection Area","Spring Massachusetts Bay SPA") | Year == "2009", "SPRING SPAWNING", "WINTER SPAWNING"))

closure_timeline[nrow(closure_timeline) + 1,] <- list("GOM Cod Protection Areas (rolling closures)", 1998, NA, "SPRING SPAWNING")
closure_timeline[nrow(closure_timeline) + 1,] <- list("GOM Cod Protection Areas (rolling closures)", 1999, NA, "SPRING SPAWNING")

MADMF_Inshore_Age0_Age0plot <- ggplot(data = MADMF_Filtered_data, aes (x = YEAR, y = IDX, group = Spawning_Group)) + 
  geom_ribbon(aes(ymin=IDX-SE, ymax=IDX+SE),
              alpha=0.2) +
  geom_line() +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Spawning_Group, color = Name)) +
  facet_wrap(.~Spawning_Group, ncol = 1, nrow = 2) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'Index of Abundance',
    title =  'MADMF Inshore Survey Age-0 Recruitment'
  ) 

#### external factors preliminary graphing ####

# BT anomaly (different for fall and spring)

NEFSC_WGOM_Age1_SSB_fall_bt_anomaly <- ggplot() + 
  geom_line(data = NEFSC_WGOM_Age1_SSB_fall, aes (x = Year, y = bt_anomaly)) +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Name, color = Name)) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'kg/tow',
    title =  'NEFSC Fall Survey BT Anomaly',
    color = 'Measurement'
  ) 

NEFSC_WGOM_Age1_SSB_spring_bt_anomaly <- ggplot() + 
  geom_line(data = NEFSC_WGOM_Age1_SSB_spring, aes (x = Year, y = bt_anomaly)) +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Name, color = Name)) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'Index of Abundance',
    title =  'NEFSC Spring Survey BT Anomaly',
    color = 'Measurement'
  ) 

# SST Anomaly (same for both fall and spring surveys)

NEFSC_WGOM_Age1_SSB_spring_fall_sst_anomaly <- ggplot() + 
  geom_line(data = NEFSC_WGOM_Age1_SSB_spring, aes (x = Year, y = sst_anomaly)) +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Name, color = Name)) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'Index of Abundance',
    title =  'NEFSC Spring and Fall Survey SST Anomaly',
    color = 'Measurement'
  ) 

# Heatwaves (same for both fall and spring surveys)

NEFSC_WGOM_Age1_SSB_spring_fall_heatwaves <- ggplot() + 
  geom_line(data = NEFSC_WGOM_Age1_SSB_spring, aes (x = Year, y = Heatwave)) +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Name, color = Name)) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'Number of Haetwaves',
    title =  'NEFSC Spring and Fall Survey Heatwaves',
    color = 'Measurement'
  ) 

# GSI (Gulf stream index) (same for fall and spring survey)
# gulf stream index is a position anomaly meaning the larger the value of the index the further north the northern wall of the Gulf Stream is for that year.

NEFSC_WGOM_Age1_SSB_spring_fall_GSI <- ggplot() + 
  geom_line(data = NEFSC_WGOM_Age1_SSB_spring, aes (x = Year, y = GSI)) +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Name, color = Name)) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'GSI',
    title =  'NEFSC Spring and Fall Survey GSI',
    color = 'Measurement'
  ) 

#### prey species (not rel) ####
# calfin_100m3 (fall and spring survey are he same)

NEFSC_WGOM_Age1_SSB_fall_spring_calfin_100m3 <- ggplot() + 
  geom_line(data = NEFSC_WGOM_Age1_SSB_fall, aes (x = Year, y = calfin_100m3)) +
  geom_line(data = NEFSC_WGOM_Age1_SSB_fall, aes (x = Year, y = pseudo_100m3)) +
  geom_vline(data = closure_timeline, aes(xintercept = Year, group = Name, color = Name)) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    x = 'Year',
    y = 'Calanus finmarchicus abundance anomaly (/100m3)',
    title =  'NEFSC Fall and Spring Survey calanus finmarchi anomaly',
    color = 'Measurement'
  ) 

#### environmental covariates plotted against recruitment ####

# BT anomaly

NEFSC_WGOM_Age1_SSB_fall_age1_BT <- ggplot(data = NEFSC_WGOM_Age1_SSB_fall, aes (y = Age.1, x = bt_anomaly)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'Index of Abundance',
    x = 'BT anomaly',
    title =  'Fall survey',
    color = 'Measurement'
  ) 

NEFSC_WGOM_Age1_SSB_spring_age1_BT <- ggplot(data = NEFSC_WGOM_Age1_SSB_spring, aes (y = Age.1, x = bt_anomaly)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'Index of Abundance',
    x = 'BT anomaly',
    title =  'Spring survey',
    color = 'Measurement'
  ) 

# SST anomaly

NEFSC_WGOM_Age1_SSB_fall_age1_SST <- ggplot(data = NEFSC_WGOM_Age1_SSB_fall, aes (y = Age.1, x = sst_anomaly)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'Index of Abundance',
    x = 'SST anomaly',
    title =  'Fall survey',
    color = 'Measurement'
  ) 

NEFSC_WGOM_Age1_SSB_spring_age1_SST <- ggplot(data = NEFSC_WGOM_Age1_SSB_spring, aes (y = Age.1, x = sst_anomaly)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'Index of Abundance',
    x = 'SST anomaly',
    title =  'Spring survey',
    color = 'Measurement'
  ) 

# Heatwaves

NEFSC_WGOM_Age1_SSB_fall_age1_heatwaves <- ggplot(data = NEFSC_WGOM_Age1_SSB_fall, aes (y = Age.1, x = Heatwave)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'Index of Abundance',
    x = 'Number of Heatwaves',
#    title =  'Spring and fall survey Age-1 recruitment compared to Heatwaves',
    color = 'Measurement'
  ) 

# GSI

NEFSC_WGOM_Age1_SSB_spring_age1_GSI <- ggplot(data = NEFSC_WGOM_Age1_SSB_fall, aes (y = Age.1, x = GSI)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'Index of Abundance',
    x = 'GSI',
#    title =  'Spring and fall survey Age-1 recruitment compared to Gulf Stream Index',
    color = 'Measurement'
  ) 

ggarrange(NEFSC_WGOM_Age1_SSB_fall_age1_BT, NEFSC_WGOM_Age1_SSB_spring_age1_BT, NEFSC_WGOM_Age1_SSB_fall_age1_SST, 
          NEFSC_WGOM_Age1_SSB_spring_age1_SST, NEFSC_WGOM_Age1_SSB_fall_age1_heatwaves, NEFSC_WGOM_Age1_SSB_spring_age1_GSI,
          ncol = 3, nrow = 2)

#### environmental covariates plotted against SSB ####

# BT anomaly

NEFSC_WGOM_Age1_SSB_fall_SSB_BT <- ggplot(data = NEFSC_WGOM_Age1_SSB_fall, aes (y = SSB, x = bt_anomaly)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'SSB',
    x = 'BT anomaly',
    title =  'Fall survey',
    color = 'Measurement'
  ) 

NEFSC_WGOM_Age1_SSB_spring_SSB_BT <- ggplot(data = NEFSC_WGOM_Age1_SSB_spring, aes (y = SSB, x = bt_anomaly)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'SSB',
    x = 'BT anomaly',
    title =  'Spring survey',
    color = 'Measurement'
  ) 

# SST anomaly

NEFSC_WGOM_Age1_SSB_fall_SSB_SST <- ggplot(data = NEFSC_WGOM_Age1_SSB_fall, aes (y = SSB, x = sst_anomaly)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'SSB',
    x = 'SST anomaly',
    title =  'Fall survey',
    color = 'Measurement'
  ) 

NEFSC_WGOM_Age1_SSB_spring_SSB_SST <- ggplot(data = NEFSC_WGOM_Age1_SSB_spring, aes (y = SSB, x = sst_anomaly)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'SSB',
    x = 'SST anomaly',
    title =  'Spring survey',
    color = 'Measurement'
  ) 

# Heatwaves

NEFSC_WGOM_Age1_SSB_fall_SSB_heatwaves <- ggplot(data = NEFSC_WGOM_Age1_SSB_fall, aes (y = SSB, x = Heatwave)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'SSB',
    x = 'Number of Heatwaves',
#    title =  'Spring and fall survey SSB compared to Heatwaves',
    color = 'Measurement'
  ) 

# GSI

NEFSC_WGOM_Age1_SSB_spring_SSB_GSI <- ggplot(data = NEFSC_WGOM_Age1_SSB_fall, aes (y = SSB, x = GSI)) + 
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs")) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') +
  labs(
    y = 'SSB',
    x = 'GSI',
#    title =  'Spring and fall survey SSB compared to Gulf Stream Index',
    color = 'Measurement'
  ) 

ggarrange(NEFSC_WGOM_Age1_SSB_fall_SSB_BT, NEFSC_WGOM_Age1_SSB_spring_SSB_BT, NEFSC_WGOM_Age1_SSB_fall_SSB_SST, 
          NEFSC_WGOM_Age1_SSB_spring_SSB_SST, NEFSC_WGOM_Age1_SSB_fall_SSB_heatwaves, NEFSC_WGOM_Age1_SSB_spring_SSB_GSI,
          ncol = 3, nrow = 2)
