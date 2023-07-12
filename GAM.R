#### IMPORT PACKAGES ####
# GAM stuff
library(mgcv)
library(visreg)
library(ggeffects)
library(gratia)
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
library(gcookbook)
# for data exploration
library(GGally)
#### DATA TABLE TRANSFORMS ####
## split FINAL_TABLE into winter and spring groups
FINAL_TABLE_SPRING <- filter(FINAL_TABLE, population == "spring")
FINAL_TABLE_WINTER <- filter(FINAL_TABLE, population == "winter")

#### WINTER GAM ####

## SSB
# compare each variable against the other
# correlation coefficient of +/- 1 indicates linear relationship. (+) = direct, (-) = indirect.
ggpairs(FINAL_TABLE_WINTER %>% select(c(ssb.apx, r, rps.apx, bt_anomaly, sst_anomaly, Heatwave, GSI))) +
  labs(subtitle = "Numeric variance exploration") 
ggpairs(FINAL_TABLE_WINTER %>% select(c(ssb.st, r, rps.st, bt_anomaly, sst_anomaly, Heatwave, GSI))) +
  labs(subtitle = "Numeric variance exploration") 

# sbb GAM
WINTER_GAM_ssb <- gam(ssb.apx ~ factor(Quota_MGT) + factor(Closures_WINTER) +
                       s(bt_anomaly, k = 4) + s(GSI, k = 4) + Heatwave, select = TRUE,
                       data = FINAL_TABLE_WINTER, method = "REML")
summary(WINTER_GAM_ssb)

# created by Gavin L. Simpson in {gratia}
# Plots estimated smooths from a fitted GAM model in a similar way to mgcv::plot.gam() but instead of using base graphics, ggplot2::ggplot() is used instead.
draw(WINTER_GAM_ssb, parametric = TRUE, scales = "fixed")

par(mfrow = c(2, 2))
gam.check(WINTER_GAM_ssb)

WINTER_ssb_pred <- data.frame(
  Year = FINAL_TABLE_WINTER$Year,
  ssb.apx = FINAL_TABLE_WINTER$ssb.apx,
  predicted_values = predict(WINTER_GAM_ssb, newdata = FINAL_TABLE_WINTER, type = "response")
)

# closures
W_ssb_closures_plot <- ggplot() +
  geom_rect(aes(xmin = 1981, xmax = 1998, ymin = -Inf, ymax = Inf, fill = 'No Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 1998, xmax = 2003, ymin = -Inf, ymax = Inf, fill = 'Rolling Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2003, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Winter Spawning Closures'),
            alpha = 1/5) +
  geom_line(data = FINAL_TABLE_WINTER, aes(x = Year, y = ssb.apx), size = 1, alpha = 0.5) +
  geom_line(data = WINTER_ssb_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1)

#quota MGT
W_ssb_quota_plot <- ggplot() +
  geom_line(data = FINAL_TABLE_WINTER, aes(x = Year, y = ssb.apx), size = 1, alpha = 0.5) +
  geom_line(data = WINTER_ssb_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1) +
  geom_rect(aes(xmin = 1981, xmax = 2010, ymin = -Inf, ymax = Inf, fill = 'Indirect Controls'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2010, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Quotas'),
            alpha = 1/5)

ggarrange(W_ssb_closures_plot, W_ssb_quota_plot, ncol = 1, nrow = 2)


## RperS GAM
# maybe consider including ssb into model
WINTER_GAM_RperS <- gam(rps.apx ~ factor(Closures_WINTER) + factor(Quota_MGT) 
                        + s(GSI, k = 4) + s(bt_anomaly, k = 4) + s(ssb.apx, k = 4),
                         select = TRUE, data = FINAL_TABLE_WINTER, method = "REML")
summary(WINTER_GAM_RperS)

draw(WINTER_GAM_RperS, parametric = TRUE, scales = "fixed")

par(mfrow = c(2, 2))
gam.check(WINTER_GAM_RperS)

WINTER_rps_pred <- data.frame(
  Year = FINAL_TABLE_WINTER$Year,
  rps.apx = FINAL_TABLE_WINTER$rps.apx,
  predicted_values = predict(WINTER_GAM_RperS, newdata = FINAL_TABLE_WINTER, type = "response")
)

# closures
W_rps_closures_plot <- ggplot() +
  geom_rect(aes(xmin = 1981, xmax = 1998, ymin = -Inf, ymax = Inf, fill = 'No Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 1998, xmax = 2003, ymin = -Inf, ymax = Inf, fill = 'Rolling Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2003, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Winter Spawning Closures'),
            alpha = 1/5) +
  geom_line(data = FINAL_TABLE_WINTER, aes(x = Year, y = rps.apx), size = 1, alpha = 0.5) +
  geom_line(data = WINTER_rps_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1)
  
#quota MGT
W_rps_quota_plot <- ggplot() +
  geom_line(data = FINAL_TABLE_WINTER, aes(x = Year, y = rps.apx), size = 1, alpha = 0.5) +
  geom_line(data = WINTER_rps_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1) +
  geom_rect(aes(xmin = 1981, xmax = 2010, ymin = -Inf, ymax = Inf, fill = 'Indirect Controls'),
          alpha = 1/5) +
  geom_rect(aes(xmin = 2010, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Quotas'),
          alpha = 1/5)

ggarrange(W_rps_closures_plot, W_rps_quota_plot, ncol = 1, nrow = 2)

## R GAM
WINTER_GAM_r <- gam(r ~ factor(Closures_WINTER) + factor(Quota_MGT) 
                        + s(GSI, k = 4) + s(ssb.apx, k = 4), select = TRUE,
                        data = FINAL_TABLE_WINTER, method = "REML")
summary(WINTER_GAM_r)

draw(WINTER_GAM_r, parametric = TRUE, scales = "fixed")
vis.gam(WINTER_GAM_r, type = "response", theta = 210)

par(mfrow = c(2, 2))
gam.check(WINTER_GAM_r)

WINTER_r_pred <- data.frame(
  Year = FINAL_TABLE_WINTER$Year,
  r = FINAL_TABLE_WINTER$r,
  predicted_values = predict(WINTER_GAM_r, newdata = FINAL_TABLE_WINTER, type = "response")
)

# closures
W_r_closures_plot <- ggplot() +
  geom_rect(aes(xmin = 1981, xmax = 1998, ymin = -Inf, ymax = Inf, fill = 'No Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 1998, xmax = 2003, ymin = -Inf, ymax = Inf, fill = 'Rolling Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2003, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Winter Spawning Closures'),
            alpha = 1/5) +
  geom_line(data = FINAL_TABLE_WINTER, aes(x = Year, y = r), size = 1, alpha = 0.5) +
  geom_line(data = WINTER_r_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1)

#quota MGT
W_r_quota_plot <- ggplot() +
  geom_line(data = FINAL_TABLE_WINTER, aes(x = Year, y = r), size = 1, alpha = 0.5) +
  geom_line(data = WINTER_r_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1) +
  geom_rect(aes(xmin = 1981, xmax = 2010, ymin = -Inf, ymax = Inf, fill = 'Indirect Controls'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2010, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Quotas'),
            alpha = 1/5)

ggarrange(W_r_closures_plot, W_r_quota_plot, ncol = 1, nrow = 2)

#### run against real data (example format) ####
winter_RperS_output_bt_anomaly <-data.frame( bt_anomaly = seq(-1.5, 2.5, length.out = 100),
                                  Heatwave = median(FINAL_TABLE_WINTER$Heatwave, na.rm = T),
                                  GSI = median(FINAL_TABLE_WINTER$GSI, na.rm = T),
                                  MGT = levels(FINAL_TABLE_WINTER$MGT))

pred_winter_RperS_output_bt_anomaly <- predict(WINTER_GAM_RperS, winter_RperS_output_bt_anomaly, type = "response")

winter_RperS_output_bt_anomaly$prediction <- pred_winter_RperS_output_bt_anomaly

winter_RperS_model_bt_anomaly_plot <- ggplot() +
  geom_line(data = FINAL_TABLE_WINTER, aes (x = bt_anomaly, y = RperS, group = MGT, color = "Real")) +
  geom_line(data = winter_RperS_output_bt_anomaly, aes(x = bt_anomaly, y = prediction, group = MGT, color = "Model")) +
  facet_wrap(.~MGT) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') 

# run against real data (Heatwave)
winter_RperS_output_Heatwave <-data.frame( Heatwave = seq(-1.0, 2.5, length.out = 100),
                                             bt_anomaly = median(FINAL_TABLE_WINTER$bt_anomaly, na.rm = T),
                                             GSI = median(FINAL_TABLE_WINTER$GSI, na.rm = T),
                                             MGT = levels(FINAL_TABLE_WINTER$MGT))

pred_winter_RperS_output_Heatwave <- predict(WINTER_GAM_RperS, winter_RperS_output_Heatwave, type = "response")

winter_RperS_output_Heatwave$prediction <- pred_winter_RperS_output_Heatwave

winter_RperS_model_Heatwave_plot <- ggplot() +
  geom_line(data = FINAL_TABLE_WINTER, aes (x = Heatwave, y = RperS, group = MGT, color = "Real")) +
  geom_line(data = winter_RperS_output_Heatwave, aes(x = Heatwave, y = prediction, group = MGT, color = "Model")) +
  facet_wrap(.~MGT) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') 

# run against real data (GSI)
winter_RperS_output_GSI <-data.frame( GSI = seq(-1.0, 2.0, length.out = 100),
                                           bt_anomaly = median(FINAL_TABLE_WINTER$bt_anomaly, na.rm = T),
                                           Heatwave = median(FINAL_TABLE_WINTER$Heatwave, na.rm = T),
                                           MGT = levels(FINAL_TABLE_WINTER$MGT))

pred_winter_RperS_output_GSI <- predict(WINTER_GAM_RperS, winter_RperS_output_GSI, type = "response")

winter_RperS_output_GSI$prediction <- pred_winter_RperS_output_GSI

winter_RperS_model_GSI_plot <- ggplot() +
  geom_line(data = FINAL_TABLE_WINTER, aes (x = GSI, y = RperS, group = MGT, color = "Real")) +
  geom_line(data = winter_RperS_output_GSI, aes(x = GSI, y = prediction, group = MGT, color = "Model")) +
  facet_wrap(.~MGT) +
  theme_bw() + # simple display theme (white background, light grey grid)
  theme(legend.position = 'bottom') 

#### SPRING GAM ####

# compare each variable against the other
# correlation coefficienty of +/- 1 indicates linear relationship. (+) = direct, (-) = indirect.
ggpairs(FINAL_TABLE_SPRING %>% select(c(ssb.apx, r, rps.apx, bt_anomaly, sst_anomaly, Heatwave, GSI))) +
  labs(subtitle = "Numeric variance exploration") 
ggpairs(FINAL_TABLE_SPRING %>% select(c(ssb.st, r, rps.st, bt_anomaly, sst_anomaly, Heatwave, GSI))) +
  labs(subtitle = "Numeric variance exploration") 

## SSB
# ssb GAM
SPRING_GAM_ssb <- gam(ssb.apx ~ factor(Closures_SPRING) + factor(Quota_MGT)
                      + s(bt_anomaly, k = 4) + Heatwave,
                      select = TRUE, data = FINAL_TABLE_SPRING, method = "REML", family=Tweedie(1.25,power(.1)))
summary(SPRING_GAM_ssb)

# draws partial effects of each explanatory variable (smooths and factors) on the response variable
# 0 is the mean response (in this case, mean ssb). above 0 means it has an increasing effect, below zero means a decreasing effect
draw(SPRING_GAM_ssb, parametric = TRUE, scales = "fixed")

par(mfrow = c(2, 2))
gam.check(SPRING_GAM_ssb)

SPRING_ssb_pred <- data.frame(
  Year = FINAL_TABLE_SPRING$Year,
  ssb.apx = FINAL_TABLE_SPRING$ssb.apx,
  predicted_values = predict(SPRING_GAM_ssb, newdata = FINAL_TABLE_SPRING, type = "response")
)

# closures
S_ssb_closures_plot <- ggplot() +
  geom_rect(aes(xmin = 1981, xmax = 1998, ymin = -Inf, ymax = Inf, fill = 'No Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 1998, xmax = 2009, ymin = -Inf, ymax = Inf, fill = 'Rolling Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2009, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Spring Spawning Closures'),
            alpha = 1/5) +
  geom_line(data = FINAL_TABLE_SPRING, aes(x = Year, y = ssb.apx), size = 1, alpha = 0.5) +
  geom_line(data = SPRING_ssb_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1)

#quota MGT
S_ssb_quota_plot <- ggplot() +
  geom_line(data = FINAL_TABLE_SPRING, aes(x = Year, y = ssb.apx), size = 1, alpha = 0.5) +
  geom_line(data = SPRING_ssb_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1) +
  geom_rect(aes(xmin = 1981, xmax = 2010, ymin = -Inf, ymax = Inf, fill = 'Indirect Controls'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2010, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Quotas'),
            alpha = 1/5)

ggarrange(S_ssb_closures_plot, S_ssb_quota_plot, ncol = 1, nrow = 2)


## RperS GAM

SPRING_GAM_RperS <- gam(rps.apx ~ factor(Closures_SPRING) + factor(Quota_MGT) +
                        s(sst_anomaly, k = 4) + s(ssb.apx, k = 4) + s(Heatwave, k = 4) + s(GSI, k = 4), 
                        select = TRUE, data = FINAL_TABLE_SPRING, method = "REML", family=Tweedie(1.25,power(.1)))
summary(SPRING_GAM_RperS)

draw(SPRING_GAM_RperS, parametric = TRUE, scales = "fixed")

par(mfrow = c(2, 2))
gam.check(SPRING_GAM_RperS)

SPRING_rps_pred <- data.frame(
  Year = FINAL_TABLE_SPRING$Year,
  rps.apx = FINAL_TABLE_SPRING$rps.apx,
  predicted_values = predict(SPRING_GAM_RperS, newdata = FINAL_TABLE_SPRING, type = "response")
)

# closures
S_rps_closures_plot <- ggplot() +
  geom_rect(aes(xmin = 1981, xmax = 1998, ymin = -Inf, ymax = Inf, fill = 'No Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 1998, xmax = 2009, ymin = -Inf, ymax = Inf, fill = 'Rolling Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2009, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Spring Spawning Closures'),
            alpha = 1/5) +
  geom_line(data = FINAL_TABLE_SPRING, aes(x = Year, y = rps.apx), size = 1, alpha = 0.5) +
  geom_line(data = SPRING_rps_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1)

#quota MGT
S_rps_quota_plot <- ggplot() +
  geom_line(data = FINAL_TABLE_SPRING, aes(x = Year, y = rps.apx), size = 1, alpha = 0.5) +
  geom_line(data = SPRING_rps_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1) +
  geom_rect(aes(xmin = 1981, xmax = 2010, ymin = -Inf, ymax = Inf, fill = 'Indirect Controls'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2010, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Quotas'),
            alpha = 1/5)

ggarrange(S_rps_closures_plot, S_rps_quota_plot, ncol = 1, nrow = 2)


## r GAM
SPRING_GAM_r <- gam(r ~ factor(Closures_SPRING) + factor(Quota_MGT)
                      + s(sst_anomaly, k = 4) + s(GSI, k = 4) + s(ssb.apx, k = 4) + Heatwave,
                      select = TRUE, data = FINAL_TABLE_SPRING, method = "REML", family=Tweedie(1.25,power(.1)))
summary(SPRING_GAM_r)

# draws partial effects of each explanatory variable (smooths and factors) on the response variable
# 0 is the mean response (in this case, mean ssb). above 0 means it has an increasing effect, below zero means a decreasing effect
draw(SPRING_GAM_r, parametric = TRUE, scales = "fixed")

par(mfrow = c(2, 2))
gam.check(SPRING_GAM_r)

SPRING_r_pred <- data.frame(
  Year = FINAL_TABLE_SPRING$Year,
  r = FINAL_TABLE_SPRING$r,
  predicted_values = predict(SPRING_GAM_r, newdata = FINAL_TABLE_SPRING, type = "response")
)

# closures
S_r_closures_plot <- ggplot() +
  geom_rect(aes(xmin = 1981, xmax = 1998, ymin = -Inf, ymax = Inf, fill = 'No Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 1998, xmax = 2009, ymin = -Inf, ymax = Inf, fill = 'Rolling Closures'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2009, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Spring Spawning Closures'),
            alpha = 1/5) +
  geom_line(data = FINAL_TABLE_SPRING, aes(x = Year, y = r), size = 1, alpha = 0.5) +
  geom_line(data = SPRING_r_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1)

#quota MGT
S_r_quota_plot <- ggplot() +
  geom_line(data = FINAL_TABLE_SPRING, aes(x = Year, y = r), size = 1, alpha = 0.5) +
  geom_line(data = SPRING_r_pred, aes(x = Year, y = predicted_values, color = 'Model'), size = 1) +
  geom_rect(aes(xmin = 1981, xmax = 2010, ymin = -Inf, ymax = Inf, fill = 'Indirect Controls'),
            alpha = 1/5) +
  geom_rect(aes(xmin = 2010, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'Quotas'),
            alpha = 1/5)

ggarrange(S_r_closures_plot, S_r_quota_plot, ncol = 1, nrow = 2)

 
#### APX and ST differences ####

W_ssb_apx_vs_st <- ggplot () + 
  geom_line(data = FINAL_TABLE_WINTER, aes (x= Year, y =ssb.apx), color = 'blue') +
  geom_line(data = FINAL_TABLE_WINTER, aes (x= Year, y =ssb.st), color = 'red') +
    labs(
    title = "Winter SSB Approximate vs Stanza",
    y = "SSB"
  )

S_ssb_apx_vs_st <- ggplot () + 
  geom_line(data = FINAL_TABLE_SPRING, aes (x= Year, y =ssb.apx), color = 'blue') +
  geom_line(data = FINAL_TABLE_SPRING, aes (x= Year, y =ssb.st), color = 'red') +
  labs(
    title = "Spring SSB Approximate vs Stanza",
    y = "SSB"
  )

W_rps_apx_vs_st <- ggplot () + 
  geom_line(data = FINAL_TABLE_WINTER, aes (x= Year, y =rps.apx), color = 'blue') +
  geom_line(data = FINAL_TABLE_WINTER, aes (x= Year, y =rps.st), color = 'red') +
    labs(
    title = "Winter RperS Approximate vs Stanza",
    y = "RperS"
  )

S_rps_apx_vs_st <- ggplot () + 
  geom_line(data = FINAL_TABLE_SPRING, aes (x= Year, y =rps.apx, color = 'blue')) +
  geom_line(data = FINAL_TABLE_SPRING, aes (x= Year, y =rps.st, color = 'red')) +
  theme(legend.position = 'bottom') +
  labs(
    title = "Spring RperS Approximate vs Stanza",
    y = "RperS"
  ) +
  scale_color_manual(values = c("blue", "red"), # specify color for each line
                     labels = c("Approximate ", "Stanza")) # add labels to each line


ggarrange(W_ssb_apx_vs_st, S_ssb_apx_vs_st, W_rps_apx_vs_st, S_rps_apx_vs_st,
          ncol = 1, nrow = 4) 



## Set k equal to number of covariates ##