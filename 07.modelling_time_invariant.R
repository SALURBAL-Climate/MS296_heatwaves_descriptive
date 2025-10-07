##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# This script multilevel models for NHW -  add variables
# Time invariant factors
# 
# Sara Lopes de Moraes
# Last updated: Oct 2025
##################################################################

library(dplyr); library(glmmTMB); library(arrow)
library(tidyr); library(tidyverse); library(lme4)
library(ggplot2); library(ggeffects); library(patchwork)
library(performance); library(DHARMa); library(broom.mixed)
library(splines); library(broom.mixed); library(report)
library(car); library(sjPlot)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Model")

data <- read_parquet("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/data_final.parquet")

str(data)

data$SALID1 <- as.factor(data$SALID1)
data$Country <- as.factor(data$Country)
data$CLZ <- as.factor(data$CLZ)

data <- data[order(data$SALID1, data$YEAR), ]


# ------------------------------------------
# HWN - Time invariant factors
# ------------------------------------------
# yearly
# center the YEAR (2000 as reference)
data <- data %>% mutate(YEAR_c = YEAR - 2000)

# Arid as reference?
m_clz <- glmmTMB(HWN_EHF ~ CLZ + YEAR_c + CLZ:YEAR_c + (1 | Country/SALID1), 
                data = data, 
                family = nbinom2) 
summary(m_clz)

check_overdispersion(m_clz)
check_zeroinflation(m_clz)
simulation_output <- simulateResiduals(m_clz)  %>%  plot()

icc_vals <- performance::icc(m_clz, by_group = TRUE)
print(icc_vals)

tab_model(m_clz)

# export table
tab_model(m_clz, transform = "exp", string.est = "Rate Ratio", string.ci = "Conf. Int.", 
          string.p = "p-value", string.pred = "Coefficients", string.intercept = "Intercept", 
          dv.labels = "HWN_EHF", pred.labels = c(becurbaygtraffictimellux_z = "avg traffic time"), 
          file = "m_clz.html")


# Year by decade
data <- data %>%
  mutate(YEAR_dec = (YEAR - 2000) / 10)

# Arid as reference?
m_clz_d <- glmmTMB(HWN_EHF ~ CLZ + YEAR_dec + CLZ:YEAR_dec + (1 | Country/SALID1), 
                                   data = data, 
                                   family = nbinom2) 
summary(m_clz_d)

check_overdispersion(m_clz_d)
check_zeroinflation(m_clz_d)
simulation_output <- simulateResiduals(m_clz_d)  %>%  plot()

icc_vals <- performance::icc(m_clz_d, by_group = TRUE)
print(icc_vals)

tab_model(m_clz_d)

# export table
tab_model(m_clz_d, transform = "exp", string.est = "Rate Ratio", string.ci = "Conf. Int.", 
          string.p = "p-value", string.pred = "Coefficients", string.intercept = "Intercept", 
          dv.labels = "HWN_EHF", pred.labels = c(becurbaygtraffictimellux_z = "avg traffic time"), 
          file = "m_clz_d.html")


