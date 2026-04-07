#############################################################
# MS296: Descriptive Heatwaves
#
# This runs null models to get variance estimates
#############################################################

library(dplyr); library(tidyr); library(tidyverse) 
library(arrow); library(glmmTMB); library(patchwork)
library(performance); library(broom.mixed); library(sjPlot)       
library(ggplot2); library(writexl); library(brms)
library(insight)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296")

data <- read_parquet("Data/data_final_model.parquet")

str(data)

data$SALID1 <- as.factor(data$SALID1)
data$Country <- as.factor(data$Country)

data <- data[order(data$SALID1, data$YEAR), ]

# ------------------------------------------------
# HWN - add YEAR_dec as linear for EHF definition
# ------------------------------------------------
# 1. Model
model_null <- glmmTMB(HWN_EHF ~ 1 + (1 | Country/SALID1), 
                                data = data, 
                                family = nbinom2(link = "log"))
summary(model_null)
tab_model(model_null)

icc(model_null)
icc(model_null, by_group = TRUE)
# ICC by Group
# 
# Group          |   ICC
# ----------------------
#   SALID1:Country | 0.006
# Country        | 0.022

get_variance(model_null)
# $var.intercept
# SALID1:Country        Country 
# 0.002806319    0.010021974

get_variance_residual(model_null)
# var.residual 
# 0.4501249

library(insight)

get_variance(model_null)

var_country <- 0.010021974
var_city    <- 0.002806319
var_dist    <- v$var.distribution

V_total <- var_country + var_city + var_dist

prop_country <- var_country / V_total
prop_city    <- var_city / V_total
prop_within  <- var_dist / V_total

data.frame(
  Level = c("Country", "City", "Within-city (distribution)"),
  Variance = c(var_country, var_city, var_dist),
  Proportion = c(prop_country, prop_city, prop_within)* 100)
# Level    Variance Proportion
# 1                    Country 0.010021974  2.1647919
# 2                       City 0.002806319  0.6061776
# 3 Within-city (distribution) 0.450124941 97.2290305


