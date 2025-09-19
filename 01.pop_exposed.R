##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# This script calculate the annual population exposure to HW 
# 
# Sara Lopes de Moraes
# Last updated: Sep 2025
##################################################################

library(tidyverse); library(dplyr); library(arrow)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data")

data <- read_parquet("data_hw_INCLUDE_NOUNITCHG_L1AD.parquet")

glimpse(data)

#---------------------------------------------
# Annual and daily population exposed to HW
#---------------------------------------------
# 1. Calculation pop exposure
# a. Population exposed to at least one heatwave
# b. Population exposed to the number of HW in a year - Person-heatwaves
# c. Person-days of heatwave exposure (HWF)

pop_ex_hw <- data %>% 
  mutate(
    hw_bin = if_else(HWN_EHF > 0, 1, 0),               # at least one heatwave in the year
    pop_exposed = total_pop * hw_bin,                  # Population exposed to at least one heatwave
    person_events_exposed = total_pop * HWN_EHF,       # Person-heatwaves (person-events)
    person_days_exposed = total_pop * HWF_EHF)         # Person-days of heatwave exposure

# 2. export 
write_parquet(pop_ex_hw, "data_hw_pop_ex_INCLUDE_NOUNITCHG_L1AD.parquet")

