##################################################################
# MS296: Descriptive Heatwaves- L1AD level
#
# This script calculate the population exposed to HW events
# 1. Population exposed to at least one heatwave event
# 2. Population exposed to the number of HWN in a year - 
#     Annual person-heatwaves
# 3. Person-days of heatwave exposure in a year (HWF) - 
#     Annual person-days of heatwave exposed
#
##################################################################

library(tidyverse); library(dplyr); library(arrow)
library(tidyr); library(writexl)

data <- read_parquet("Data/data_hw_INCLUDE_NOUNITCHG_L1AD_NI.parquet")

hw_vars <- c( "HWN_EHF", 
              "HWN_90P_2d", "HWN_90P_3d", "HWN_90P_4d",
              "HWN_95P_2d", "HWN_95P_3d", "HWN_95P_4d",
              "HWN_97.5P_2d", "HWN_97.5P_3d", "HWN_97.5P_4d")

hwf_vars <- c("HWF_EHF", 
              "HWF_90P_2d", "HWF_90P_3d", "HWF_90P_4d",
              "HWF_95P_2d", "HWF_95P_3d", "HWF_95P_4d",
              "HWF_97.5P_2d", "HWF_97.5P_3d", "HWF_97.5P_4d")

# Add a column of pop 2000 data only (year 2000 as constant) for each SALID1
data <- data %>%
  group_by(SALID1) %>%
  mutate(pop_2000 = total_pop[YEAR == 2000][1]) %>%
  ungroup()

#-----------------------------------------------------------------
# Annual population exposed to HW (one event, events, days/years)
#-----------------------------------------------------------------
# Calculation of pop exposed

# 1. Population exposed to at least one heatwave
# HWN
for(v in hw_vars){
  data <- data %>%  
    mutate(
      !!paste0("pop_exposed_", v) :=
        if_else(.data[[v]] > 0, total_pop, 0),
      !!paste0("pop2000_exposed_", v) :=
        if_else(.data[[v]] > 0, pop_2000, 0))
}

# 2. Person-heatwaves (person-events)
# HWN
for(v in hw_vars){
  data <- data %>%
    mutate(
      !!paste0("person_events_", v) := total_pop * .data[[v]],
      !!paste0("person_events_2000_", v) := pop_2000 * .data[[v]])
}

# 3. Person-days
# HWF
for(v in hwf_vars){
  data <- data %>%
    mutate(
      !!paste0("person_days_", v) := total_pop * .data[[v]],
      !!paste0("person_days_2000_", v) := pop_2000 * .data[[v]])
}
person_days_vars <- grep("^person_days_", names(data), value = TRUE)

# 4. person-years (PY)
for(v in person_days_vars){
  
  new_name <- gsub("^person_days_", "py_", v)
  
  data <- data %>%
    mutate(
      !!new_name := round(.data[[v]] / 365.25, 0))
}

# export 
write_parquet(data, "Data/data_final_model.parquet")
