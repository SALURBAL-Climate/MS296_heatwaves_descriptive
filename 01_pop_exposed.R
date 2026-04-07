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
# source: https://doi.org/10.1038/s41586-023-06963-z
##################################################################

library(tidyverse); library(dplyr); library(arrow)
library(tidyr); library(writexl)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296")

data <- read_parquet("Data/data_hw_INCLUDE_NOUNITCHG_L1AD_NI.parquet")

hw_vars <- c( "HWN_EHF", 
              "HWN_90P_2d", "HWN_90P_3d", "HWN_90P_4d",
              "HWN_95P_2d", "HWN_95P_3d", "HWN_95P_4d",
              "HWN_97.5P_2d", "HWN_97.5P_3d", "HWN_97.5P_4d")

hwf_vars <- c("HWF_EHF", 
              "HWF_90P_2d", "HWF_90P_3d", "HWF_90P_4d",
              "HWF_95P_2d", "HWF_95P_3d", "HWF_95P_4d",
              "HWF_97.5P_2d", "HWF_97.5P_3d", "HWF_97.5P_4d")

#-----------------------------------------------------------
# Annual population exposed to HW (ine event, events, days)
#-----------------------------------------------------------
# Calculation of pop exposed

# 1. Population exposed to at least one heatwave
# HWN
for(v in hw_vars){
  data <- data %>%  
    mutate(
      !!paste0("pop_exposed_", v) :=
        if_else(.data[[v]] > 0, total_pop, 0))}

# 2. Person-heatwaves (person-events)
# HWN
for(v in hw_vars){
  data <- data %>%
    mutate(!!paste0("person_events_", v) := total_pop * .data[[v]])}

# 3. # Person-days of heatwave exposure of heatwave exposure
# HWF
for(v in hwf_vars){
  data <- data %>%
    mutate(
      !!paste0("person_days_", v) := total_pop * .data[[v]])}

person_events_vars <- grep("^person_events_", names(data), value = TRUE)
person_days_vars <- grep("^person_days_", names(data), value = TRUE)

# export 
write_parquet(data, "Data/data_final_model.parquet")


#------------------------------------------------------------
# Calculate annual pop and exposed pop total - country- city
#------------------------------------------------------------
pop_annual_c <- data %>% 
  group_by(Country, YEAR) %>%
  summarise(pop_total     = sum(total_pop, na.rm = TRUE),
            pop_exposed   = sum(pop_exposed_HWN_EHF, na.rm = TRUE),
            person_events = sum(person_events_HWN_EHF, na.rm = TRUE),
            person_days   = sum(person_days_HWF_EHF, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(pct_exposed_events = (pop_exposed / pop_total) * 100)

pop_annual_pct <- pop_annual_c %>% 
  select(Country, YEAR, pct_exposed_events) %>% 
  pivot_wider(names_from  = Country,
    values_from = pct_exposed_events)

# write_xlsx(pop_annual_c, "C:/Users/saral/Desktop/pop_annual_c.xlsx")

pop_total_year <- data %>%
  group_by(YEAR) %>%
  summarise(pop_total     = sum(total_pop, na.rm = TRUE),
            pop_exposed   = sum(pop_exposed_HWN_EHF, na.rm = TRUE),
            person_events = sum(person_events_HWN_EHF, na.rm = TRUE),
            person_days   = sum(person_days_HWF_EHF, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(pct_exposed_events = (pop_exposed / pop_total) * 100)

pop_annual_city <- data %>% 
  group_by(Country, SALID1, YEAR) %>%
  summarise(pop_total     = sum(total_pop, na.rm = TRUE),
            pop_exposed   = sum(pop_exposed_HWN_EHF, na.rm = TRUE),
            person_events = sum(person_events_HWN_EHF, na.rm = TRUE),
            person_days   = sum(person_days_HWF_EHF, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(pct_exposed_events = (pop_exposed / pop_total) * 100)

pop_city_2023_2024 <- pop_annual_city %>% filter(YEAR == 2020 | YEAR == 2023 | YEAR == 2024)

# write_xlsx(pop_annual_city, "C:/Users/saral/Desktop/pop_annual_city.xlsx")

# Sup tables 
person_days_vars   <- names(data)[grepl("^person_days_", names(data))]
person_events_vars <- names(data)[grepl("^person_events_", names(data))]

# person days
pop_country_year <- data %>%
  group_by(Country, YEAR) %>%
  summarise(across(all_of(c(person_days_vars, person_events_vars)),
                   ~ sum(.x, na.rm = TRUE)),.groups = "drop")

pop_country_mean <- pop_country_year %>%
  group_by(Country) %>%
  summarise(across(all_of(c(person_days_vars, person_events_vars)),
                   ~ mean(.x, na.rm = TRUE)),.groups = "drop")

# wide table with pop + share (%)
make_wide_with_share <- function(data_mean, vars) {
  data_mean %>%
    select(Country, all_of(vars)) %>%
    pivot_longer(cols = all_of(vars),
                 names_to  = "metric",
                 values_to = "mean_value") %>%
    group_by(metric) %>%
    mutate(pct_total = 100 * mean_value / sum(mean_value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(value_share = sprintf(
      "%.0f (%.1f%%)",
      mean_value,
      pct_total)) %>%
    select(metric, Country, value_share) %>%
    pivot_wider(
      names_from  = Country,
      values_from = value_share)}

# Tables
table_person_days   <- make_wide_with_share(pop_country_mean, person_days_vars)
table_person_events <- make_wide_with_share(pop_country_mean, person_events_vars)

#rename variables
table_person_events_final <- table_person_events %>%
  mutate(metric = metric |>
      gsub("^person_events_HWN_", "", x = _)) %>%
  rename("  " = metric)

person_events_row <- table_person_events_final[1, ]
person_events_row[1, ] <- "Person-events exposure"
person_events_row[-1] <- ""

table_person_events_final <- bind_rows(
  person_events_row,
  table_person_events_final)

table_person_days_final <- table_person_days %>%
  mutate(metric = metric |>
           gsub("^person_days_HWF_", "", x = _)) %>%
  rename("  " = metric)

person_days_row <- table_person_days_final[1, ]
person_days_row[1, ] <- "Person-days exposure"
person_days_row[-1] <- ""

table_person_days_final <- bind_rows(
  person_days_row,
  table_person_days_final)

# join tables
sup_table_final <- bind_rows(table_person_days_final, table_person_events_final)

# export
write_xlsx(sup_table_final, "Tables/Sup_table_3.xlsx")