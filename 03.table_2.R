##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# Table 2 
#
# Sara Lopes de Moraes
# Last updated: Sep 2025
##################################################################

library(tidyverse); library(dplyr); library(arrow)
library(glue); library(rlang)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data")

data <- read_parquet("data_hw_pop_INCLUDE_NOUNITCHG_L1AD_NI.parquet")

glimpse(data)

#---------------------------------------------
# Table 2 - HWN
#---------------------------------------------

# 1. mean and quartiles
hw_mean <- data %>%
  summarize(
    hw_mean = mean(HWN_EHF, na.rm = TRUE), .by = c(SALID1)) %>%
  mutate(hazard_HWN = factor(ntile(hw_mean, 4), labels = c("Q1", "Q2", "Q3", "Q4")))

# 2. Number of cities
table_2a <- hw_mean %>% 
  summarize(n_cities = length(unique(SALID1)), .by = hazard_HWN) %>% 
  pivot_wider(names_from = hazard_HWN,values_from = n_cities) %>%
  mutate(variables = "n_cities", .before = 1)   

# Join to obtain salid1 quantiles
data_table2 <- data %>% 
  left_join(hw_mean, by = "SALID1")

# 4. Pop exposure
table_2b <- data_table2 %>% 
  summarize(pop_ex = first(person_events_exposed), .by = c(hazard_HWN, SALID1, YEAR)) %>% 
  summarize(city_pop_ex = pop_ex/1000, .by = c(hazard_HWN, SALID1, YEAR)) %>% 
  summarize(city_pop_ex = mean(city_pop_ex), .by = c(hazard_HWN, SALID1)) %>% 
  summarize(city_pop_ex = glue("{med} ({lower}, {upper})",
                               med   = round(median(city_pop_ex), 0),
                               lower = round(quantile(city_pop_ex, .1), 0),
                               upper = round(quantile(city_pop_ex, .9), 0)), .by = hazard_HWN) %>%
  pivot_wider(names_from = hazard_HWN,
              values_from = city_pop_ex) %>%
  mutate(variables = "city_pop_ex", .before = 1)


# 5. Climate zones (number of cities pct)
table_2c <- data_table2 %>%
  distinct(hazard_HWN, CLZ, SALID1) %>% 
  count(hazard_HWN, CLZ, name = "n_cities") %>%
  group_by(hazard_HWN) %>%
  mutate(total = sum(n_cities),
         pct_clz = round(100 * n_cities / total, 1)) %>% 
  ungroup()  %>% 
  select(hazard_HWN, CLZ, pct_clz) %>% 
  pivot_wider(names_from = hazard_HWN,
              values_from = pct_clz,
              values_fill = 0 ) %>% 
  rename(variables = CLZ)



# 6. other Varibles
var <- c("pop_under5", 
         "pop_over65", 
         "area_km", 
         "pop_density_guf",
         "urban_area_pct",
         "NDVI",
         "GDP",
         # "GINI",
         "education",
         "overcrowding_rooms", 
         # "overcrowding_bedrooms", 
         "wall_dur",
         "sewage",
         "water_network") 

result_list <- lapply(var, function(v) {
  
  var_sym <- sym(v)
  
  data_table2 %>% 
    summarize(mean_val = mean(!!var_sym, na.rm = TRUE), .by = c(hazard_HWN, SALID1)) %>%
    summarize(!!v := glue("{med} ({lower}, {upper})",
                          med   = round(median(mean_val, na.rm = TRUE), 1),
                          lower = round(quantile(mean_val, 0.1, na.rm = TRUE), 1),
                          upper = round(quantile(mean_val, 0.9, na.rm = TRUE), 1)), .by = hazard_HWN) %>%
    pivot_wider(names_from = hazard_HWN, values_from = !!v) %>%
    mutate(variables = v, .before = 1)})

table_2d <- bind_rows(result_list)


# join tables - HWN as main HW variable
table2 <- rbind(table_2a, table_2b, table_2d, table_2c)

str(table2)

#rename variables and organize table
var_labels <- c(
  n_cities              = "Number of cities",
  city_pop_ex           = "Exposed population HWN (thousands)",
  pop_density_guf       = "Population density",
  GDP                   = "GDP per capita ",
  area_km               = "Area (km²)",
  urban_area_pct        = "Urban area (%)",
  NDVI                  = "NDVI",
  pop_under5            = "Population < 5 years (%)",
  pop_over65            = "Population ≥65 years (%)",
  #GINI                  = "Gini index",
  overcrowding_rooms    = "Overcrowding rooms (%)",
  #overcrowding_bedrooms = "Overcrowding bedrooms (%)",
  wall_dur              = "Walls durable materials (%)",
  sewage                = "Sewage network connection (%)",
  water_network         = "Water network (%)",
  education             = "Completed primary education (%)"
)

table2_names <- table2 %>%
  mutate(variables = recode(variables, !!!var_labels))

# arrange columns
table2_names <- table2_names %>% select(variables, Q1, Q2, Q3, Q4)


#---------------------------------------------
# Table 2 - POP EX HWN
#---------------------------------------------

# 1. mean and quartiles
pop_mean <- data %>%
  summarize(
    pop_mean = mean(person_events_exposed, na.rm = TRUE), .by = c(SALID1)) %>%
  mutate(pop_ex_HWN = factor(ntile(pop_mean, 4), labels = c("Q1", "Q2", "Q3", "Q4")))

# 2. Number of cities
table_2a <- pop_mean %>% 
  summarize(n_cities = length(unique(SALID1)), .by = pop_ex_HWN) %>% 
  pivot_wider(names_from = pop_ex_HWN,values_from = n_cities) %>%
  mutate(variables = "n_cities", .before = 1)   

#3. Join to obtain salid1 quantiles
data_table2 <- data %>% 
  left_join(pop_mean, by = "SALID1")


# 4. HWN 
table_2b <- data_table2 %>% 
  summarize(HWN_EHF = mean(HWN_EHF), .by = c(pop_ex_HWN, SALID1)) %>% 
  summarize(HWN_EHF = glue("{med} ({lower}, {upper})",
                               med   = round(median(HWN_EHF), 0),
                               lower = round(quantile(HWN_EHF, .1), 0),
                               upper = round(quantile(HWN_EHF, .9), 0)), .by = pop_ex_HWN) %>%
  pivot_wider(names_from = pop_ex_HWN,
              values_from = HWN_EHF) %>%
  mutate(variables = "HWN_EHF", .before = 1)


# 5. Climate zones (number of cities pct)
table_2c <- data_table2 %>%
  distinct(pop_ex_HWN, CLZ, SALID1) %>% 
  count(pop_ex_HWN, CLZ, name = "n_cities") %>%
  group_by(pop_ex_HWN) %>%
  mutate(total = sum(n_cities),
         pct_clz = round(100 * n_cities / total, 1)) %>% 
  ungroup()  %>% 
  select(pop_ex_HWN, CLZ, pct_clz) %>% 
  pivot_wider(names_from = pop_ex_HWN,
              values_from = pct_clz,
              values_fill = 0 ) %>% 
  rename(variables = CLZ)



# 6. other Varibles
var <- c("pop_under5", 
         "pop_over65", 
         "area_km", 
         "pop_density_guf",
         "urban_area_pct",
         "NDVI",
         "GDP",
         # "GINI",
         "education",
         "overcrowding_rooms", 
         # "overcrowding_bedrooms", 
         "wall_dur",
         "sewage",
         "water_network") 

result_list <- lapply(var, function(v) {
  
  var_sym <- sym(v)
  
  data_table2 %>% 
    summarize(mean_val = mean(!!var_sym, na.rm = TRUE), .by = c(pop_ex_HWN, SALID1)) %>%
    summarize(!!v := glue("{med} ({lower}, {upper})",
                          med   = round(median(mean_val, na.rm = TRUE), 1),
                          lower = round(quantile(mean_val, 0.1, na.rm = TRUE), 1),
                          upper = round(quantile(mean_val, 0.9, na.rm = TRUE), 1)), .by = pop_ex_HWN) %>%
    pivot_wider(names_from = pop_ex_HWN, values_from = !!v) %>%
    mutate(variables = v, .before = 1)})

table_2d <- bind_rows(result_list)


# join tables - HWN as main HW variable
table2 <- rbind(table_2a, table_2b, table_2d, table_2c)

str(table2)

#rename variables and organize table
var_labels <- c(
  n_cities              = "Number of cities",
  HWN_EHF               = "HWN - EHF",
  pop_density_guf       = "Population density",
  GDP                   = "GDP per capita ",
  area_km               = "Area (km²)",
  urban_area_pct        = "Urban area (%)",
  NDVI                  = "NDVI",
  pop_under5            = "Population < 5 years (%)",
  pop_over65            = "Population ≥65 years (%)",
  #GINI                  = "Gini index",
  overcrowding_rooms    = "Overcrowding rooms (%)",
  #overcrowding_bedrooms = "Overcrowding bedrooms (%)",
  wall_dur              = "Walls durable materials (%)",
  sewage                = "Sewage network connection (%)",
  water_network         = "Water network (%)",
  education             = "Completed primary education (%)"
)

table2_names <- table2 %>%
  mutate(variables = recode(variables, !!!var_labels))

# arrange columns
table2_names <- table2_names %>% select(variables, Q1, Q2, Q3, Q4)
