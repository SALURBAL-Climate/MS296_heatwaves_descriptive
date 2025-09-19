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

data <- read_parquet("data_hw_pop_ex_INCLUDE_NOUNITCHG_L1AD.parquet")

glimpse(data)

#---------------------------------------------
# Table 2 - HWN
#---------------------------------------------

# 1. mean and quartiles
hw_mean <- data %>%
  summarize(
    hw_mean = mean(HWN_EHF, na.rm = TRUE),
    across(-c(HWN_EHF), first),   # mantém as demais colunas
    .by = c(SALID1)
  ) %>%
  mutate(
    hazard_HWN = factor(
      ntile(hw_mean, 4),
      labels = c("Q1", "Q2", "Q3", "Q4")))

# 2. Number of cities
table_2a <- hw_mean %>% 
  summarize(n_cities = length(unique(SALID1)), .by = hazard_HWN) %>% 
  pivot_wider(
    names_from = hazard_HWN,
    values_from = n_cities
  ) %>%
  mutate(variables = "n_cities", .before = 1)   # cria coluna de nome da métrica


# 3. city population
table_2b <- hw_mean %>% 
  summarize(pop = first(total_pop), .by = c(hazard_HWN, SALID1, YEAR)) %>% 
  summarize(city_pop = pop/1000, .by = c(hazard_HWN, SALID1, YEAR)) %>% 
  summarize(city_pop = mean(city_pop), .by = c(hazard_HWN, SALID1)) %>% 
  summarize(
    city_pop = glue("{med} ({lower}, {upper})",
                    med   = round(median(city_pop), 0),
                    lower = round(quantile(city_pop, .1), 0),
                    upper = round(quantile(city_pop, .9), 0)),.by = hazard_HWN) %>%
  pivot_wider(names_from = hazard_HWN,
              values_from = city_pop) %>%
  mutate(variables = "city_pop", .before = 1)

# 4. Pop exposure
table_2c <- hw_mean %>% 
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
table_2d <- hw_mean %>%
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
var <- c("pop_under5", # missing NI
         "pop_over65", # missing NI
         "area_km", # missing AR
         "pop_density_guf",
         "urban_area_pct",
         "NDVI",
         "GDP",
         "GINI",  # "Argentina", "Colombia", "Peru", "Central America"
         "education",
         "overcrowding_rooms", # missing NI
         "overcrowding_bedrooms", # mising NI and Peru
         "wall_dur",
         "sewage",
         "water_network") 

result_list <- lapply(var, function(v) {
  
  var_sym <- sym(v)
  
  hw_mean %>% 
    summarize(mean_val = mean(!!var_sym, na.rm = TRUE), .by = c(hazard_HWN, SALID1)) %>%
    summarize(!!v := glue("{med} ({lower}, {upper})",
                          med   = round(median(mean_val, na.rm = TRUE), 1),
                          lower = round(quantile(mean_val, 0.1, na.rm = TRUE), 1),
                          upper = round(quantile(mean_val, 0.9, na.rm = TRUE), 1)), .by = hazard_HWN) %>%
    pivot_wider(names_from = hazard_HWN, values_from = !!v) %>%
    mutate(variables = v, .before = 1)})

table_2e <- bind_rows(result_list)


# join tables - HWN as main HW variable
table2 <- rbind(table_2a, table_2c, table_2b, table_2e, table_2d)

str(table2)

#rename variables and organize table
var_labels <- c(
  n_cities              = "Number of cities",
  city_pop              = "City population (thousands)",
  city_pop_ex           = "Exposed population HWN (thousands)",
  pop_density_guf       = "Population density",
  GDP                   = "GDP per capita ",
  area_km               = "Area (km²)",
  urban_area_pct        = "Urban area (%)",
  NDVI                  = "NDVI",
  pop_under5            = "Population < 5 years (%)",
  pop_over65            = "Population ≥65 years (%)",
  GINI                  = "Gini index",
  overcrowding_rooms    = "Overcrowding rooms (%)",
  overcrowding_bedrooms = "Overcrowding bedrooms (%)",
  wall_dur              = "Walls durable materials (%)",
  sewage                = "Sewage network connection (%)",
  water_network         = "Water network (%)",
  education             = "Completed primary education (%)"
)

table2_names <- table2 %>%
  mutate(variables = recode(variables, !!!var_labels))

# arrange columns
table2_names <- table2_names %>% select(variables, Q1, Q2, Q3, Q4)

