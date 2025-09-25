##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# Table 1 (examples) 
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
# Table 1
#---------------------------------------------

# 1. number of cities and study period for each country 
table_1a <- data %>% 
  summarize(n_cities = length(unique(SALID1)), .by = Country) %>% 
  pivot_wider(
    names_from = Country,
    values_from = n_cities
  ) %>%
  mutate(variables = "n_cities", .before = 1)   # cria coluna de nome da métrica


# 2. city population
table_1b <- data %>% 
  summarize(pop = first(total_pop), .by = c(Country, SALID1, YEAR)) %>% 
  summarize(city_pop = pop/1000, .by = c(Country, SALID1, YEAR)) %>% 
  summarize(city_pop = mean(city_pop), .by = c(Country, SALID1)) %>% 
  summarize(
    city_pop = glue("{med} ({lower}, {upper})",
                    med   = round(median(city_pop), 0),
                    lower = round(quantile(city_pop, .1), 0),
                    upper = round(quantile(city_pop, .9), 0)),.by = Country) %>%
  pivot_wider(names_from = Country,
              values_from = city_pop) %>%
  mutate(variables = "city_pop", .before = 1)

# 3. Pop exposure
table_1c <- data %>% 
  summarize(pop_ex = first(person_events_exposed), .by = c(Country, SALID1, YEAR)) %>% 
  summarize(city_pop_ex = pop_ex/1000, .by = c(Country, SALID1, YEAR)) %>% 
  summarize(city_pop_ex = mean(city_pop_ex), .by = c(Country, SALID1)) %>% 
  summarize(city_pop_ex = glue("{med} ({lower}, {upper})",
                       med   = round(median(city_pop_ex), 0),
                       lower = round(quantile(city_pop_ex, .1), 0),
                       upper = round(quantile(city_pop_ex, .9), 0)), .by = Country) %>%
  pivot_wider(names_from = Country,
              values_from = city_pop_ex) %>%
  mutate(variables = "city_pop_ex", .before = 1)

# 4. Climate zones (number of cities pct)
table_1d <- data %>%
  distinct(Country, CLZ, SALID1) %>% 
  count(Country, CLZ, name = "n_cities") %>%
  group_by(Country) %>%
  mutate(total = sum(n_cities),
         pct_clz = round(100 * n_cities / total, 1)) %>% 
  ungroup()  %>% 
  select(Country, CLZ, pct_clz) %>% 
  pivot_wider(names_from = Country,
              values_from = pct_clz,
              values_fill = 0 ) %>% 
  rename(variables = CLZ)


#5. HWN
table_1e <- data %>% 
  summarize(mean_HWN_EHF = mean(HWN_EHF, na.rm = TRUE), .by = c(Country, SALID1)) %>%
  summarize(HWN_EHF = glue("{med} ({lower}, {upper})",
                   med   = round(median(mean_HWN_EHF), 0),
                   lower = round(quantile(mean_HWN_EHF, .1), 0),
                   upper = round(quantile(mean_HWN_EHF, .9), 0)), .by = Country) %>%
  pivot_wider(names_from = Country,
              values_from = HWN_EHF) %>%
  mutate(variables = "HWN_EHF", .before = 1)

# # 6. HWF - check to add or not
# table_1f <- data %>% 
#   summarize(mean_HWF_EHF = mean(HWF_EHF, na.rm = TRUE), .by = c(Country, SALID1)) %>%
#   summarize(HWF_EHF = glue("{med} ({lower}, {upper})",
#                    med   = round(median(mean_HWF_EHF), 0),
#                    lower = round(quantile(mean_HWF_EHF, .1), 0),
#                    upper = round(quantile(mean_HWF_EHF, .9), 0)), .by = Country) %>%
#   pivot_wider(names_from = Country,
#               values_from = HWF_EHF) %>%
#   mutate(variables = "HWF_EHF", .before = 1)


# 6. other Varibles
var <- c("pop_under5", # missing NI
         "pop_over65", # missing NI
         "area_km", # missing AR
         "pop_density_guf",
         "urban_area_pct",
         "NDVI",
         "GDP",
         #"GINI",  # "Argentina", "Colombia", "Peru", "Central America"
         "education",
         "overcrowding_rooms", # missing NI
         #"overcrowding_bedrooms", # mising NI and Peru
         "wall_dur",
         "sewage",
         "water_network") 

result_list <- lapply(var, function(v) {
  
  var_sym <- sym(v)
  
  data %>% 
    summarize(mean_val = mean(!!var_sym, na.rm = TRUE), .by = c(Country, SALID1)) %>%
    summarize(!!v := glue("{med} ({lower}, {upper})",
                  med   = round(median(mean_val, na.rm = TRUE), 1),
                  lower = round(quantile(mean_val, 0.1, na.rm = TRUE), 1),
                  upper = round(quantile(mean_val, 0.9, na.rm = TRUE), 1)), .by = Country) %>%
    pivot_wider(names_from = Country,
                values_from = !!v) %>%
    mutate(variables = v, .before = 1)})

table_1g <- bind_rows(result_list)


# join tables - HWN as main HW variable
table1 <- rbind(table_1a, table_1e, table_1c, table_1b, table_1g, table_1d)

str(table1)

#rename variables and organize table
var_labels <- c(
  n_cities              = "Number of cities",
  HWN_EHF               = "HWN (EHF)",
  HWF_EHF               = "HWF (EHF)",
  city_pop              = "City population (thousands)",
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

table1_names <- table1 %>%
  mutate(variables = recode(variables, !!!var_labels))
