#############################################################
# MS296: Descriptive Heatwaves
#
# This script prepare the yearly datasets of heatwaves (HW), 
# population, and other variables for descriptive, exposed 
# population, and modelling
#############################################################

library(dplyr); library(arrow); library(here)
library(foreign)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296")

data<- read.csv("Data/2025_12_12/data/L1AD_LONG_20251212.csv")
data_latitude <- read.dbf("Data/SHP/L1AD_city_center.dbf")

colnames(data)

# 1. Select variables for the study
data_hw <- data %>%
  select(ISO2,
         SALID1, 
         YEAR,
         TMPMEAN_L1AD,
         matches("^HW.*PW"), #pop weight
         BECMEDNDVINWL1AD,
         BECADAREAL1AD,
         BECPOPDENGUFL1AD,
         BECCZL1L1UX,
         BECPCTURBANL1AD,
         PRJL1ADPOP,
         PRJL1ADPOP_LT5,
         PRJL1ADPOP_GE65,
         SECGINIINCL1AD,
         GDP_PCL1AD,
         CNSCROWD3RML1AD,
         CNSCROWD25BRL1AD,
         CNSWALLDUR1L1AD,
         CNSSEWNETL1AD,
         CNSWATNETL1AD,
         CNSMINPR_L1AD,
         INCLUDE_NOUNITCHG_L1AD,
         INCLUDE_NOBDRYCHG_L1AD)

# 2.  Create CLZ varible as names, recode countries and rename some columns
data_hw <- data_hw %>%
  mutate(CLZ = case_when(
      BECCZL1L1UX  == "A" ~ "Tropical",
      BECCZL1L1UX == "B" ~ "Arid",
      BECCZL1L1UX == "C" ~ "Temperate",
      BECCZL1L1UX == "D" ~ "continental",
      BECCZL1L1UX == "E" ~ "Polar",
      TRUE ~ "Other")) %>%
  mutate(Country = case_when(
      ISO2 == "AR" ~ "Argentina",
      ISO2 == "BR" ~ "Brazil",
      ISO2 == "CL" ~ "Chile",
      ISO2 %in% c("CR", "NI", "PA", "SV", "GT") ~ "Central America",
      ISO2 == "CO" ~ "Colombia",
      ISO2 == "MX" ~ "Mexico",
      ISO2 == "PE" ~ "Peru",
      TRUE ~ ISO2)) %>% 
  rename(HWF_EHF = HWVANNHWFEHFPWL1AD,
         HWN_EHF = HWVANNHWNEHFPWL1AD,
         HWD_EHF = HWVANNHWDEHFPWL1AD,
         HWA_EHF = HWVANNHWAEHFPWL1AD,
         HWM_EHF = HWVANNHWMEHFPWL1AD,
         HWF_90P_2d = HWVANNHWFPW_90_2_L1AD, 
         HWF_90P_3d = HWVANNHWFPW_90_3_L1AD, 
         HWF_90P_4d = HWVANNHWFPW_90_4_L1AD,
         HWF_95P_2d = HWVANNHWFPW_95_2_L1AD, 
         HWF_95P_3d = HWVANNHWFPW_95_3_L1AD, 
         HWF_95P_4d = HWVANNHWFPW_95_4_L1AD,
         HWF_97.5P_2d = HWVANNHWFPW_97_2_L1AD, 
         HWF_97.5P_3d = HWVANNHWFPW_97_3_L1AD, 
         HWF_97.5P_4d = HWVANNHWFPW_97_4_L1AD,
         HWN_90P_2d = HWVANNHWNPW_90_2_L1AD,
         HWN_90P_3d = HWVANNHWNPW_90_3_L1AD,
         HWN_90P_4d = HWVANNHWNPW_90_4_L1AD,
         HWN_95P_2d = HWVANNHWNPW_95_2_L1AD,
         HWN_95P_3d = HWVANNHWNPW_95_3_L1AD,
         HWN_95P_4d = HWVANNHWNPW_95_4_L1AD,
         HWN_97.5P_2d = HWVANNHWNPW_97_2_L1AD,
         HWN_97.5P_3d = HWVANNHWNPW_97_3_L1AD,
         HWN_97.5P_4d = HWVANNHWNPW_97_4_L1AD,
         HWD_90P_2d = HWVANNHWDPW_90_2_L1AD, 
         HWD_90P_3d = HWVANNHWDPW_90_3_L1AD, 
         HWD_90P_4d = HWVANNHWDPW_90_4_L1AD,
         HWD_95P_2d = HWVANNHWDPW_95_2_L1AD, 
         HWD_95P_3d = HWVANNHWDPW_95_3_L1AD, 
         HWD_95P_4d = HWVANNHWDPW_95_4_L1AD,
         HWD_97.5P_2d = HWVANNHWDPW_97_2_L1AD, 
         HWD_97.5P_3d = HWVANNHWDPW_97_3_L1AD, 
         HWD_97.5P_4d = HWVANNHWDPW_97_4_L1AD,
         HWA_90P_2d = HWVANNHWAPW_90_2_L1AD, 
         HWA_90P_3d = HWVANNHWAPW_90_3_L1AD, 
         HWA_90P_4d = HWVANNHWAPW_90_4_L1AD,
         HWA_95P_2d = HWVANNHWAPW_95_2_L1AD, 
         HWA_95P_3d = HWVANNHWAPW_95_3_L1AD, 
         HWA_95P_4d = HWVANNHWAPW_95_4_L1AD,
         HWA_97.5P_2d = HWVANNHWAPW_97_2_L1AD, 
         HWA_97.5P_3d = HWVANNHWAPW_97_3_L1AD, 
         HWA_97.5P_4d = HWVANNHWAPW_97_4_L1AD,
         HWM_90P_2d = HWVANNHWMPW_90_2_L1AD, 
         HWM_90P_3d = HWVANNHWMPW_90_3_L1AD, 
         HWM_90P_4d = HWVANNHWMPW_90_4_L1AD,
         HWM_95P_2d = HWVANNHWMPW_95_2_L1AD, 
         HWM_95P_3d = HWVANNHWMPW_95_3_L1AD, 
         HWM_95P_4d = HWVANNHWMPW_95_4_L1AD,
         HWM_97.5P_2d = HWVANNHWMPW_97_2_L1AD, 
         HWM_97.5P_3d = HWVANNHWMPW_97_3_L1AD, 
         HWM_97.5P_4d = HWVANNHWMPW_97_4_L1AD,
         NDVI = BECMEDNDVINWL1AD, 
         area_km = BECADAREAL1AD,
         pop_density_guf = BECPOPDENGUFL1AD,
         urban_area_pct= BECPCTURBANL1AD,
         total_pop = PRJL1ADPOP,
         pop_under5 = PRJL1ADPOP_LT5,
         pop_over65 = PRJL1ADPOP_GE65,
         GINI = SECGINIINCL1AD,
         GDP = GDP_PCL1AD,
         overcrowding_rooms = CNSCROWD3RML1AD,
         overcrowding_bedrooms = CNSCROWD25BRL1AD,
         wall_dur =  CNSWALLDUR1L1AD,
         sewage = CNSSEWNETL1AD,
         water_network = CNSWATNETL1AD,
         education =   CNSMINPR_L1AD)

# 3. check NA values
colSums(is.na(data_hw)) > 0

unique(data_hw$Country[is.na(data_hw$area_km)]) 
unique(data_hw$SALID1[is.na(data_hw$area_km)])

unique(data_hw$Country[is.na(data_hw$pop_under5)])
unique(data_hw$SALID1[is.na(data_hw$pop_under5)])

unique(data_hw$Country[is.na(data_hw$pop_over65)]) 
unique(data_hw$SALID1[is.na(data_hw$pop_over65)])

unique(data_hw$Country[is.na(data_hw$GINI)])
unique(data_hw$SALID1[is.na(data_hw$GINI)]) 

unique(data_hw$Country[is.na(data_hw$overcrowding_rooms)]) 
unique(data_hw$SALID1[is.na(data_hw$overcrowding_rooms)]) 

unique(data_hw$Country[is.na(data_hw$overcrowding_bedrooms)]) 
unique(data_hw$SALID1[is.na(data_hw$overcrowding_bedrooms)])

# export table for HW descriptive figures
write_parquet(data_hw, "Data/data_hw_descriptive.parquet")

# Table for modelling
# drop GINI and overcrowding_bedrooms
# drop NI 

# 4. Add latitude
data_latitude <- data_latitude %>% 
  select(SALID1, BEC_City_1) %>% 
  mutate(latitude = round(BEC_City_1, 1),   
         lat_abs = abs(latitude),           
         lat_abs_z = as.numeric(scale(lat_abs))) %>% 
  select(SALID1, latitude, lat_abs, lat_abs_z)

#join
data_hw_lat <- data_hw %>% left_join(data_latitude, by = "SALID1")

# 5. exclude the Known boundary change indicator (INCLUDE_NOUNITCHG_L1AD) and NI cities
#all cities
data_hw_L1AD_chac <- data_hw_lat %>%
  group_by(SALID1) %>%
  filter(!any(INCLUDE_NOUNITCHG_L1AD == 0)) %>% 
  ungroup() %>% 
  filter(ISO2 != "NI")

length(unique(data_hw_L1AD_chac$SALID1))
length(unique(data_hw_L1AD_chac$Country))
colSums(is.na(data_hw_L1AD_chac)) > 0

# check which cities and countries were excluded
excluded_salid1 <- data_hw_lat %>%
  group_by(SALID1) %>%
  filter(any(INCLUDE_NOUNITCHG_L1AD == 0)) %>%
  distinct(SALID1) %>%
  pull(SALID1)
excluded_salid1

excluded_countries <- data_hw_lat %>%
  filter(SALID1 %in% excluded_salid1) %>%
  distinct(Country) %>%
  pull(Country)
excluded_countries

# 6. Create a column of decade (modelling step)
# Year by decade
data_hw_L1AD_chac <- data_hw_L1AD_chac %>%
  mutate(YEAR_dec = (YEAR - 2000) / 10)

# export dataset
write_parquet(data_hw_L1AD_chac, "Data/data_hw_INCLUDE_NOUNITCHG_L1AD_NI.parquet")
