##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# This script prepare the final yearly datasets of heatwaves (HW), 
# population, and other variables for descriptive and modelling
# 
# HW: here we will consider only the EHF definition (main 
# definition for the paper), other definitions will be in the 
# supplementary material
#
#
# Sara Lopes de Moraes
# Last updated: Sep 2025
##################################################################

library(dplyr) 
library(arrow)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data")

data <- read.csv("L1AD_LONG_09082025.csv")
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
         BECCZL1AD,
         BECPCTURBANL1AD,
         PRJL1ADPOP,
         PRJPOPLT5L1AD,
         PRJPOPGE65L1AD,
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
data_final <- data_hw %>%
  mutate(
    CLZ = case_when(
      BECCZL1AD %in% 1:4 ~ "Tropical",
      BECCZL1AD %in% c(5, 6, 7, 8) ~ "Arid",
      BECCZL1AD %in% 9:17 ~ "Temperate",
      BECCZL1AD %in% 30:31 ~ "Polar",
      TRUE ~ "Other")) %>% 
  mutate(
    Country = case_when(
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
         NDVI = BECMEDNDVINWL1AD, 
         area_km = BECADAREAL1AD,
         pop_density_guf = BECPOPDENGUFL1AD,
         urban_area_pct= BECPCTURBANL1AD,
         total_pop = PRJL1ADPOP,
         pop_under5 = PRJPOPLT5L1AD,
         pop_over65 = PRJPOPGE65L1AD,
         GINI = SECGINIINCL1AD,
         GDP = GDP_PCL1AD,
         overcrowding_rooms = CNSCROWD3RML1AD,
         overcrowding_bedrooms = CNSCROWD25BRL1AD,
         wall_dur =  CNSWALLDUR1L1AD,
         sewage = CNSSEWNETL1AD,
         water_network = CNSWATNETL1AD,
         education =   CNSMINPR_L1AD)


# 3. check NA values
colSums(is.na(data_final)) > 0

unique(data_final$Country[is.na(data_final$area_km)]) # "Argentina"
unique(data_final$SALID1[is.na(data_final$area_km)]) # 101135

unique(data_final$Country[is.na(data_final$pop_under5)]) # "Central America" = NI
unique(data_final$SALID1[is.na(data_final$pop_under5)]) # 205101 205102 205103 205104 205105

unique(data_final$Country[is.na(data_final$pop_over65)]) # "Central America" = NI
unique(data_final$SALID1[is.na(data_final$pop_over65)]) # 205101 205102 205103 205104 205105

unique(data_final$Country[is.na(data_final$GINI)])# "Argentina", "Colombia", "Peru", "Central America"
unique(data_final$SALID1[is.na(data_final$GINI)]) 
#[1] 101105 101108 101120 101122 101123 101130 101135 104101 104104 104105 104108 104113 104114 104116 104120 104122 104126 104128 104129
#[20] 104130 104131 104132 104133 104134 104135 105101 105102 105103 105104 105105 105106 105107 105108 105109 105110 105111 105112 105114
#[39] 105115 105116 105117 105118 105119 105120 105121 105122 105123 201103 203101 203103 205101 205102 205103 205104 205105

unique(data_final$Country[is.na(data_final$overcrowding_rooms)]) # "Central America" NI
unique(data_final$SALID1[is.na(data_final$overcrowding_rooms)]) # 205101 205102 205103 205104 205105

unique(data_final$Country[is.na(data_final$overcrowding_bedrooms)]) # "Peru"            "Central America" NI
unique(data_final$SALID1[is.na(data_final$overcrowding_bedrooms)]) #  [1] 105101 105102 105103 105104 105105 105106 105107 
# 105108 105109 105110 105111 105112 105113 105114 105115 105116 105117 105118 105119
#[20] 105120 105121 105122 105123 205101 205102 205103 205104 205105

# drop GINI and overcrowding_bedrooms
# drop NI 

# # exclude the Known boundary change indicator
# INCLUDE_NOUNITCHG_L1AD
data_final_hw <- data_final %>% 
  filter(INCLUDE_NOUNITCHG_L1AD == 1) %>% 
  filter(ISO2 != "NI")

length(unique(data_final_hw$SALID1))
colSums(is.na(data_final_hw)) > 0

# export final dataset
write_parquet(data_final_hw, "data_hw_INCLUDE_NOUNITCHG_L1AD_NI.parquet")

# write_parquet(data_final, "data_hw.parquet")

