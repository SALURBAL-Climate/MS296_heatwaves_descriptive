##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# This script multilevel models for NHW -  add variables
# Time variant factors
# 
# Sara Lopes de Oct 2025
##################################################################

library(dplyr); library(glmmTMB); library(arrow)
library(tidyr); library(tidyverse); library(lme4)
library(ggplot2); library(ggeffects); library(patchwork)
library(performance); library(DHARMa); library(broom.mixed)
library(splines); library(broom.mixed); library(report)
library(car); library(sjPlot); library(corrplot)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Model")

data <- read_parquet("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/data_final.parquet")

str(data)

data$SALID1 <- as.factor(data$SALID1)
data$Country <- as.factor(data$Country)

data <- data[order(data$SALID1, data$YEAR), ]

# correl
col <- colorRampPalette(c("#BB4444", "#EE9988",  
                          "#FFFFFF", "#77AADD", 
                          "#4477AA")) 
cor_vars <- data %>% 
  select(pop_density_guf, NDVI, urban_area_pct, pop_over65, pop_under5, GDP,
         overcrowding_rooms, water_network, sewage, education)

cor_matrix <- cor(cor_vars, use = "complete.obs")


# plot
corrplot(cor_matrix, method = "color", col = col(200),   
         type = "upper", order = "hclust",  
         addCoef.col = "black", # adiciona valores
         tl.col = "black", tl.srt = 45, # texto
         sig.level = 0.01, insig = "blank",  
         diag = FALSE)


# cor_vars <- data %>% 
#   select(pop_density_guf, urban_area_pct, pop_over65, pop_under5, GDP,
#          overcrowding_rooms, water_network, sewage, education)
# 
# cor_matrix <- cor(cor_vars, use = "complete.obs")
# print(cor_matrix)

# ------------------------------------------
# HWN - Time variant factors
# ------------------------------------------
# Year by decade
data <- data %>%
  mutate(YEAR_dec = (YEAR - 2000) / 10)

# scale variables
data <- data %>% 
  mutate(
    pop_density_guf = scale(pop_density_guf),
    urban_area_pct = scale(urban_area_pct),
    pop_over65 = scale(pop_over65),
    pop_under5 = scale(pop_under5),
    GDP = scale(GDP),
    NDVI = scale(NDVI),
    overcrowding_rooms = scale(overcrowding_rooms),
    water_network = scale(water_network),
    sewage = scale(sewage),
    education = scale(education))

# Add variables one by one
# 1.pop density 
m_v1 <- glmmTMB(HWN_EHF ~ pop_density_guf + YEAR_dec + (1 | Country/SALID1), 
                family = nbinom2(link = "log"),
                data = data,) 
summary(m_v1)

tab_model(m_v1)

# export table
tab_model(m_v1, transform = "exp", string.est = "Rate Ratio", string.ci = "Conf. Int.", 
          string.p = "p-value", string.pred = "Coefficients", string.intercept = "Intercept", 
          dv.labels = "HWN_EHF", pred.labels = c(becurbaygtraffictimellux_z = "avg traffic time"), 
          file = "m_v1.html")

  
# 2.pop over 65 years
m_v2 <- glmmTMB(HWN_EHF ~ pop_density_guf + pop_over65 + YEAR_dec + (1 | Country/SALID1), 
                family = nbinom2(link = "log"),
                data = data,) 
summary(m_v2)
tab_model(m_v2)


# 3.GDP
m_v3 <- glmmTMB(HWN_EHF ~ pop_density_guf + pop_over65 + 
                  GDP + YEAR_dec + (1 | Country/SALID1), 
                family = nbinom2(link = "log"),
                data = data,) 
summary(m_v3)
tab_model(m_v3)

# 4. education 
m_v4 <- glmmTMB(HWN_EHF ~ pop_density_guf + pop_over65 + 
                  GDP + education + YEAR_dec + (1 | Country/SALID1), 
                family = nbinom2(link = "log"),
                data = data,) 
summary(m_v4)
tab_model(m_v4)

# 5. water_network
m_v5 <- glmmTMB(HWN_EHF ~ pop_density_guf + pop_over65 + 
                  GDP + education + water_network + 
                  YEAR_dec + (1 | Country/SALID1), 
                family = nbinom2(link = "log"),
                data = data,) 
summary(m_v5)
tab_model(m_v5)

# 6. urban_area_pct
m_v6 <- glmmTMB(HWN_EHF ~ pop_density_guf + pop_over65 + 
                  GDP + education + water_network + urban_area_pct +
                  YEAR_dec + (1 | Country/SALID1), 
                family = nbinom2(link = "log"),
                data = data,) 
summary(m_v6)
tab_model(m_v6)

# 7. NDVI
m_v7 <- glmmTMB(HWN_EHF ~ pop_density_guf + pop_over65 + 
                  GDP + education + water_network + urban_area_pct +
                  YEAR_dec + (1 | Country/SALID1), 
                family = nbinom2(link = "log"),
                data = data,) 
summary(m_v7)
tab_model(m_v7)

check_collinearity(m_v7)
simulation_output <- simulateResiduals(m_v7)  %>%  plot()


# Final model
m_time_variant <- glmmTMB(HWN_EHF ~ pop_over65 + water_network + 
                            YEAR_dec + (1 | Country/SALID1), 
                          family = nbinom2(link = "log"),
                          data = data,) 

summary(m_time_variant)
tab_model(m_time_variant)

simulation_output <- simulateResiduals(m_time_variant)  %>%  plot()

res <- simulateResiduals(m_time_variant)
res_agg <- recalculateResiduals(res, group = data$YEAR_dec)
testTemporalAutocorrelation(res_agg, time = sort(unique(data$YEAR_dec)))


# export table
tab_model(m_time_variant, transform = "exp", string.est = "Rate Ratio", string.ci = "Conf. Int.", 
          string.p = "p-value", string.pred = "Coefficients", string.intercept = "Intercept", 
          dv.labels = "HWN_EHF", pred.labels = c(becurbaygtraffictimellux_z = "avg traffic time"), 
          file = "m_time_variant.html")

#---------------------------------------------------- HWF ----------------------------------------------------------------------------------------
# ------------------------------------------
# HWF - Time variant factors
# ------------------------------------------

# Add variables one by one
# 1.pop density 
m_v1_f <- glmmTMB(HWF_EHF ~ pop_density_guf + YEAR_dec + (1 | Country/SALID1), 
                  family = nbinom2(link = "log"),
                  data = data,) 
summary(m_v1_f)

tab_model(m_v1_f)

# export table
tab_model(m_v1_f, transform = "exp", string.est = "Rate Ratio", string.ci = "Conf. Int.", 
          string.p = "p-value", string.pred = "Coefficients", string.intercept = "Intercept", 
          dv.labels = "HWF_EHF", pred.labels = c(becurbaygtraffictimellux_z = "avg traffic time"), 
          file = "m_v1_f.html")


# 2.pop over 65 years
m_v2_f <- glmmTMB(HWF_EHF ~ pop_density_guf + pop_over65 + YEAR_dec + (1 | Country/SALID1), 
                  family = nbinom2(link = "log"),
                  data = data,) 
summary(m_v2_f)
tab_model(m_v2_f)


# 3.GDP
m_v3_f <- glmmTMB(HWF_EHF ~ pop_density_guf + pop_over65 + 
                    GDP + YEAR_dec + (1 | Country/SALID1), 
                  family = nbinom2(link = "log"),
                  data = data,) 
summary(m_v3_f)
tab_model(m_v3_f)

# 4. education 
m_v4_f <- glmmTMB(HWF_EHF ~ pop_density_guf + pop_over65 + 
                    GDP + education + YEAR_dec + (1 | Country/SALID1), 
                  family = nbinom2(link = "log"),
                  data = data,) 
summary(m_v4_f)
tab_model(m_v4_f)

# 5. water_network
m_v5_f <- glmmTMB(HWF_EHF ~ pop_density_guf + pop_over65 + 
                    GDP + education + water_network + 
                    YEAR_dec + (1 | Country/SALID1), 
                  family = nbinom2(link = "log"),
                  data = data,) 
summary(m_v5_f)
tab_model(m_v5_f)

# 6. urban_area_pct
m_v6_f <- glmmTMB(HWF_EHF ~ pop_density_guf + pop_over65 + 
                    GDP + education + water_network + urban_area_pct +
                    YEAR_dec + (1 | Country/SALID1), 
                  family = nbinom2(link = "log"),
                  data = data,) 
summary(m_v6_f)
tab_model(m_v6_f)

# 7. NDVI
m_v7_f <- glmmTMB(HWF_EHF ~ pop_density_guf + pop_over65 + 
                    GDP + education + water_network + urban_area_pct +
                    YEAR_dec + (1 | Country/SALID1), 
                  family = nbinom2(link = "log"),
                  data = data,) 
summary(m_v7_f)
tab_model(m_v7_f)

check_collinearity(m_v7_f)
simulation_output <- simulateResiduals(m_v7_f)  %>%  plot()


# Final model
m_time_variant_f <- glmmTMB(HWF_EHF ~ pop_over65 + water_network + 
                              YEAR_dec + (1 | Country/SALID1), 
                            family = nbinom2(link = "log"),
                            data = data,) 

summary(m_time_variant_f)
tab_model(m_time_variant_f)

simulation_output <- simulateResiduals(m_time_variant_f)  %>%  plot()

res <- simulateResiduals(m_time_variant_f)
res_agg <- recalculateResiduals(res, group = data$YEAR_dec)
testTemporalAutocorrelation(res_agg, time = sort(unique(data$YEAR_dec)))


# export table
tab_model(m_time_variant_f, transform = "exp", string.est = "Rate Ratio", string.ci = "Conf. Int.", 
          string.p = "p-value", string.pred = "Coefficients", string.intercept = "Intercept", 
          dv.labels = "HWF_EHF", pred.labels = c(becurbaygtraffictimellux_z = "avg traffic time"), 
          file = "m_time_variant_f.html")