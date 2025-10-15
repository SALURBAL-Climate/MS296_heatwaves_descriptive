##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# This script multilevel models for NHW - trends - 
# Percentile definitions
# 
# Sara Lopes de Moraes
# Last updated: Oct 2025
##################################################################

library(dplyr); library(glmmTMB); library(arrow)
library(tidyr); library(tidyverse); library(lme4)
library(ggplot2); library(ggeffects); library(patchwork)
library(performance); library(DHARMa); library(broom.mixed)
library(splines); library(broom.mixed); library(report)
library(sjPlot); library(writexl)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Model")

data <- read_parquet("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/data_final.parquet")

str(data)

data2 <- read.csv("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/HWN_L1AD.csv")

str(data2)

data_hwp <- data2 %>% select(SALID1, year,
                             HWN_90P_2d_L1AD_tpw, HWN_90P_3d_L1AD_tpw, HWN_90P_4d_L1AD_tpw,
                             HWN_95P_2d_L1AD_tpw, HWN_95P_3d_L1AD_tpw, HWN_95P_4d_L1AD_tpw,
                             HWN_97.5P_2d_L1AD_tpw, HWN_97.5P_3d_L1AD_tpw, HWN_97.5P_4d_L1AD_tpw)

data <- data %>% left_join(data_hwp, by = c("SALID1" = "SALID1", "YEAR" = "year"))


# ------------------------------------------
# HWN -  YEAR as linear 
# ------------------------------------------
# 1. Model with linear trend (YEAR fixed effect)
# Year by decade
data <- data %>%
  mutate(YEAR_dec = (YEAR - 2000) / 10)

# hw variables
# dep_vars <- c(
#   "HWVANNHWNPW_90_2_L1AD", "HWVANNHWNPW_90_3_L1AD", "HWVANNHWNPW_90_4_L1AD",
#   "HWVANNHWNPW_95_2_L1AD", "HWVANNHWNPW_95_3_L1AD", "HWVANNHWNPW_95_4_L1AD",
#   "HWVANNHWNPW_97_2_L1AD", "HWVANNHWNPW_97_3_L1AD", "HWVANNHWNPW_97_4_L1AD")

hw_vars <- c("HWN_EHF", "HWN_90P_2d_L1AD_tpw", "HWN_90P_3d_L1AD_tpw", "HWN_90P_4d_L1AD_tpw",
             "HWN_95P_2d_L1AD_tpw", "HWN_95P_3d_L1AD_tpw", "HWN_95P_4d_L1AD_tpw",
             "HWN_97.5P_2d_L1AD_tpw", "HWN_97.5P_3d_L1AD_tpw", "HWN_97.5P_4d_L1AD_tpw")

# loop para rodar modelos
model_list <- lapply(hw_vars, function(var) {
  formula_mod <- reformulate("YEAR_dec + (1 | Country/SALID1)", response = var)
  glmmTMB(formula_mod, data = data, family = nbinom2(link = "log"))
})
names(model_list) <- hw_vars

# Extract results
results <- lapply(names(model_list), function(name) {
  mod <- model_list[[name]]
  
  # Coef (fixed YEAR_dec)
  coefs <- broom.mixed::tidy(mod, effects = "fixed", conf.int = TRUE)
  year_row <- coefs[coefs$term == "YEAR_dec", ]
  year_row$variable <- name
  
  # ICC
  icc_val <- tryCatch({
    performance::icc(mod)$ICC_adjusted
  }, error = function(e) NA)
  
  year_row$ICC <- icc_val
  year_row
})

results_df <- do.call(rbind, results)

results_df <- results_df %>%
  mutate(
    perc_change = (exp(estimate) - 1) * 100,
    perc_low = (exp(conf.low) - 1) * 100,
    perc_high = (exp(conf.high) - 1) * 100
  ) %>%
  select(variable, estimate, conf.low, conf.high, std.error, p.value,
         perc_change, perc_low, perc_high, ICC)

print(results_df, digits = 3)

# check 
# model_YEAR_HWN_97.5P_4d_L1AD_tpw <- glmmTMB(HWN_97.5P_4d_L1AD_tpw ~ YEAR_dec + (1 | Country/SALID1), 
#                                 data = data, 
#                                 family = nbinom2(link = "log"))
# 
# fixef(model_YEAR_HWN_97.5P_4d_L1AD_tpw)$cond["YEAR_dec"] # 0.9008551
# tab_model(model_YEAR_HWN_97.5P_4d_L1AD_tpw)

# residuals
# 90P
simulation_output <- simulateResiduals(model_list[["HWN_90P_2d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWN_90P_3d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWN_90P_4d_L1AD_tpw"]]) %>%  plot()

#95P
simulation_output <- simulateResiduals(model_list[["HWN_95P_2d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWN_95P_3d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWN_95P_4d_L1AD_tpw"]]) %>%  plot()

#97.5
simulation_output <- simulateResiduals(model_list[["HWN_97.5P_2d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWN_97.5P_3d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWN_97.5P_4d_L1AD_tpw"]]) %>%  plot()

# export
write_xlsx(results_df, "HWN_trend_results.xlsx")

#---------------------------------------------------- HWF ----------------------------------------------------------------------------------------

# ------------------------------------------
# HWF - YEAR as linear
# ------------------------------------------

data3 <- read.csv("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/HWF_L1AD.csv")

str(data3)

data_hwpf <- data3 %>% select(SALID1, year,
                              HWF_90P_2d_L1AD_tpw, HWF_90P_3d_L1AD_tpw, HWF_90P_4d_L1AD_tpw,
                              HWF_95P_2d_L1AD_tpw, HWF_95P_3d_L1AD_tpw, HWF_95P_4d_L1AD_tpw,
                              HWF_97.5P_2d_L1AD_tpw, HWF_97.5P_3d_L1AD_tpw, HWF_97.5P_4d_L1AD_tpw)

data <- data %>% left_join(data_hwpf, by = c("SALID1" = "SALID1", "YEAR" = "year"))



hwf_vars <- c("HWF_EHF", "HWF_90P_2d_L1AD_tpw", "HWF_90P_3d_L1AD_tpw", "HWF_90P_4d_L1AD_tpw",
              "HWF_95P_2d_L1AD_tpw", "HWF_95P_3d_L1AD_tpw", "HWF_95P_4d_L1AD_tpw",
              "HWF_97.5P_2d_L1AD_tpw", "HWF_97.5P_3d_L1AD_tpw", "HWF_97.5P_4d_L1AD_tpw")

# loop para rodar modelos
model_list_f <- lapply(hwf_vars, function(var) {
  formula_mod <- reformulate("YEAR_dec + (1 | Country/SALID1)", response = var)
  glmmTMB(formula_mod, data = data, family = nbinom2(link = "log"))
})
names(model_list_f) <- hwf_vars

# Extract results
results_f <- lapply(names(model_list_f), function(name) {
  mod <- model_list_f[[name]]
  
  # Coef (fixed YEAR_dec)
  coefs <- broom.mixed::tidy(mod, effects = "fixed", conf.int = TRUE)
  year_row <- coefs[coefs$term == "YEAR_dec", ]
  year_row$variable <- name
  
  # ICC
  icc_val <- tryCatch({
    performance::icc(mod)$ICC_adjusted
  }, error = function(e) NA)
  
  year_row$ICC <- icc_val
  year_row
})

results_df_f <- do.call(rbind, results_f)

results_df_f <- results_df_f %>%
  mutate(
    perc_change = (exp(estimate) - 1) * 100,
    perc_low = (exp(conf.low) - 1) * 100,
    perc_high = (exp(conf.high) - 1) * 100
  ) %>%
  select(variable, estimate, conf.low, conf.high, std.error, p.value,
         perc_change, perc_low, perc_high, ICC)

print(results_df_f, digits = 3)

# check 
# model_YEAR_HWF_97.5P_4d_L1AD_tpw <- glmmTMB(HWF_97.5P_4d_L1AD_tpw ~ YEAR_dec + (1 | Country/SALID1),
#                                 data = data,
#                                 family = nbinom2(link = "log"))
# 
# fixef(model_YEAR_HWF_97.5P_4d_L1AD_tpw)$cond["YEAR_dec"] #1.018926 
# tab_model(model_YEAR_HWF_97.5P_4d_L1AD_tpw)

# residuals
# 90P
simulation_output <- simulateResiduals(model_list[["HWF_90P_2d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWF_90P_3d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWF_90P_4d_L1AD_tpw"]]) %>%  plot()

#95P
simulation_output <- simulateResiduals(model_list[["HWF_95P_2d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWF_95P_3d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWF_95P_4d_L1AD_tpw"]]) %>%  plot()

#97.5
simulation_output <- simulateResiduals(model_list[["HWF_97.5P_2d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWF_97.5P_3d_L1AD_tpw"]]) %>%  plot()
simulation_output <- simulateResiduals(model_list[["HWF_97.5P_4d_L1AD_tpw"]]) %>%  plot()

# export
write_xlsx(results_df_f, "HWF_trend_results.xlsx")
