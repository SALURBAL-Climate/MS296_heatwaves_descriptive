##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# This script multilevel models for NHW - trends
# 
# Sara Lopes de Moraes
# Last updated: Sep 2025
##################################################################

library(dplyr); library(glmmTMB); library(arrow)
library(tidyr); library(tidyverse); library(lme4)
library(ggplot2); library(ggeffects); library(patchwork)
library(performance); library(DHARMa); library(broom.mixed)
library(splines); library(broom.mixed); library(report)
library(sjPlot)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Model")

data <- read_parquet("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/data_final.parquet")

length(unique(data$SALID1))

str(data)

data$SALID1 <- as.factor(data$SALID1)
data$Country <- as.factor(data$Country)

data <- data[order(data$SALID1, data$YEAR), ]

# ggplot(data, aes(x = YEAR, y = HWN_EHF)) +
#   geom_point() +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(title = "HWN")
# 
# ggplot(data, aes(x = YEAR, y = HWN_EHF)) +
#   geom_line(aes(group = SALID1), alpha = 0.2) +
#   geom_smooth(aes(group = 1), method = "loess", color = "red") +
#   labs(title = "Tendency HWN",
#        x = "YEAR", y = "HWN_EHF")
# 
# #mean overall
# data %>%
#   group_by(YEAR) %>%
#   summarise(mean_hwn = mean(HWN_EHF, na.rm = TRUE)) %>%
#   ggplot(aes(x = YEAR, y = mean_hwn)) +
#   geom_point() +
#   geom_line() +
#   geom_smooth(method = "loess", se = FALSE, color = "red")


# ------------------------------------------
# HWN - Null models
# ------------------------------------------
# 1. cities
model_null_city <- glmmTMB(HWN_EHF ~ 1 + (1 | SALID1), 
                       data = data, 
                       family = nbinom2,
                       ziformula = ~1)

summary(model_null_city)


check_overdispersion(model_null_city)
check_zeroinflation(model_null_city)
simulation_output <- simulateResiduals(model_null_city)  %>%  plot()
DHARMa::testZeroInflation(model_null_city)

VarCorr(model_null_city)
print(VarCorr(model_null_city), comp="Variance") # 0.009375

tab_model(model_null_city)

# 2. Cities + Country 
model_null_city_country <- glmmTMB(HWN_EHF ~ 1 + (1 | Country/SALID1), 
                           data = data, 
                           family = nbinom2,
                           ziformula = ~1) 

summary(model_null_city_country)

check_overdispersion(model_null_city_country)
check_zeroinflation(model_null_city_country)
simulation_output <- simulateResiduals(model_null_city_country)  %>%  plot()
DHARMa::testZeroInflation(model_null_city_country)

VarCorr(model_null_city_country)
print(VarCorr(model_null_city_country), comp="Variance")

tab_model(model_null_city_country)


# 3. Compare models
AIC(model_null_city, model_null_city_country)
compare_performance(model_null_city, model_null_city_country)
anova(model_null_city, model_null_city_country, test = "LRT")

# best model is with cities and countries in terms of AIC

# ------------------------------------------
# HWN - add YEAR as linear 
# ------------------------------------------
# 1. Model with linear trend (YEAR fixed effect)
# center the YEAR (2000 as reference)
data <- data %>% mutate(YEAR_c = YEAR - 2000)

model_YEAR_fixed_center <- glmmTMB(HWN_EHF ~ YEAR_c + (1 | Country/SALID1), 
                            data = data, 
                            family = nbinom2,
                            ziformula = ~1) 
summary(model_YEAR_fixed_center)

check_overdispersion(model_YEAR_fixed_center)
check_zeroinflation(model_YEAR_fixed_center)
simulation_output <- simulateResiduals(model_YEAR_fixed_center)  %>%  plot()
DHARMa::testZeroInflation(model_YEAR_fixed_center)

icc_vals <- performance::icc(model_YEAR_fixed_center)
print(icc_vals)

report(model_YEAR_fixed_center)

# ------------------------------------------------
# HWN - YEAR - random intercept and random slope
# ------------------------------------------------
# 1. random intercept for YEAR
model_YEAR_random_inter <- glmmTMB(HWN_EHF ~ 1 + (1 | YEAR_c) + (1 | Country/SALID1), 
                                   data = data,
                                   family = nbinom2,
                                   ziformula = ~1)
summary(model_YEAR_random_inter)
check_zeroinflation(model_YEAR_random_inter)
check_overdispersion(model_YEAR_random_inter)
simulation_output <- simulateResiduals(model_YEAR_random_inter)  %>%  plot()

report(model_YEAR_random_inter)


# 2. YEAR - random slope
# (YEAR_c | Country/SALID1) same as (1 + YEAR_c).
model_radom_slope_YEAR <- glmmTMB(HWN_EHF ~ YEAR_c + (YEAR_c | Country/SALID1), 
                                  data = data, 
                                  family = nbinom2,
                                  ziformula = ~1)

summary(model_radom_slope_YEAR)
summary(model_radom_slope_YEAR)$varcor
as.data.frame(VarCorr(model_radom_slope_YEAR)$cond)

check_overdispersion(model_radom_slope_YEAR)
check_zeroinflation(model_radom_slope_YEAR)
simulation_output <- simulateResiduals(model_radom_slope_YEAR)  %>%  plot()

report(model_radom_slope_YEAR)

#random effects
ranef(model_radom_slope_YEAR) # shows how each city or country deviates from the mean

# coef (fixed + random effects)
coef(model_radom_slope_YEAR) # shows the final intercepts and slopes for each city or country


city_slopes <- coef(model_radom_slope_YEAR)$cond$`SALID1:Country` %>%
  as.data.frame() %>%
  tibble::rownames_to_column("City") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_c)

country_slopes <- coef(model_radom_slope_YEAR)$cond$Country %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Country") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_c)

# Plots
# 1. mean plot
data$pred <- predict(model_radom_slope_YEAR, type = "response")

obs_pred_city <- data %>%
  group_by(Country, SALID1) %>%
  summarise(obs = mean(HWN_EHF),
            pred = mean(pred),
            .groups = "drop")


ggplot(obs_pred_city, aes(x = obs, y = pred, label = SALID1, color = Country)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Observed",
       y = "Predicted",
       title = "Observed vs Predicted for each city (mean)")


# 2. Temporal plots for each city
data <- data %>%
  group_by(Country, SALID1) %>%
  mutate(city_id = cur_group_id()) %>%   
  ungroup() %>%
  mutate(city_group = (city_id - 1) %/% 20 + 1)  # 20 cities by group

if (!dir.exists("plots_hw")) dir.create("plots_hw") # folder

# 2.2. Loop para gerar e salvar os plots
countries <- unique(data$Country)

for (c in countries) {
  max_group <- max(filter(data, Country == c)$city_group)
  
  for (g in 1:max_group) {
    sub_data <- filter(data, Country == c, city_group == g)
    
    if (nrow(sub_data) == 0) next  
    
    p <- ggplot(sub_data, aes(x = YEAR)) +
      geom_line(aes(y = pred, color = "Predicted")) +
      geom_line(aes(y = HWN_EHF, color = "Observed"), alpha = 0.7) +
      facet_wrap(~ SALID1, scales = "free_y") +
      theme_minimal() +
      labs(y = "HWN",
           title = paste(c, "- Observed vs Predicted (Cities group", g, ")")) +
      scale_color_manual(values = c("Observed" = "black", "Predicted" = "red")) +
      theme(legend.position = "bottom")
    
    ggsave(filename = paste0("plots_city_slope/plot_", c, "_group", g, ".png"),
           plot = p, width = 12, height = 8, bg = "white")
  }
}


# pred_city <- expand.grid(
#   SALID1 = unique(salid1_re$SALID1),
#   YEAR_c = 0:24  # 2000 a 2024
# )
# 
# # Adicionar intercepto e slope aleatórios
# pred_city <- pred_city %>%
#   left_join(salid1_re, by = "SALID1") %>%
#   mutate(pred = exp(intercept + slope * YEAR_c + fixef(model_radom_slope_YEAR)$cond["(Intercept)"] + fixef(model_radom_slope_YEAR)$cond["YEAR_c"] * YEAR_c))
# 
# # Plot
# ggplot(pred_city, aes(x = YEAR_c + 2000, y = pred, group = SALID1)) +
#   geom_line(alpha = 0.5) +
#   labs(x = "Ano", y = "Previsão HWN_EHF",
#        title = "Tendência por cidade com random slopes") +
#   theme_minimal()

# 3. compare models
AIC(model_YEAR_random_inter, model_radom_slope_YEAR)
anova(model_YEAR_random_inter, model_radom_slope_YEAR, test = "LRT")


#---------------------------------------------------- HWF ----------------------------------------------------------------------------------------

# ------------------------------------------
# HWF - Null models
# ------------------------------------------
# 1. cities
model_null_city_HWF <- glmmTMB(HWF_EHF ~ 1 + (1 | SALID1), 
                               data = data, 
                               family = nbinom2,
                               ziformula = ~1)

summary(model_null_city_HWF)


check_overdispersion(model_null_city_HWF)
check_zeroinflation(model_null_city_HWF)
simulation_output <- simulateResiduals(model_null_city_HWF)  %>%  plot()
DHARMa::testZeroInflation(model_null_city_HWF)

VarCorr(model_null_city_HWF)
print(VarCorr(model_null_city_HWF), comp="Variance") # 7.9719e-10


# 2. Cities + Country 
model_null_city_country_HWF <- glmmTMB(HWF_EHF ~ 1 + (1 | Country/SALID1), 
                                       data = data, 
                                       family = nbinom2,
                                       ziformula = ~1) 

summary(model_null_city_country_HWF)

check_overdispersion(model_null_city_country_HWF)
check_zeroinflation(model_null_city_country_HWF)
simulation_output <- simulateResiduals(model_null_city_country_HWF)  %>%  plot()
DHARMa::testZeroInflation(model_null_city_country_HWF)

VarCorr(model_null_city_country_HWF)
print(VarCorr(model_null_city_country_HWF), comp="Variance")

# 3. Compare models
AIC(model_null_city_HWF, model_null_city_country_HWF)
anova(model_null_city_HWF, model_null_city_country_HWF, test = "LRT")

# best model is with cities and countries in terms of AIC and BIC

# ------------------------------------------
# HWF - add YEAR as linear 
# ------------------------------------------
# 1. fixed year
model_YEAR_fixed_HWF_center <- glmmTMB(HWF_EHF ~ YEAR_c + (1 | Country/SALID1), 
                                       data = data, 
                                       family = nbinom2,
                                       ziformula = ~1) 
summary(model_YEAR_fixed_HWF_center)

check_overdispersion(model_YEAR_fixed_HWF_center)
check_zeroinflation(model_YEAR_fixed_HWF_center)
simulation_output <- simulateResiduals(model_YEAR_fixed_HWF_center)  %>%  plot()
DHARMa::testZeroInflation(model_YEAR_fixed_HWF_center)


# ------------------------------------------------
# HWF - YEAR - random slope and random intercept
# ------------------------------------------------
# 1. random intercept for YEAR
model_YEAR_random_inter_HWF <- glmmTMB(HWF_EHF ~ 1 + (1 | YEAR_c) + (1 | Country/SALID1), 
                                         data = data,
                                         family = nbinom2,
                                         ziformula = ~1) 
summary(model_YEAR_random_inter_HWF)
check_overdispersion(model_YEAR_random_inter_HWF)
check_zeroinflation(model_YEAR_random_inter_HWF)
simulation_output <- simulateResiduals(model_YEAR_random_inter_HWF)  %>%  plot()
DHARMa::testZeroInflation(model_YEAR_random_inter_HWF)


# 2. YEAR - random slope
# (YEAR_c | Country/SALID1) same as (1 + YEAR_c).
model_radom_slope_YEAR_HWF <- glmmTMB(HWF_EHF ~ YEAR_c + (YEAR_c | Country/SALID1), 
                                      data = data, 
                                      family = nbinom2,
                                      ziformula = ~ 1,)

summary(model_radom_slope_YEAR_HWF)

check_overdispersion(model_radom_slope_YEAR_HWF)
check_zeroinflation(model_radom_slope_YEAR_HWF)
simulation_output <- simulateResiduals(model_radom_slope_YEAR_HWF) %>%  plot() 
DHARMa::testZeroInflation(model_radom_slope_YEAR_HWF)


#random effects
ranef(model_radom_slope_YEAR_HWF) # to show how each city or country deviates from the mean

# coef (fixed + random effects)
coef(model_radom_slope_YEAR_HWF) # to show the final intercepts and slopes for each city or country


city_slopes <- coef(model_radom_slope_YEAR_HWF)$cond$`SALID1:Country` %>%
  as.data.frame() %>%
  tibble::rownames_to_column("City") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_c)

country_slopes <- coef(model_radom_slope_YEAR_HWF)$cond$Country %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Country") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_c)

# Plots
# 1. mean plot
data$pred <- predict(model_radom_slope_YEAR_HWF, type = "response")

obs_pred_city <- data %>%
  group_by(Country, SALID1) %>%
  summarise(obs = mean(HWF_EHF),
            pred = mean(pred),
            .groups = "drop")


ggplot(obs_pred_city, aes(x = obs, y = pred, label = SALID1, color = Country)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Observed",
       y = "Predicted",
       title = "Observed vs Predicted for each city (mean)")


# 2. Temporal plots for each city
data <- data %>%
  group_by(Country, SALID1) %>%
  mutate(city_id = cur_group_id()) %>%   
  ungroup() %>%
  mutate(city_group = (city_id - 1) %/% 20 + 1)  # 20 cities by group

if (!dir.exists("plots_HWF")) dir.create("plots_HWF") # folder

# 2.2. Loop para gerar e salvar os plots
countries <- unique(data$Country)

for (c in countries) {
  max_group <- max(filter(data, Country == c)$city_group)
  
  for (g in 1:max_group) {
    sub_data <- filter(data, Country == c, city_group == g)
    
    if (nrow(sub_data) == 0) next  
    
    p <- ggplot(sub_data, aes(x = YEAR)) +
      geom_line(aes(y = pred, color = "Predicted")) +
      geom_line(aes(y = HWF_EHF, color = "Observed"), alpha = 0.7) +
      facet_wrap(~ SALID1, scales = "free_y") +
      theme_minimal() +
      labs(y = "HWF",
           title = paste(c, "- Observed vs Predicted (Cities group", g, ")")) +
      scale_color_manual(values = c("Observed" = "black", "Predicted" = "red")) +
      theme(legend.position = "bottom")
    
    ggsave(filename = paste0("plots_HWF/plot_", c, "_group", g, ".png"),
           plot = p, width = 12, height = 8, bg = "white")
  }
}



# salid1_re <- ranef(model_radom_slope_YEAR_HWF)$cond$`SALID1:Country`
# 
# salid1_re <- salid1_re %>%
#   rownames_to_column("SALID1") %>%
#   rename(intercept = `(Intercept)`, slope = YEAR_c)
# 
# pred_city <- expand.grid(
#   SALID1 = unique(salid1_re$SALID1),
#   YEAR_c = 0:24  # supondo 2000–2024 centrado em 2000
# )
# 
# # juntar com efeitos aleatórios
# pred_city <- pred_city %>%
#   left_join(salid1_re, by = "SALID1") %>%
#   mutate(
#     pred = exp(intercept + slope * YEAR_c +
#                  fixef(model_radom_slope_YEAR_HWF)$cond["(Intercept)"] +
#                  fixef(model_radom_slope_YEAR_HWF)$cond["YEAR_c"] * YEAR_c)
#   )
# 
# ggplot(pred_city, aes(x = YEAR_c + 2000, y = pred, group = SALID1)) +
#   geom_line(alpha = 0.4, color = "steelblue") +
#   labs(
#     x = "Ano",
#     y = "Previsão HWF_EHF",
#     title = "Tendência prevista por cidade (random slopes)"
#   ) +
#   theme_minimal()

