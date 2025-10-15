##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# This script multilevel models for NHW - trends
# 
# Sara Lopes de Moraes
# Last updated: Oct 2025
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
                           family = nbinom2(link = "log"))

summary(model_null_city)

check_overdispersion(model_null_city)
check_zeroinflation(model_null_city)
simulation_output <- simulateResiduals(model_null_city)  %>%  plot()
DHARMa::testZeroInflation(model_null_city)

VarCorr(model_null_city)
print(VarCorr(model_null_city), comp="Variance")

# 2. Cities + Country 
model_null_city_country <- glmmTMB(HWN_EHF ~ 1 + (1 | Country/SALID1), 
                                   data = data, 
                                   family = nbinom2(link = "log"))

summary(model_null_city_country)

check_overdispersion(model_null_city_country)
check_zeroinflation(model_null_city_country)
simulation_output <- simulateResiduals(model_null_city_country)  %>%  plot()
DHARMa::testZeroInflation(model_null_city_country)

VarCorr(model_null_city_country)
print(VarCorr(model_null_city_country), comp="Variance")


# 3. Compare models
AIC(model_null_city, model_null_city_country)
compare_performance(model_null_city, model_null_city_country)
anova(model_null_city, model_null_city_country, test = "LRT")


# best model is with cities and countries in terms of AIC
tab_model(model_null_city_country)

# ------------------------------------------
# HWN - add YEAR as linear 
# ------------------------------------------
# 1. Transform year to decade
data <- data %>%
  mutate(YEAR_dec = (YEAR - 2000) / 10)

# 2. Model
model_YEAR_fixed_dec <- glmmTMB(HWN_EHF ~ YEAR_dec + (1 | Country/SALID1), 
                                data = data, 
                                family = nbinom2(link = "log"))
summary(model_YEAR_fixed_dec)
fixef(model_YEAR_fixed_dec)$cond["YEAR_dec"] # 0.4455813

#pct
coef_year <- fixef(model_YEAR_fixed_dec)$cond["YEAR_dec"]
percent_increase <- (exp(coef_year) - 1) * 100
round(percent_increase, 2)
model_YEAR_fixed_dec
# YEAR_dec 
# 56.14 increase

check_overdispersion(model_YEAR_fixed_dec)
check_zeroinflation(model_YEAR_fixed_dec)
simulation_output <- simulateResiduals(model_YEAR_fixed_dec)  %>%  plot()
DHARMa::testZeroInflation(model_YEAR_fixed_dec)

icc_vals <- performance::icc(model_YEAR_fixed_dec)
print(icc_vals)

report(model_YEAR_fixed_dec)

tab_model(model_YEAR_fixed_dec)
# 56.1% increase

# ------------------------------------------------
# HWN - YEAR - random slope
# ------------------------------------------------
# 1. YEAR - random slope
model_rs_YEAR_dec <- glmmTMB(HWN_EHF ~ YEAR_dec + (YEAR_dec | Country/SALID1), 
                             data = data, 
                             family = nbinom2(link = "log"))

summary(model_rs_YEAR_dec)
summary(model_rs_YEAR_dec)$varcor
as.data.frame(VarCorr(model_rs_YEAR_dec)$cond)

check_overdispersion(model_rs_YEAR_dec)
check_zeroinflation(model_rs_YEAR_dec)
simulation_output <- simulateResiduals(model_rs_YEAR_dec)  %>%  plot()

report(model_rs_YEAR_dec)

tab_model(model_rs_YEAR_dec)

#random effects
ranef(model_rs_YEAR_dec) # shows how each city or country deviates from the mean

# coef (fixed + random effects)
coef(model_rs_YEAR_dec) # shows the final intercepts and slopes for each city or country

city_slopes <- coef(model_rs_YEAR_dec)$cond$`SALID1:Country` %>%
  as.data.frame() %>%
  tibble::rownames_to_column("City") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_dec)

country_slopes <- coef(model_rs_YEAR_dec)$cond$Country %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Country") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_dec)

# Plot Obs vs pred 
pred_city <- data %>%
  distinct(Country, SALID1) %>%
  crossing(YEAR_dec = seq(0, 2.4, by = 0.1)) %>%   
  mutate(YEAR = YEAR_dec * 10 + 2000)

pred_link <- predict(model_rs_YEAR_dec, newdata = pred_city,
                     type = "link", se.fit = TRUE)

pred_city <- pred_city %>%
  mutate(
    fit_link = pred_link$fit,
    se_link  = pred_link$se.fit,
    lower = exp(fit_link - 1.96 * se_link),
    upper = exp(fit_link + 1.96 * se_link),
    pred  = exp(fit_link))

obs_data <- data %>%
  select(Country, SALID1, YEAR, HWN_EHF)

plot_data <- left_join(pred_city, obs_data, by = c("Country", "SALID1", "YEAR"))

plot_data <- plot_data %>%
  group_by(Country, SALID1) %>%
  mutate(city_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(city_group = (city_id - 1) %/% 20 + 1)


# Loop to plot and save
if (!dir.exists("plots_city_slope_HWN")) dir.create("plots_city_slope_HWN")

countries <- unique(plot_data$Country)

for (c in countries) {
  max_group <- max(filter(plot_data, Country == c)$city_group)
  
  for (g in 1:max_group) {
    sub_data <- filter(plot_data, Country == c, city_group == g)
    
    if (nrow(sub_data) == 0) next
    
    p <- ggplot(sub_data, aes(x = YEAR)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +   # IC95%
      geom_line(aes(y = pred, color = "Predicted"), size = 0.8) +
      geom_point(aes(y = HWN_EHF, color = "Observed"), alpha = 0.6, size = 1.2) +
      facet_wrap(~ SALID1, scales = "free_y") +
      theme_minimal() +
      labs(y = "HWN",
           color = NULL,
           title = paste(c, "- Observed vs Predicted (Cities group", g, ")")) +
      scale_color_manual(values = c("Observed" = "black", "Predicted" = "red")) +
      theme(legend.position = "bottom")
    
    ggsave(filename = paste0("plots_city_slope_HWN/plot_", c, "_group", g, ".png"),
           plot = p, width = 12, height = 8, bg = "white")}}


#---------------------------------------------------- HWF ----------------------------------------------------------------------------------------

# ------------------------------------------
# HWF - Null models
# ------------------------------------------
# 1. cities
model_null_city_F <- glmmTMB(HWF_EHF ~ 1 + (1 | SALID1), 
                             data = data, 
                             family = nbinom2(link = "log"))

summary(model_null_city_F)

check_overdispersion(model_null_city_F)
check_zeroinflation(model_null_city_F)
simulation_output <- simulateResiduals(model_null_city_F)  %>%  plot()
DHARMa::testZeroInflation(model_null_city_F)

VarCorr(model_null_city_F)
print(VarCorr(model_null_city_F), comp="Variance")

# 2. Cities + Country 
model_null_city_country_F <- glmmTMB(HWF_EHF ~ 1 + (1 | Country/SALID1), 
                                     data = data, 
                                     family = nbinom2(link = "log"))
summary(model_null_city_country_F)

check_overdispersion(model_null_city_country_F)
check_zeroinflation(model_null_city_country_F)
simulation_output <- simulateResiduals(model_null_city_country_F)  %>%  plot()
DHARMa::testZeroInflation(model_null_city_country_F)

VarCorr(model_null_city_country_F)
print(VarCorr(model_null_city_country_F), comp="Variance")


# 3. Compare models
AIC(model_null_city_F, model_null_city_country_F)
compare_performance(model_null_city_F, model_null_city_country_F)
anova(model_null_city_F, model_null_city_country_F, test = "LRT")


# best model is with cities and countries in terms of AIC
tab_model(model_null_city_country_F)

# ------------------------------------------
# HWN - add YEAR as linear 
# ------------------------------------------
# 1. Model with linear trend (YEAR fixed effect)
model_YEAR_fixed_dec_F <- glmmTMB(HWF_EHF ~ YEAR_dec + (1 | Country/SALID1), 
                                  data = data, 
                                  family = nbinom2(link = "log"))
summary(model_YEAR_fixed_dec_F)
fixef(model_YEAR_fixed_dec_F)$cond["YEAR_dec"]
# 0.6005877

check_overdispersion(model_YEAR_fixed_dec_F)
check_zeroinflation(model_YEAR_fixed_dec_F)
simulation_output <- simulateResiduals(model_YEAR_fixed_dec_F)  %>%  plot()
DHARMa::testZeroInflation(model_YEAR_fixed_dec_F)

icc_vals <- performance::icc(model_YEAR_fixed_dec_F)
print(icc_vals)

report(model_YEAR_fixed_dec_F)
tab_model(model_YEAR_fixed_dec_F)

# ------------------------------------------------
# HWN - YEAR - random slope
# ------------------------------------------------
# 1. Year random slope - DECADE
model_rs_YEAR_dec_F <- glmmTMB(HWF_EHF ~ YEAR_dec + (YEAR_dec | Country/SALID1), 
                               data = data, 
                               family = nbinom2(link = "log"))

summary(model_rs_YEAR_dec_F)
summary(model_rs_YEAR_dec_F)$varcor
as.data.frame(VarCorr(model_rs_YEAR_dec_F)$cond)

check_overdispersion(model_rs_YEAR_dec_F)
check_zeroinflation(model_rs_YEAR_dec_F)
simulation_output <- simulateResiduals(model_rs_YEAR_dec_F)  %>%  plot()

report(model_rs_YEAR_dec_F)

tab_model(model_rs_YEAR_dec_F)

# tab_model(model_rs_YEAR_dec_F, 
#           show.est = TRUE, 
#           show.se = TRUE,
#           transform = "exp")

#random effects
ranef(model_rs_YEAR_dec_F) 

# coef (fixed + random effects)
coef(model_rs_YEAR_dec_F)

city_slopes_F <- coef(model_rs_YEAR_dec_F)$cond$`SALID1:Country` %>%
  as.data.frame() %>%
  tibble::rownames_to_column("City") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_dec)

country_slopes_F <- coef(model_rs_YEAR_dec_F)$cond$Country %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Country") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_dec)

# Plot Obs vs pred
pred_city_F <- data %>%
  distinct(Country, SALID1) %>%
  crossing(YEAR_dec = seq(0, 2.4, by = 0.1)) %>%   
  mutate(YEAR = YEAR_dec * 10 + 2000)

pred_link_F <- predict(model_rs_YEAR_dec_F, newdata = pred_city_F,
                     type = "link", se.fit = TRUE)

pred_city_F <- pred_city_F %>%
  mutate(
    fit_link = pred_link_F$fit,
    se_link  = pred_link_F$se.fit,
    lower = exp(fit_link - 1.96 * se_link),
    upper = exp(fit_link + 1.96 * se_link),
    pred  = exp(fit_link))

obs_data_F <- data %>%
  select(Country, SALID1, YEAR, HWF_EHF)

plot_data_F <- left_join(pred_city_F, obs_data_F, by = c("Country", "SALID1", "YEAR"))

plot_data_F <- plot_data_F %>%
  group_by(Country, SALID1) %>%
  mutate(city_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(city_group = (city_id - 1) %/% 20 + 1)


# Loop to plot and save
if (!dir.exists("plots_city_slope_HWF")) dir.create("plots_city_slope_HWF")

countries <- unique(plot_data_F$Country)

for (c in countries) {
  max_group <- max(filter(plot_data_F, Country == c)$city_group)
  
  for (g in 1:max_group) {
    sub_data <- filter(plot_data_F, Country == c, city_group == g)
    
    if (nrow(sub_data) == 0) next
    
    p <- ggplot(sub_data, aes(x = YEAR)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.2) + 
      geom_line(aes(y = pred, color = "Predicted"), size = 0.8) +
      geom_point(aes(y = HWF_EHF, color = "Observed"), alpha = 0.6, size = 1.2) +
      facet_wrap(~ SALID1, scales = "free_y") +
      theme_minimal() +
      labs(y = "HWF",
           color = NULL,
           title = paste(c, "- Observed vs Predicted (Cities group", g, ")")) +
      scale_color_manual(values = c("Observed" = "black", "Predicted" = "red")) +
      theme(legend.position = "bottom")
    
    ggsave(filename = paste0("plots_city_slope_HWF/plot_", c, "_group", g, ".png"),
           plot = p, width = 12, height = 8, bg = "white")}}
