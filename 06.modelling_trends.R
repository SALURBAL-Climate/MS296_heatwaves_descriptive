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
                           nbinom2(link = "log"))

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
                                   nbinom2(link = "log"))

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
# 1. Model with linear trend (YEAR fixed effect)
# center the YEAR (2000 as reference)
data <- data %>% mutate(YEAR_c = YEAR - 2000)

model_YEAR_fixed_center <- glmmTMB(HWN_EHF ~ YEAR_c + (1 | Country/SALID1), 
                                   data = data, 
                                   nbinom2(link = "log"))
summary(model_YEAR_fixed_center)
fixef(model_YEAR_fixed_center)$cond["YEAR_c"]
# 4.4% increase per year

check_overdispersion(model_YEAR_fixed_center)
check_zeroinflation(model_YEAR_fixed_center)
simulation_output <- simulateResiduals(model_YEAR_fixed_center)  %>%  plot()
DHARMa::testZeroInflation(model_YEAR_fixed_center)

# 2. Transform year to decade
data <- data %>%
  mutate(YEAR_dec = (YEAR - 2000) / 10)

model_YEAR_fixed_dec <- glmmTMB(HWN_EHF ~ YEAR_dec + (1 | Country/SALID1), 
                                   data = data, 
                                   nbinom2(link = "log"))
summary(model_YEAR_fixed_dec)
fixef(model_YEAR_fixed_dec)$cond["YEAR_dec"]
# 0.4455813 = 44%

check_overdispersion(model_YEAR_fixed_dec)
check_zeroinflation(model_YEAR_fixed_dec)
simulation_output <- simulateResiduals(model_YEAR_fixed_dec)  %>%  plot()
DHARMa::testZeroInflation(model_YEAR_fixed_dec)

icc_vals <- performance::icc(model_YEAR_fixed_dec)
print(icc_vals)

report(model_YEAR_fixed_dec)
tab_model(model_YEAR_fixed_dec)

# ------------------------------------------------
# HWN - YEAR - random slope
# ------------------------------------------------
# 1. YEAR - random slope
# (YEAR_c| Country/SALID1) same as (1 + YEAR_c).
model_radom_slope_YEAR <- glmmTMB(HWN_EHF ~ YEAR_c + (YEAR_c | Country/SALID1), 
                                  data = data, 
                                  nbinom2(link = "log"))

summary(model_radom_slope_YEAR)
summary(model_radom_slope_YEAR)$varcor
as.data.frame(VarCorr(model_radom_slope_YEAR)$cond)

check_overdispersion(model_radom_slope_YEAR)
check_zeroinflation(model_radom_slope_YEAR)
simulation_output <- simulateResiduals(model_radom_slope_YEAR)  %>%  plot()

report(model_radom_slope_YEAR)

tab_model(model_radom_slope_YEAR)

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
# Pred CI95%
pred_city <- data %>%
  distinct(Country, SALID1) %>%
  crossing(YEAR_c = 0:24) %>%    # anos 2000–2024 centralizados
  mutate(YEAR = YEAR_c + 2000)

pred_city <- pred_city %>%
  mutate(
    pred = predict(model_radom_slope_YEAR, newdata = ., type = "response"),
    pred_se = predict(model_radom_slope_YEAR, newdata = ., type = "link", se.fit = TRUE)$se.fit)

pred_link <- predict(model_radom_slope_YEAR, newdata = pred_city, type = "link", se.fit = TRUE)

pred_city <- pred_city %>%
  mutate(
    fit_link = pred_link$fit,
    se_link  = pred_link$se.fit,
    lower = exp(fit_link - 1.96 * se_link),
    upper = exp(fit_link + 1.96 * se_link))

obs_data <- data %>% select(Country, SALID1, YEAR, HWN_EHF)

plot_data <- left_join(pred_city, obs_data, by = c("Country", "SALID1", "YEAR"))

plot_data <- plot_data %>%
  group_by(Country, SALID1) %>%
  mutate(city_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(city_group = (city_id - 1) %/% 20 + 1)


# Loop para salvar gráficos
if (!dir.exists("plots_city_slope")) dir.create("plots_city_slope")

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
           title = paste(c, "- Observed vs Predicted (Cities group", g, ")")) +
      scale_color_manual(values = c("Observed" = "black", "Predicted" = "red")) +
      theme(legend.position = "bottom")
    
    ggsave(filename = paste0("plots_city_slope/plot_", c, "_group", g, ".png"),
           plot = p, width = 12, height = 8, bg = "white")}}

# 2. Year random slope - DECADE
model_rs_YEAR_dec <- glmmTMB(HWN_EHF ~ YEAR_dec + (YEAR_dec | Country/SALID1), 
                                  data = data, 
                                  nbinom2(link = "log"))

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


# 3. Compare models
AIC(model_YEAR_fixed_center, model_radom_slope_YEAR)
anova(model_YEAR_fixed_center, model_radom_slope_YEAR, test = "LRT")


#---------------------------------------------------- HWF ----------------------------------------------------------------------------------------

# ------------------------------------------
# HWF - Null models
# ------------------------------------------
# 1. cities
model_null_city_F <- glmmTMB(HWF_EHF ~ 1 + (1 | SALID1), 
                             data = data, 
                             nbinom2(link = "log"))

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
                                     nbinom2(link = "log"))

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
# center the YEAR (2000 as reference)
data <- data %>% mutate(YEAR_c = YEAR - 2000)

model_YEAR_fixed_center_F <- glmmTMB(HWF_EHF ~ YEAR_c + (1 | Country/SALID1), 
                                     data = data, 
                                     nbinom2(link = "log"))
summary(model_YEAR_fixed_center_F)
fixef(model_YEAR_fixed_center_F)$cond["YEAR_c"]
# 4.4% increase per year

check_overdispersion(model_YEAR_fixed_center_F)
check_zeroinflation(model_YEAR_fixed_center_F)
simulation_output <- simulateResiduals(model_YEAR_fixed_center_F)  %>%  plot()
DHARMa::testZeroInflation(model_YEAR_fixed_center_F)

# 2. Transform year to decade
data <- data %>%
  mutate(YEAR_dec = (YEAR - 2000) / 10)

model_YEAR_fixed_dec_F <- glmmTMB(HWF_EHF ~ YEAR_dec + (1 | Country/SALID1), 
                                  data = data, 
                                  nbinom2(link = "log"))
summary(model_YEAR_fixed_dec_F)
fixef(model_YEAR_fixed_dec_F)$cond["YEAR_dec"]
# 0.4455813 = 44%

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
# 1. YEAR - random slope
# (YEAR_c| Country/SALID1) same as (1 + YEAR_c).
model_radom_slope_YEAR_F <- glmmTMB(HWF_EHF ~ YEAR_c + (YEAR_c | Country/SALID1), 
                                    data = data, 
                                    nbinom2(link = "log"))

summary(model_radom_slope_YEAR_F)
summary(model_radom_slope_YEAR_F)$varcor
as.data.frame(VarCorr(model_radom_slope_YEAR_F)$cond)

check_overdispersion(model_radom_slope_YEAR_F)
check_zeroinflation(model_radom_slope_YEAR_F)
simulation_output <- simulateResiduals(model_radom_slope_YEAR_F)  %>%  plot()

report(model_radom_slope_YEAR_F)

tab_model(model_radom_slope_YEAR_F)

#random effects
ranef(model_radom_slope_YEAR_F) # shows how each city or country deviates from the mean

# coef (fixed + random effects)
coef(model_radom_slope_YEAR_F) # shows the final intercepts and slopes for each city or country

city_slopes <- coef(model_radom_slope_YEAR_F)$cond$`SALID1:Country` %>%
  as.data.frame() %>%
  tibble::rownames_to_column("City") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_c)

country_slopes <- coef(model_radom_slope_YEAR_F)$cond$Country %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Country") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_c)

# Plots
# Pred CI95%
pred_city <- data %>%
  distinct(Country, SALID1) %>%
  crossing(YEAR_c = 0:24) %>%    # anos 2000–2024 centralizados
  mutate(YEAR = YEAR_c + 2000)

pred_city <- pred_city %>%
  mutate(
    pred = predict(model_radom_slope_YEAR_F, newdata = ., type = "response"),
    pred_se = predict(model_radom_slope_YEAR_F, newdata = ., type = "link", se.fit = TRUE)$se.fit)

pred_link <- predict(model_radom_slope_YEAR_F, newdata = pred_city, type = "link", se.fit = TRUE)

pred_city <- pred_city %>%
  mutate(
    fit_link = pred_link$fit,
    se_link  = pred_link$se.fit,
    lower = exp(fit_link - 1.96 * se_link),
    upper = exp(fit_link + 1.96 * se_link))

obs_data <- data %>% select(Country, SALID1, YEAR, HWF_EHF)

plot_data <- left_join(pred_city, obs_data, by = c("Country", "SALID1", "YEAR"))

plot_data <- plot_data %>%
  group_by(Country, SALID1) %>%
  mutate(city_id = cur_group_id()) %>%
  ungroup() %>%
  mutate(city_group = (city_id - 1) %/% 20 + 1)


# Loop para salvar gráficos
if (!dir.exists("plots_city_slope_HWF")) dir.create("plots_city_slope_HWF")

countries <- unique(plot_data$Country)

for (c in countries) {
  max_group <- max(filter(plot_data, Country == c)$city_group)
  
  for (g in 1:max_group) {
    sub_data <- filter(plot_data, Country == c, city_group == g)
    
    if (nrow(sub_data) == 0) next
    
    p <- ggplot(sub_data, aes(x = YEAR)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +   # IC95%
      geom_line(aes(y = pred, color = "Predicted"), size = 0.8) +
      geom_point(aes(y = HWF_EHF, color = "Observed"), alpha = 0.6, size = 1.2) +
      facet_wrap(~ SALID1, scales = "free_y") +
      theme_minimal() +
      labs(y = "HWN",
           title = paste(c, "- Observed vs Predicted (Cities group", g, ")")) +
      scale_color_manual(values = c("Observed" = "black", "Predicted" = "red")) +
      theme(legend.position = "bottom")
    
    ggsave(filename = paste0("plots_city_slope_HWF/plot_", c, "_group", g, ".png"),
           plot = p, width = 12, height = 8, bg = "white")}}

# 2. Year random slope - DECADE
model_rs_YEAR_dec_F <- glmmTMB(HWF_EHF ~ YEAR_dec + (YEAR_dec | Country/SALID1), 
                               data = data, 
                               nbinom2(link = "log"))

summary(model_rs_YEAR_dec_F)
summary(model_rs_YEAR_dec_F)$varcor
as.data.frame(VarCorr(model_rs_YEAR_dec_F)$cond)

check_overdispersion(model_rs_YEAR_dec_F)
check_zeroinflation(model_rs_YEAR_dec_F)
simulation_output <- simulateResiduals(model_rs_YEAR_dec_F)  %>%  plot()

report(model_rs_YEAR_dec_F)

tab_model(model_rs_YEAR_dec_F)

#random effects
ranef(model_rs_YEAR_dec_F) # shows how each city or country deviates from the mean

# coef (fixed + random effects)
coef(model_rs_YEAR_dec_F) # shows the final intercepts and slopes for each city or country

city_slopes <- coef(model_rs_YEAR_dec_F)$cond$`SALID1:Country` %>%
  as.data.frame() %>%
  tibble::rownames_to_column("City") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_dec)

country_slopes <- coef(model_rs_YEAR_dec_F)$cond$Country %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Country") %>%
  dplyr::rename(intercept = `(Intercept)`,
                slope = YEAR_dec)


# 3. Compare models
AIC(model_YEAR_fixed_center_F, model_rs_YEAR_dec_F)
anova(model_YEAR_fixed_center_F, model_rs_YEAR_dec_F, test = "LRT")
