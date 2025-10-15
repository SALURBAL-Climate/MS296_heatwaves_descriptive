##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# This script fits multilevel models for NHW and the HW
# 
# Sara Lopes de Moraes
# Last updated: July 2025
##################################################################

library(dplyr); library(glmmTMB); library(arrow)
library(tidyr); library(tidyverse); library(lme4)
library(ggplot2); library(ggeffects); library(patchwork)
library(performance); library(DHARMa); library(broom.mixed)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Model")

data <- read_parquet("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/data_final.parquet")

str(data)

data$SALID1 <- as.factor(data$SALID1)
data$Country <- as.factor(data$Country)


#---------------------------------------------
# HWN - number of HW
#---------------------------------------------
# hist
hist(data$HWN_EHF)

#Check zero
table(data$HWN_EHF == 0)

mean(data$HWN_EHF)
var(data$HWN_EHF)

# ggplot(data, aes(x = HWN_EHF)) +
#   geom_histogram(aes(y = after_stat(density)), binwidth = 1) +
#   geom_line(aes(y = dpois(HWN_EHF, lambda = mean(HWN_EHF))),
#             color = "red", linewidth = 1) +
#   labs(title = "",
#        x = "NHW", y = "Density")
# 
# 
# ggplot(data = data[data$SALID1 %in% unique(data$SALID1)[221:240], ],
#        aes(x = YEAR, y = HWN_EHF)) +
#   geom_point(alpha = 0.6) +
#   stat_smooth(method = "glm", method.args = list(family = "poisson"), se = FALSE, color = "blue") +
#   facet_wrap(~ SALID1) +
#   labs(x = "Ano", y = "NHW",
#        title = "Tendency by city (model Poisson)") +
#   theme_minimal()


#---  Fit distribution
# 1. Cities - Poisson distribution
model_null_poisson <- glmmTMB(HWN_EHF ~ 1 + (1 | SALID1), 
                              data = data, 
                              family = nbinom2(link = "log"))
summary(model_null_poisson)

check_overdispersion(model_null_poisson)
check_zeroinflation(model_null_poisson)
simulation_output <- simulateResiduals(model_null_poisson)  %>%  plot()
# DHARMa::testZeroInflation(model_null_poisson)



# 2. Cities - Negative binomial
model_null_nb <- glmmTMB(HWN_EHF ~ 1 + (1 | SALID1), 
                         data = data, 
                         family = nbinom2(link = "log"))
summary (model_null_nb)

check_overdispersion(model_null_nb) 
check_zeroinflation(model_null_nb)
simulateResiduals(fittedModel = model_null_nb) %>%  plot()
DHARMa::testZeroInflation(model_null_nb)

# res <- simulateResiduals(fittedModel = model_null_nb)
# testOutliers(res, type = "bootstrap")


# 3. Country/cities - Negative binomial
model_null_nb_c <- glmmTMB(HWN_EHF ~ 1 + (1 | Country/SALID1), 
                           data = data, 
                           family = nbinom2(link = "log"))
summary(model_null_nb_c)

check_overdispersion(model_null_nb_c) 
check_zeroinflation(model_null_nb_c)
simulateResiduals(fittedModel = model_null_nb_c) %>%  plot()
DHARMa::testZeroInflation(model_null_nb_c)

# 4. Compare models
AIC(model_null_nb, model_null_nb_c)

compare_performance(model_null_nb, model_null_nb_c, rank = TRUE)

anova(model_null_nb, model_null_nb_c, test = "LRT")

# Best nb Countries/cities

#---------------------------------------------
# HWF frequency - heatwave days
#---------------------------------------------

# hist
hist(data$HWF_EHF)

#Check zero
table(data$HWF_EHF == 0)

mean(data$HWF_EHF) 
var(data$HWF_EHF)


# ggplot(data, aes(x = HWF_EHF)) +
#   geom_histogram(aes(y = after_stat(density)), binwidth = 1) +
#   geom_line(aes(y = dpois(HWF_EHF, lambda = mean(HWF_EHF))),
#             color = "red", linewidth = 1) +
#   labs(title = "",
#        x = "NHW", y = "Density")
# 
# 
# ggplot(data = data[data$SALID1 %in% unique(data$SALID1)[221:240], ],
#        aes(x = YEAR, y = HWF_EHF)) +
#   geom_point(alpha = 0.6) +
#   stat_smooth(method = "glm", method.args = list(family = "poisson"), se = FALSE, color = "blue") +
#   facet_wrap(~ SALID1) +
#   labs(x = "Ano", y = "NHW",
#        title = "Tendency by city (model Poisson)") +
#   theme_minimal()


#---  Fit distribution
# 1. Cities - Poisson distribution
model_null_poisson_F <- glmmTMB(HWF_EHF ~ 1 + (1 | SALID1), 
                                data = data, 
                                family = nbinom2(link = "log"))
summary(model_null_poisson_F)

check_overdispersion(model_null_poisson_F)
check_zeroinflation(model_null_poisson_F)
simulation_output <- simulateResiduals(model_null_poisson_F)  %>%  plot()
# DHARMa::testZeroInflation(model_null_poisson_F)


# 2. Cities - Negative binomial
model_null_nb_F <- glmmTMB(HWF_EHF ~ 1 + (1 | SALID1), 
                           data = data, 
                           family = nbinom2(link = "log"))
summary(model_null_nb_F)

check_overdispersion(model_null_nb_F) 
check_zeroinflation(model_null_nb_F)
simulateResiduals(fittedModel = model_null_nb_F) %>%  plot()
DHARMa::testZeroInflation(model_null_nb_F)

# res <- simulateResiduals(fittedModel = model_null_nb_F)
# testOutliers(res, type = "bootstrap")


# 3. Country/cities - Negative binomial
model_null_nb_c_F <- glmmTMB(HWF_EHF ~ 1 + (1 | Country/SALID1), 
                             data = data, 
                             family = nbinom2(link = "log"))
summary(model_null_nb_c_F)

check_overdispersion(model_null_nb_c_F) 
check_zeroinflation(model_null_nb_c_F)
simulateResiduals(fittedModel = model_null_nb_c_F) %>%  plot()
DHARMa::testZeroInflation(model_null_nb_c_F)


# 4. Compare models
AIC(model_null_nb_F, model_null_nb_c_F)

compare_performance(model_null_nb_F, model_null_nb_c_F, rank = TRUE)

anova(model_null_nb_F, model_null_nb_c_F, test = "LRT")

# Best model is the nb countries/cities

# tryig nb1
model_null_nb_c_F1 <- glmmTMB(HWF_EHF ~ 1 + (1 | Country/SALID1), 
                              data = data, 
                              family = nbinom2(link = "log"))
summary(model_null_nb_c_F1)
simulateResiduals(fittedModel = model_null_nb_c_F1) %>%  plot()

# better fit nb2