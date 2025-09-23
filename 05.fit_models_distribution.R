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

data <- read_parquet("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/data_hw_pop_ex_INCLUDE_NOUNITCHG_L1AD.parquet")

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
# FALSE  TRUE 
# 8204  1003

mean(data$HWN_EHF) # 3.09
var(data$HWN_EHF) # 5.4


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


#----------------------------- Fit distribution
# Poisson distribution
model_null_poisson <- glmmTMB(HWN_EHF ~ 1 + (1 | SALID1), 
                               data = data, 
                               family = poisson(link = "log"))

check_overdispersion(model_null_poisson)
check_zeroinflation(model_null_poisson)
# simulation_output <- simulateResiduals(model_null_poisson)  %>%  plot()
# DHARMa::testZeroInflation(model_null_poisson)


# Negative binomial
model_null_nb <- glmmTMB(HWN_EHF ~ 1 + (1 | SALID1), 
                                  data = data, 
                                  nbinom2(link = "log"))

check_overdispersion(model_null_nb) # No overdispersion detected.
check_zeroinflation(model_null_nb) # Model seems ok, ratio of observed and predicted zeros is within the tolerance range 
simulateResiduals(fittedModel = model_null_nb) %>%  plot()

res <- simulateResiduals(fittedModel = model_null_nb)
testOutliers(res, type = "bootstrap")


# Negative binomial zero-inflated
model_zinb <- glmmTMB(HWN_EHF ~ 1 + (1 | SALID1), 
                               data = data, 
                               family = nbinom2,
                               ziformula = ~1) 
# family(model_zinb)

check_overdispersion(model_zinb) # No overdispersion detected.
check_zeroinflation(model_zinb) # Model seems ok, ratio of observed and predicted zeros is within the tolerance range 
simulation_output <- simulateResiduals(model_zinb)  %>%  plot()
DHARMa::testZeroInflation(model_zinb)

# summary (model_zinb)
# glance(model_zinb)
# tidy(model_zinb)

# compare models
AIC(model_null_poisson, model_null_nb, model_zinb)
compare_performance(model_null_poisson, model_null_nb, model_zinb, rank = TRUE)

anova(model_null_nb, model_zinb, test = "LRT")

# Best adjustment is the Negative binomial zero-inflate

#---------- cities and countries
# Poisson distribution
model_null_poisson_c <- glmmTMB(HWN_EHF ~ 1 + (1 | Country/SALID1), 
                                data = data, 
                                family = poisson(link = "log"))

# check_overdispersion(model_null_poisson_c)
# check_zeroinflation(model_null_poisson_c)
# simulation_output <- simulateResiduals(model_null_poisson_c)  %>%  plot()
# DHARMa::testZeroInflation(model_null_poisson_c)


# Negative binomial
model_null_nb_c <- glmmTMB(HWN_EHF ~ 1 + (1 | Country/SALID1), 
                           data = data, 
                           nbinom2(link = "log"))

check_overdispersion(model_null_nb_c) #No overdispersion detected.
check_zeroinflation(model_null_nb_c) # Model seems ok, ratio of observed and predicted zeros is within the tolerance range 
simulateResiduals(fittedModel = model_null_nb_c) %>%  plot()

# res <- simulateResiduals(fittedModel = model_null_nb)
# testOutliers(res, type = "bootstrap")


# Negative binomial zero-inflated
model_zinb_c <- glmmTMB(HWN_EHF ~ 1 + (1 | Country/SALID1), 
                        data = data, 
                        family = nbinom2,
                        ziformula = ~1) 

check_overdispersion(model_zinb_c) # No overdispersion detected.
check_zeroinflation(model_zinb_c) # Model seems ok, ratio of observed and predicted zeros is within the tolerance range 
simulation_output <- simulateResiduals(model_zinb_c)  %>%  plot()
DHARMa::testZeroInflation(model_zinb_c)


AIC(model_null_poisson_c, model_null_nb_c, model_zinb_c)
# df      AIC
# model_null_poisson_c  3 40839.22
# model_null_nb_c       4 39403.93
# model_zinb_c          5 39391.79

compare_performance(model_null_poisson_c, model_null_nb_c, model_zinb_c, rank = TRUE)

# Best adjustment is the Negative binomial zero-inflate



#---------------------------------------------
# HWF frequency - heatwave days
#---------------------------------------------

# hist
hist(data$HWF_EHF)

#Check zero
table(data$HWF_EHF == 0)
# FALSE  TRUE 
# 8218   989 

mean(data$HWF_EHF) # 21.36
var(data$HWF_EHF) # 366.9614


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


#----------------------------- Fit distribution
# Poisson distribution
model_null_poisson_F <- glmmTMB(HWF_EHF ~ 1 + (1 | SALID1), 
                                data = data, 
                                family = poisson(link = "log"))

check_overdispersion(model_null_poisson_F)
check_zeroinflation(model_null_poisson_F)
# simulation_output <- simulateResiduals(model_null_poisson_F)  %>%  plot()
# DHARMa::testZeroInflation(model_null_poisson_F)


# Negative binomial
model_null_nb_F <- glmmTMB(HWF_EHF ~ 1 + (1 | SALID1), 
                           data = data, 
                           nbinom2(link = "log"))

check_overdispersion(model_null_nb_F) # Underdispersion detected  
check_zeroinflation(model_null_nb_F) # Model is underfitting zeros (probable zero-inflation) 
simulation_output <- simulateResiduals(model_null_nb_F)  %>%  plot()
DHARMa::testZeroInflation(model_null_nb_F)

# Negative binomial zero-inflated
model_zinb_F <- glmmTMB(HWF_EHF ~ 1 + (1 | SALID1), 
                        data = data, 
                        family = nbinom2,
                        ziformula = ~1) 

check_overdispersion(model_zinb_F) # a bit of Overdispersion
check_zeroinflation(model_zinb_F) # Model seems ok, ratio of observed and predicted zeros is within the tolerance range
simulation_output <- simulateResiduals(model_zinb_F)  %>%  plot()
DHARMa::testZeroInflation(model_zinb_F)

# compare models
AIC(model_null_poisson_F, model_null_nb_F, model_zinb_F)
compare_performance(model_null_poisson_F, model_null_nb_F, model_zinb_F , rank = TRUE)

# Best adjustment is the Negative binomial zero-inflate

#---------- cities and countries
# Poisson distribution
model_null_poisson_F_c <- glmmTMB(HWF_EHF ~ 1 + (1 | Country/SALID1), 
                                  data = data, 
                                  family = poisson(link = "log"))

check_overdispersion(model_null_poisson_F_c)
check_zeroinflation(model_null_poisson_F_c)
simulation_output <- simulateResiduals(model_null_poisson_F_c)  %>%  plot()
DHARMa::testZeroInflation(model_null_poisson_F_c)


# Negative binomial
model_null_nb_F_c <- glmmTMB(HWF_EHF ~ 1 + (1 | Country/SALID1), 
                             data = data, 
                             nbinom2(link = "log"))

check_overdispersion(model_null_nb_F_c) # Underdispersion detected.
check_zeroinflation(model_null_nb_F_c) # Model is underfitting zeros (probable zero-inflation) 
simulateResiduals(fittedModel = model_null_nb_F_c) %>%  plot()


# Negative binomial zero-inflated
model_zinb_F_c <- glmmTMB(HWF_EHF ~ 1 + (1 | Country/SALID1), 
                          data = data, 
                          family = nbinom2,
                          ziformula = ~1) 


check_overdispersion(model_zinb_F_c) # No overdispersion detected.
check_zeroinflation(model_zinb_F_c) # Model seems ok, ratio of observed and predicted zeros is within the tolerance range 
simulation_output <- simulateResiduals(model_zinb_F_c)  %>%  plot()
DHARMa::testZeroInflation(model_zinb_F_c) 



AIC(model_null_poisson_F_c, model_null_nb_F_c, model_zinb_F_c)
# df       AIC
# model_null_poisson_F_c  3 190449.42
# model_null_nb_F_c       4  75821.24
# model_zinb_F_c          5  74098.65

compare_performance(model_null_poisson_F_c, model_null_nb_F_c, model_zinb_F_c, rank = TRUE)
anova(model_null_nb_F_c, model_zinb_F_c, test = "LRT")

# Best adjustment is the Negative binomial zero-inflate