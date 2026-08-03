##################################################################
# MS296: Descriptive Heatwaves
#
# Multilevel models for HWN, HWF, HWD
##################################################################

library(dplyr); library(tidyr); library(purrr)
library(arrow); library(glmmTMB); library(broom.mixed)
library(ggplot2); library(writexl)

data <- read_parquet("Data/data_final_model.parquet")

data$SALID1 <- as.factor(data$SALID1)
data$Country <- as.factor(data$Country)

data <- data[order(data$SALID1, data$YEAR), ]

# 1. Functions
format_result <- function(mod, var_name) {
  
  coefs <- tidy(mod, effects = "fixed", conf.int = TRUE)
  year_row <- coefs %>% filter(term == "YEAR_dec")
  
  irr  <- exp(year_row$estimate)
  low  <- exp(year_row$conf.low)
  high <- exp(year_row$conf.high)
  
  star <- ifelse(year_row$p.value < 0.05, "*", "")
  
  # warning de convergence (+)
  warn <- ifelse(!is.null(mod$sdr$pdHess) && mod$sdr$pdHess == FALSE, "+", "")
  
  irr_txt <- sprintf("%.2f (%.2f, %.2f)%s%s", irr, low, high, star, warn)
  
  data.frame(
    Model = var_name,
    "Estimate (95% CI)" = irr_txt)
}

run_models <- function(data, vars, output_name) {
  
  model_list <- map(vars, function(var) {
    formula_mod <- reformulate("YEAR_dec + (1 | Country/SALID1)", response = var)
    
    tryCatch({
      glmmTMB(formula_mod, data = data, family = nbinom2(link = "log"))
    }, error = function(e) {
      message(paste("Error in:", var))
      return(NULL)
    })
  })
  
  names(model_list) <- vars
  model_list <- compact(model_list)
  
  results <- map2_dfr(model_list, names(model_list), format_result)
  
  write_xlsx(results, paste0("Model_results/", output_name))
  
  return(results)
}

# 2. Run function for each HW definition
HWN_vars <- c("HWN_EHF",
              "HWN_90P_2d","HWN_90P_3d","HWN_90P_4d",
              "HWN_95P_2d","HWN_95P_3d","HWN_95P_4d",
              "HWN_97.5P_2d","HWN_97.5P_3d","HWN_97.5P_4d")

HWF_vars <- c("HWF_EHF",
              "HWF_90P_2d","HWF_90P_3d","HWF_90P_4d",
              "HWF_95P_2d","HWF_95P_3d","HWF_95P_4d",
              "HWF_97.5P_2d","HWF_97.5P_3d","HWF_97.5P_4d")

HWD_vars <- c("HWD_EHF",
              "HWD_90P_2d","HWD_90P_3d","HWD_90P_4d",
              "HWD_95P_2d","HWD_95P_3d","HWD_95P_4d",
              "HWD_97.5P_2d","HWD_97.5P_3d","HWD_97.5P_4d")


res_HWN <- run_models(data, HWN_vars, "HWN_trends.xlsx")
res_HWF <- run_models(data, HWF_vars, "HWF_trends.xlsx")
res_HWD <- run_models(data, HWD_vars, "HWD_trends.xlsx")
