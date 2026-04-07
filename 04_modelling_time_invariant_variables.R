##################################################################
# MS296: Descriptive Heatwaves- L1AD level 
#
# This script multilevel models for NHW -  add variables
# Time invariant factors - Climate zones and latitude
##################################################################

library(dplyr); library(purrr); library(broom.mixed)
library(glmmTMB); library(writexl); library(arrow)
library(sjPlot)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296")

data <- read_parquet("Data/data_final_model.parquet")

str(data)

data$SALID1 <- as.factor(data$SALID1)
data$Country <- as.factor(data$Country)
data$CLZ <- as.factor(data$CLZ)

data <- data[order(data$SALID1, data$YEAR), ]

# CLZ
# 1. reference
data$CLZ <- relevel(factor(data$CLZ), ref = "Tropical")

# 2. function to run models and extract results
run_models_clz <- function(data, hw_vars, output_file) {
  
  format_irr <- function(irr, low, high, stars) {
    sprintf("%.2f (%.2f–%.2f)%s", irr, low, high, stars)
  }
  
  extract_results <- function(model, model_name) {
    
    tt <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
      mutate(IRR = exp(estimate),
             CI_low = exp(conf.low),
             CI_high = exp(conf.high),
             stars = ifelse(p.value < 0.05, "*", ""))
    
    # baseline
    base <- tt %>%
      filter(grepl("^CLZ", term) & !grepl(":", term)) %>%
      mutate(CLZ = sub("^CLZ", "", term),
             Baseline_IRR = format_irr(IRR, CI_low, CI_high, stars)) %>%
      select(CLZ, Baseline_IRR)
    
    base <- bind_rows(tibble(CLZ = "Tropical", Baseline_IRR = "Reference"), base)
    
    # interaction with time
    annual <- tt %>%
      filter(term == "YEAR_dec" | grepl("YEAR_dec", term)) %>%
      mutate(CLZ = case_when(
        term == "YEAR_dec" ~ "Tropical",
        grepl("Arid", term) ~ "Arid",
        grepl("Temperate", term) ~ "Temperate",
        grepl("Polar", term) ~ "Polar", TRUE ~ NA_character_),
        Annual_IRR = format_irr(IRR, CI_low, CI_high, stars)) %>%
      select(CLZ, Annual_IRR)
    
    full_join(base, annual, by = "CLZ") %>%
      mutate(CLZ = factor(CLZ, levels = c("Tropical", "Arid", "Temperate", "Polar")),
             Model = model_name) %>%
      arrange(CLZ) %>%
      select(Model, CLZ, Baseline_IRR, Annual_IRR)
  }
  
  # run models
  models <- map(hw_vars, function(var) {
    formula <- as.formula(
      paste0(var, " ~ CLZ * YEAR_dec + (1 | Country/SALID1)"))
    
    glmmTMB(formula, data = data, family = nbinom2(link = "log"))
  }) %>%
    set_names(hw_vars)
  
  final_table <- map2_df(models, names(models), extract_results)
  
  final_table <- final_table %>%
    group_by(Model) %>%
    mutate(Model = ifelse(row_number() == 1, Model, "")) %>%
    ungroup() %>%
    rename(" "  = Model,
           "  " = CLZ,
           "Differences at baseline IRR (95% CI)" = Baseline_IRR,
           "Changes over time IRR (95% CI)" = Annual_IRR)
  
  # export
  write_xlsx(final_table, output_file)
  
  return(final_table)
}

# 3. Run models for each HW definition
# HWN
run_models_clz(data, c("HWN_EHF", 
                       "HWN_90P_2d", "HWN_90P_3d", "HWN_90P_4d",
                       "HWN_95P_2d", "HWN_95P_3d", "HWN_95P_4d",
                       "HWN_97.5P_2d", "HWN_97.5P_3d", "HWN_97.5P_4d"),
               "Model_results/CLZ_HWN.xlsx")

# HWF
run_models_clz(data, c("HWF_EHF", 
                       "HWF_90P_2d", "HWF_90P_3d", "HWF_90P_4d",
                       "HWF_95P_2d", "HWF_95P_3d", "HWF_95P_4d",
                       "HWF_97.5P_2d", "HWF_97.5P_3d", "HWF_97.5P_4d"),
               "Model_results/CLZ_HWF.xlsx")

# HWD
run_models_clz(data, c("HWD_EHF", 
                       "HWD_90P_2d", "HWD_90P_3d", "HWD_90P_4d",
                       "HWD_95P_2d", "HWD_95P_3d", "HWD_95P_4d",
                       "HWD_97.5P_2d", "HWD_97.5P_3d", "HWD_97.5P_4d"),
               "Model_results/CLZ_HWD.xlsx")

# ------------------------------------- Latitude-----------------------------------------------------------------------------
# 1. Func
run_models_lat <- function(data, hw_vars, output_file) {
  
  format_irr <- function(irr, low, high, stars, warn) {
    flag <- ifelse(warn, "+", "")
    sprintf("%.2f (%.2f–%.2f)%s%s", irr, low, high, stars, flag)
  }
  
  extract_results <- function(model, model_name) {
    
    # check convergence
    warn <- !isTRUE(model$sdr$pdHess)
    
    tt <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
      mutate(IRR = exp(estimate),
             CI_low = exp(conf.low),
             CI_high = exp(conf.high),
             stars = ifelse(p.value < 0.05, "*", ""))
    
    # baseline 
    base <- tt %>%
      filter(term == "lat_abs_z") %>%
      mutate(Result = format_irr(IRR, CI_low, CI_high, stars, warn)) %>%
      select(Result) %>%
      rename("Differences at baseline IRR (95% CI)" = Result)
    
    # interaction with time
    annual <- tt %>%
      filter(term == "lat_abs_z:YEAR_dec") %>%
      mutate(Result = format_irr(IRR, CI_low, CI_high, stars, warn)) %>%
      select(Result) %>%
      rename("Changes over time IRR (95% CI)" = Result)
    
    bind_cols(tibble(Model = model_name), base, annual)
  }
  
  models <- purrr::map(hw_vars, function(var) {
    
    formula <- as.formula(
      paste0(var, " ~ lat_abs_z * YEAR_dec + (1 | Country/SALID1)"))
    
    glmmTMB::glmmTMB(formula, data = data, family = nbinom2(link = "log"))
    
  }) %>%
    set_names(hw_vars)
  
  final_table <- purrr::map2_df(models, names(models), extract_results)
  
  # export
  writexl::write_xlsx(final_table, output_file)
  
  return(final_table)
}

# 2. Run models for each HW definition
# HWN
run_models_lat(data, c("HWN_EHF", 
                       "HWN_90P_2d", "HWN_90P_3d", "HWN_90P_4d",
                       "HWN_95P_2d", "HWN_95P_3d", "HWN_95P_4d",
                       "HWN_97.5P_2d", "HWN_97.5P_3d", "HWN_97.5P_4d"),
               "Model_results/lat_HWN.xlsx")

# HWF
run_models_lat(data, c("HWF_EHF", 
                       "HWF_90P_2d", "HWF_90P_3d", "HWF_90P_4d",
                       "HWF_95P_2d", "HWF_95P_3d", "HWF_95P_4d",
                       "HWF_97.5P_2d", "HWF_97.5P_3d", "HWF_97.5P_4d"),
               "Model_results/lat_HWF.xlsx")

# HWD
run_models_lat(data, c("HWD_EHF", 
                       "HWD_90P_2d", "HWD_90P_3d", "HWD_90P_4d",
                       "HWD_95P_2d", "HWD_95P_3d", "HWD_95P_4d",
                       "HWD_97.5P_2d", "HWD_97.5P_3d", "HWD_97.5P_4d"),
               "Model_results/lat_HWD.xlsx")
