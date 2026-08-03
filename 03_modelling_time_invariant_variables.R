##################################################################
# MS296: Descriptive Heatwaves- L1AD level 
#
# This script multilevel models for NHW -  add variables
# Time invariant factors - Climate zones and latitude
##################################################################

library(dplyr); library(purrr); library(broom.mixed)
library(glmmTMB); library(writexl); library(arrow)

data <- read_parquet("Data/data_final_model.parquet")

data$SALID1 <- as.factor(data$SALID1)
data$Country <- as.factor(data$Country)
data$CLZ <- as.factor(data$CLZ)

data <- data[order(data$SALID1, data$YEAR), ]

# CLZ
# 1. Reference
data$CLZ <- relevel(factor(data$CLZ), ref = "Tropical")

# 2. function to run models and extract results
run_models_clz <- function(data, hw_vars, output_file){
  
  format_irr <- function(irr, low, high, stars){
    sprintf("%.2f (%.2f, %.2f)%s",
            irr, low, high, stars)
  }
  
  extract_results <- function(model, model_name){
    
    # coef
    b  <- fixef(model)$cond
    VC <- vcov(model)$cond
    tt <- tidy(model, effects="fixed", conf.int=TRUE)
    
    # baseline
    base <- tt %>%
      mutate(IRR=exp(estimate),
             CI_low=exp(conf.low),
             CI_high=exp(conf.high),
             stars=ifelse(p.value<0.05,"*","")) %>%
      filter(grepl("^CLZ",term) &
               !grepl(":",term)) %>%
      mutate(CLZ=sub("^CLZ","",term),
             Baseline_IRR=format_irr(IRR,
                                     CI_low,
                                     CI_high,
                                     stars)) %>%
      select(CLZ,Baseline_IRR)
    
    base <- bind_rows(
      tibble(CLZ="Tropical",
             Baseline_IRR="1 (Ref.)"),base)
    
    # Trends
    trend_fun <- function(interaction=NULL,
                          level){
      if(is.null(interaction)){
        beta <- b["YEAR_dec"]
        se <- sqrt(VC["YEAR_dec", "YEAR_dec"])
      }else{
        
        beta <- b["YEAR_dec"] + b[interaction]
        se <- sqrt(VC["YEAR_dec", "YEAR_dec"] + 
                     VC[interaction, interaction] +
                     2*VC["YEAR_dec", interaction])
      }
      
      low <- beta-1.96*se
      high<- beta+1.96*se
      z <- beta/se
      p <- 2*pnorm(abs(z),
                   lower.tail=FALSE)
      tibble(CLZ=level, Annual_IRR= format_irr(exp(beta),
                                               exp(low),
                                               exp(high),
                                               ifelse(p<0.05,"*","")))
    }
    
    annual <- bind_rows(trend_fun(NULL, "Tropical"),
                        trend_fun("CLZArid:YEAR_dec", "Arid"),
                        trend_fun("CLZTemperate:YEAR_dec", "Temperate"))
    
    full_join(base, annual, by="CLZ") %>%
      mutate(CLZ=factor(CLZ, levels=c("Tropical", "Arid", "Temperate")),
             Model=model_name) %>%
      arrange(CLZ) %>%
      select(Model, CLZ, Baseline_IRR, Annual_IRR)
  }
  
  models <- map(hw_vars, function(var){
    glmmTMB(as.formula(paste0(var, " ~ CLZ*YEAR_dec + (1|Country/SALID1)")),
            data=data,
            family=nbinom2(link="log"))
  }) %>%
    set_names(hw_vars)
  
  # table
  final_table <- map2_df(models, names(models), extract_results)
  
  final_table <- final_table %>%
    group_by(Model) %>%
    mutate(Model=ifelse(row_number()==1, Model, "")) %>%
    ungroup() %>%
    rename(" "=Model, "  "=CLZ,
           "Differences at baseline IRR (95% CI)"= Baseline_IRR,
           "Temporal trend IRR (95% CI) per decade"= Annual_IRR)
  
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

# check
# model_test <- glmmTMB(HWN_EHF ~ CLZ * YEAR_dec + (1 | Country/SALID1),
#                       data = data,
#                       family = nbinom2(link = "log"))
# tab_model(model_test)
# 
# trend <- emtrends(model_test, specs = "CLZ", var = "YEAR_dec")
# trend
# summary(trend)
# exp(summary(trend)$YEAR_dec.trend)

# ------------------------------------- Latitude-----------------------------------------------------------------------------
# 1. Func
run_models_lat <- function(data, hw_vars, output_file) {
  
  format_irr <- function(irr, low, high, stars, warn = FALSE) {
    flag <- ifelse(warn, "+", "")
    sprintf("%.2f (%.2f, %.2f)%s%s",
            irr, low, high, stars, flag)
  }
  
  extract_results <- function(model, model_name) {
    
    warn <- !isTRUE(model$sdr$pdHess)
    
    tt <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
      mutate(IRR = exp(estimate),
             CI_low = exp(conf.low),
             CI_high = exp(conf.high),
             stars = ifelse(p.value < 0.05, "*", ""))
    
    ## Baseline
    base <- tt %>%
      filter(term == "lat_abs_z") %>%
      transmute(Baseline_IRR = format_irr(IRR,
                                          CI_low,
                                          CI_high,
                                          stars,
                                          warn))
    
    ## Interaction
    interaction <- tt %>%
      filter(term == "lat_abs_z:YEAR_dec") %>%
      transmute(Interaction_IRR = format_irr(IRR,
                                             CI_low,
                                             CI_high,
                                             stars,
                                             warn))
    
    ## Temporal trend at mean latitude (0 SD)
    trend_mean <- tt %>%
      filter(term == "YEAR_dec") %>%
      transmute(Trend_mean = format_irr(IRR,
                                        CI_low,
                                        CI_high,
                                        stars,
                                        warn))
    
    ## Temporal trend at +1 SD latitude
    b <- fixef(model)$cond
    VC <- vcov(model)$cond
    
    beta <- b["YEAR_dec"] + b["lat_abs_z:YEAR_dec"]
    
    se <- sqrt(
      VC["YEAR_dec","YEAR_dec"] +
        VC["lat_abs_z:YEAR_dec","lat_abs_z:YEAR_dec"] +
        2 * VC["YEAR_dec","lat_abs_z:YEAR_dec"])
    low <- beta - 1.96 * se
    high <- beta + 1.96 * se
    
    z <- beta / se
    
    p <- 2 * pnorm(abs(z), lower.tail = FALSE)
    
    trend_1sd <- tibble(
      Trend_1SD = format_irr(exp(beta),
                             exp(low),
                             exp(high),
                             ifelse(p < 0.05, "*", ""),
                             warn))
    bind_cols(
      tibble(Model = model_name),
      base,
      interaction,
      trend_mean,
      trend_1sd)
  }
  
  # Fit models
  models <- map(hw_vars, function(var){
    glmmTMB(as.formula(paste0(var, " ~ lat_abs_z * YEAR_dec + (1 | Country/SALID1)")), 
            data = data,
            family = nbinom2(link = "log"))
    
  }) %>%
    
    set_names(hw_vars)
  
  # Extract results
  final_table <-map2_df(models, 
                        names(models), extract_results)
  
  # Tables
  final_table <- final_table %>%
    rename(
      "Model" = Model,
      "Baseline IRR (95% CI) for a 1-SD higher latitude" = Baseline_IRR,
      "Interaction with time IRR (95% CI)" = Interaction_IRR,
      "Temporal trend IRR (95% CI) at mean latitude" = Trend_mean,
      "Temporal trend IRR (95% CI) at +1 SD latitude" = Trend_1SD)
  
  write_xlsx(final_table, output_file)
  
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

# model_test <- glmmTMB(HWN_EHF ~ lat_abs_z * YEAR_dec + (1 | Country/SALID1),
#                       data = data,
#                       family = nbinom2(link = "log"))
# tab_model(model_test)
