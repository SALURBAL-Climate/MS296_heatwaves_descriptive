##################################################################
# MS296: Descriptive Heatwaves - L1AD
#
# Run univariate models - time varying variables
##################################################################


library(tidyverse); library(glmmTMB); library(broom.mixed)
library(writexl); library(readxl); library(purrr)
library(arrow)

rm(list = ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296")

data <- read_parquet("Data/data_final_model.parquet")

data$SALID1 <- as.factor(data$SALID1)
data$Country <- as.factor(data$Country)

data <- data[order(data$SALID1, data$YEAR), ]

# 1. Prepare data for modelling
data <- data %>% 
  mutate(total_pop_z = scale(total_pop),
         pop_density_guf_z = scale(pop_density_guf),
         urban_area_pc_z = scale(urban_area_pct),
         pop_over65_z = scale(pop_over65),
         GDP_z = scale(GDP),
         NDVI_z = scale(NDVI),
         education_z = scale(education))

vars <- c("total_pop_z", "pop_density_guf_z", "pop_over65_z", 
          "GDP_z", "education_z", "urban_area_pc_z", "NDVI_z")


# 2. Helpers
get_stars <- function(p) ifelse(p < 0.05, "*", "")

format_irr <- function(rr, low, high, stars, warn) {
  plus <- ifelse(warn, "+", "")
  sprintf("%.2f (%.2f–%.2f)%s%s", rr, low, high, stars, plus)
}

var_labels <- c(total_pop_z = "City population",
                pop_density_guf_z = "Population density",
                GDP_z = "GDP per capita",
                pop_over65_z = "Population ≥65 years (%)",
                education_z = "Completed primary education (%)",
                urban_area_pc_z = "Urban area (%)",
                NDVI_z = "NDVI")


# 3. Model function
fit_model <- function(formula, data) {
  
  warn_flag <- FALSE
  
  model <- tryCatch(
    withCallingHandlers(
      glmmTMB(formula, family = nbinom2(link = "log"), data = data),
      warning = function(w) {
        if (grepl("converg", w$message, ignore.case = TRUE)) {
          warn_flag <<- TRUE
        }
        invokeRestart("muffleWarning")
      }),
    error = function(e) NULL
  )
  
  list(model = model, warn = warn_flag)
}


# 4. Run models
run_models <- function(hw, data, vars) {
  
  message("Running: ", hw)
  
  results <- map_dfr(vars, function(v) {
    
    f <- as.formula(paste0(hw, " ~ ", v, " + YEAR_dec + (1 | Country/SALID1)"))
    
    res <- fit_model(f, data)
    m <- res$model
    warn <- res$warn
    
    if (is.null(m)) {
      return(tibble(
        Variable = v,
        estimate = NA))
    }
    
    tidy(m, effects = "fixed", conf.int = TRUE) %>%
      filter(term == v) %>%
      mutate(estimate = format_irr(exp(estimate),
                                   exp(conf.low),
                                   exp(conf.high),
                                   get_stars(p.value), 
                                   warn)) %>%
      select(Variable = term, estimate)
  })
  
  results %>%
    mutate(Variable = recode(Variable, !!!var_labels),
           HW = hw)
}

# 5. Run all + export
run_all <- function(hw_vars, data, vars, output_file) {
  
  final_table <- map_dfr(hw_vars, function(hw) {
    
    run_models(hw, data, vars) %>%
      mutate(
        Heatwave = gsub("HWN_|HWF_|HWD_", "", hw))
  })
  
  final_table <- final_table %>%
    select(Heatwave, 
           `City characteristic` = Variable, 
           `Univariable estimate (CI 95%)` = estimate)
  
  write_xlsx(final_table, output_file)
  
  return(final_table)
}

HWN_vars <- c("HWN_EHF",
              "HWN_90P_2d", "HWN_90P_3d", "HWN_90P_4d",
              "HWN_95P_2d", "HWN_95P_3d", "HWN_95P_4d",
              "HWN_97.5P_2d", "HWN_97.5P_3d", "HWN_97.5P_4d")

HWF_vars <- c("HWF_EHF",
              "HWF_90P_2d", "HWF_90P_3d", "HWF_90P_4d",
              "HWF_95P_2d", "HWF_95P_3d", "HWF_95P_4d",
              "HWF_97.5P_2d", "HWF_97.5P_3d", "HWF_97.5P_4d")

HWD_vars <- c("HWD_EHF",
              "HWD_90P_2d", "HWD_90P_3d", "HWD_90P_4d",
              "HWD_95P_2d", "HWD_95P_3d", "HWD_95P_4d",
              "HWD_97.5P_2d", "HWD_97.5P_3d", "HWD_97.5P_4d")

table_HWN <- run_all(HWN_vars, data, vars, "Model_results/univariable_HWN.xlsx")
table_HWF <- run_all(HWF_vars, data, vars, "Model_results/univariable_HWF.xlsx")
table_HWD <- run_all(HWD_vars, data, vars, "Model_results/univariable_HWD.xlsx")
