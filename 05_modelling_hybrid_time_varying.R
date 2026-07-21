##################################################################
# MS296: Descriptive Heatwaves - L1AD
#
# Run hybrid models - time varying variables
##################################################################

library(dplyr); library(glmmTMB); library(arrow)
library(tidyr); library(broom.mixed); library(writexl)

data <- read_parquet("Data/data_final_model.parquet")

data$SALID1 <- as.factor(data$SALID1)
data$Country <- as.factor(data$Country)

data <- data[order(data$SALID1, data$YEAR), ]

# 1. Prepare BETWEEN / WITHIN
data <- data %>%
  group_by(SALID1) %>%
  mutate(across(c(total_pop, pop_density_guf, pop_over65, GDP, 
                  education, urban_area_pct, NDVI),
      list(between = ~mean(.x, na.rm = TRUE),
           within  = ~.x - mean(.x, na.rm = TRUE)), .names = "{.col}_{.fn}")) %>%
  ungroup() %>%
  mutate(across(ends_with("between"), ~as.numeric(scale(.x)), .names = "{.col}_z"),
         across(ends_with("within"),  ~as.numeric(scale(.x)), .names = "{.col}_z"))

# BETWEEN-WITHIN
var_pairs <- list(total_pop = c("total_pop_between_z", "total_pop_within_z"),
                  pop_density = c("pop_density_guf_between_z", "pop_density_guf_within_z"),
                  pop_over65 = c("pop_over65_between_z", "pop_over65_within_z"),
                  GDP = c("GDP_between_z", "GDP_within_z"),
                  education = c("education_between_z", "education_within_z"),
                  urban_area_pct = c("urban_area_pct_between_z", "urban_area_pct_within_z"),
                  NDVI = c("NDVI_between_z", "NDVI_within_z"))


all_hw_sets <- list(
  HWN = c("HWN_EHF", 
          "HWN_90P_2d", "HWN_90P_3d", "HWN_90P_4d",
          "HWN_95P_2d", "HWN_95P_3d", "HWN_95P_4d",
          "HWN_97.5P_2d", "HWN_97.5P_3d", "HWN_97.5P_4d"),
  
  HWF = c("HWF_EHF", 
          "HWF_90P_2d", "HWF_90P_3d", "HWF_90P_4d",
          "HWF_95P_2d", "HWF_95P_3d", "HWF_95P_4d",
          "HWF_97.5P_2d", "HWF_97.5P_3d", "HWF_97.5P_4d"),
  
  HWD = c("HWD_EHF", 
          "HWD_90P_2d", "HWD_90P_3d", "HWD_90P_4d",
          "HWD_95P_2d", "HWD_95P_3d", "HWD_95P_4d",
          "HWD_97.5P_2d", "HWD_97.5P_3d", "HWD_97.5P_4d"))


get_conv_symbol <- function(model) {
  if (!is.null(model$sdr) && !model$sdr$pdHess) {
    return("+")
  } else {
    return("")
  }
}

# 2. extract results
extract_results <- function(model) {
  
  conv_symbol <- get_conv_symbol(model)
  
  broom.mixed::tidy(model, conf.int = TRUE, effects = "fixed") %>%
    mutate(rr  = exp(estimate),
           lcl = exp(conf.low),
           ucl = exp(conf.high),
           stars = ifelse(p.value < 0.05, "*", ""),
           final = sprintf("%.2f (%.2f, %.2f)%s%s", rr, lcl, ucl, stars, conv_symbol)) %>%
    select(term, final)
}

run_all_models <- function(data, hw_sets) {
  
  results_all <- list()
  
  for (set_name in names(hw_sets)) {
    
    message("Running set: ", set_name)
    
    deps <- hw_sets[[set_name]]
    
    for (dep in deps) {
      
      message("  -> ", dep)
      
      for (v in names(var_pairs)) {
        
        btw <- var_pairs[[v]][1]
        wht <- var_pairs[[v]][2]
        
        fml <- as.formula(paste0(dep, " ~ ", btw, " + ", wht,
                                 " + YEAR_dec + (1 | Country/SALID1)"))
        
        model <- tryCatch(
          glmmTMB(fml, data = data, family = nbinom2(link = "log")),
          error = function(e) {
            message(paste("Error in:", dep, "+", v))
            return(NULL)
          }
        )
        
        if (is.null(model)) next
        
        # Build tab from the model, then append it ✅
        tab <- extract_results(model) %>%
          mutate(dep_var  = dep,
                 set_name = set_name,
                 covariate = v)
        
        results_all[[length(results_all) + 1]] <- tab
      }
    }
  }
  
  bind_rows(results_all)
}

# 3. Run
final_table <- run_all_models(data, all_hw_sets)

table_HWN <- final_table %>% filter(set_name == "HWN") %>% select(-set_name)
table_HWF <- final_table %>% filter(set_name == "HWF") %>% select(-set_name)
table_HWD <- final_table %>% filter(set_name == "HWD") %>% select(-set_name)

# 4. Export
write_xlsx(table_HWN, "Model_results/HWN_hybrid_models.xlsx")
write_xlsx(table_HWF, "Model_results/HWF_hybrid_models.xlsx")
write_xlsx(table_HWD, "Model_results/HWD_hybrid_models.xlsx")
