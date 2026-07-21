##################################################################
# MS296: Descriptive Heatwaves- L1AD level
#
# Supplementary Tables 
##################################################################

library(tidyverse); library(dplyr); library(arrow)
library(glue); library(rlang); library(purrr)
library(flextable); library(openxlsx); library(readxl)
library(writexl); library(tidyr); library(gt)
library(stringr)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296")

#-----------------------------------------------------
# 1. Table summary (hw characteristics) - Sup table 2
#-----------------------------------------------------
data <- read_parquet("Data/data_hw_descriptive.parquet")

glimpse(data)

hw_vars <- c( "HWN_EHF", 
              "HWN_90P_2d", "HWN_90P_3d", "HWN_90P_4d",
              "HWN_95P_2d", "HWN_95P_3d", "HWN_95P_4d",
              "HWN_97.5P_2d", "HWN_97.5P_3d", "HWN_97.5P_4d")

hw_vars_f <- c( "HWF_EHF", 
                "HWF_90P_2d", "HWF_90P_3d", "HWF_90P_4d",
                "HWF_95P_2d", "HWF_95P_3d", "HWF_95P_4d",
                "HWF_97.5P_2d", "HWF_97.5P_3d", "HWF_97.5P_4d")

hw_vars_d <- c( "HWD_EHF", 
                "HWD_90P_2d", "HWD_90P_3d", "HWD_90P_4d",
                "HWD_95P_2d", "HWD_95P_3d", "HWD_95P_4d",
                "HWD_97.5P_2d", "HWD_97.5P_3d", "HWD_97.5P_4d")

all_hw_vars <- c(hw_vars, hw_vars_f, hw_vars_d)

make_table <- function(var) {
  data %>%
    summarize(value = glue("{med} ({lower}, {upper})",
                           med   = round(median(.data[[var]], na.rm = TRUE), 0),
                           lower = round(quantile(.data[[var]], 0.1, na.rm = TRUE), 0),
                           upper = round(quantile(.data[[var]], 0.9, na.rm = TRUE), 0)),
              .by = Country) %>%
    pivot_wider(names_from = Country, values_from = value) %>%
    mutate(variables = var, .before = 1)}


table_all_hw <- map_dfr(all_hw_vars, make_table)

#arrange columns
table_all_hw <- table_all_hw %>%  
  select(variables, Argentina, Brazil, `Central America`, Chile, Colombia, Mexico, Peru) %>% 
  rename("  " = variables)
  
table_all_hw

# Export as excel table
wb <- createWorkbook()
addWorksheet(wb, "HW_stats")

writeData(wb, sheet = "HW_stats", x = table_all_hw, startRow = 1, startCol = 1)

# note
footer_note <- "Note: All values are presented as median (10th, 90th percentile)"
footer_row <- nrow(table_all_hw) + 3
writeData(wb, sheet = "HW_stats", x = footer_note, startRow = footer_row, startCol = 1)

saveWorkbook(wb, file = "Tables/Sup_table_2.xlsx", overwrite = TRUE)

# Note: Sup table 3 in pop_exposure script
#-----------------------------------------------------
# 2. Model trends - Sup table 4
#-----------------------------------------------------
HWN_trends <- read_xlsx("Model_results/HWN_trends.xlsx")
HWF_trends <- read_xlsx("Model_results/HWF_trends.xlsx")
HWD_trends <- read_xlsx("Model_results/HWD_trends.xlsx")

# join
data_trends <- rbind(HWN_trends, HWF_trends, HWD_trends)

str(data_trends)

# note and export
note_row <- tibble(
  Model = "Note: * statistically significant (p < 0.05). HWN = number of heatwaves; HWF = heatwave frequency; HWD = heatwave duration.")

data_trends_excel <- bind_rows(data_trends, note_row)
data_trends_excel <- data_trends_excel %>% 
  rename(`Estimate (95% CI)` = "Estimate..95..CI.",
         "  " = Model)

write_xlsx(data_trends_excel, "Tables/Sup_table_4.xlsx")

#-----------------------------------------------------
# 3. Sup table 5 - CLZ
#-----------------------------------------------------
HWN_CLZ <- read_xlsx("Model_results/CLZ_HWN.xlsx")
HWF_CLZ <- read_xlsx("Model_results/CLZ_HWF.xlsx")
HWD_CLZ <- read_xlsx("Model_results/CLZ_HWD.xlsx")

str(HWN_CLZ)

HWN_CLZ <- HWN_CLZ %>%
  mutate("...1" = sub("^HWN_", "", ...1))

HWF_CLZ <- HWF_CLZ %>%
    rename("model" = "...1", 
         "clz" = "...2",
         "Differences at baseline IRR (95% CI) - HWF" = "Differences at baseline IRR (95% CI)",
         "Changes over time IRR (95% CI) - HWF" =  "Changes over time IRR (95% CI)") %>% 
  mutate(model = sub("^HWF_", "", model)) 

HWD_CLZ <- HWD_CLZ %>%
  
  rename("model" = "...1", 
         "clz" = "...2",
         "Differences at baseline IRR (95% CI) - HWD" = "Differences at baseline IRR (95% CI)",
         "Changes over time IRR (95% CI) - HWD" =  "Changes over time IRR (95% CI)") %>% 
  mutate(model = sub("^HWD_", "", model)) 

CLZ_all <- bind_cols(HWN_CLZ, HWF_CLZ, HWD_CLZ)

CLZ_all <- CLZ_all %>%
  select("...1", "...2",
         "Differences at baseline IRR (95% CI)", "Changes over time IRR (95% CI)",
         "Differences at baseline IRR (95% CI) - HWF", "Changes over time IRR (95% CI) - HWF",
         "Differences at baseline IRR (95% CI) - HWD", "Changes over time IRR (95% CI) - HWD")

note_row <- tibble("...1" = "Note: * statistically significant (p<0.05)")

CLZ_final_excel <- bind_rows(CLZ_all, note_row)

CLZ_final_excel <- CLZ_final_excel %>%
  rename("  " = "...1", "    " = "...2")

write_xlsx(CLZ_final_excel, "Tables/Sup_table_5.xlsx")

#-----------------------------------------------------
# 4. Model  - Sup table 6 - latitude
#-----------------------------------------------------
HWN_lat <- read_xlsx("Model_results/lat_HWN.xlsx")
HWF_lat <- read_xlsx("Model_results/lat_HWF.xlsx")
HWD_lat <- read_xlsx("Model_results/lat_HWD.xlsx")

HWN_lat <- HWN_lat %>%
  mutate(Model = sub("^HWN_", "", Model))

HWF_lat <- HWF_lat %>%
  mutate(Model = sub("^HWF_", "", Model)) %>% 
  rename("Differences at baseline IRR (95% CI) - HWF" = "Differences at baseline IRR (95% CI)",
         "Changes over time IRR (95% CI) - HWF" =  "Changes over time IRR (95% CI)")

HWD_lat <- HWD_lat %>%
  mutate(Model = sub("^HWD_", "", Model)) %>% 
  rename("Differences at baseline IRR (95% CI) - HWD" = "Differences at baseline IRR (95% CI)",
         "Changes over time IRR (95% CI) - HWD" =  "Changes over time IRR (95% CI)")

# join
latitude_final <- HWN_lat %>% 
  left_join(HWF_lat, by = "Model") %>% 
  left_join(HWD_lat, by = "Model")

latitude_final <- latitude_final %>% 
  rename("  " = "Model")

note_row <- tibble("  " = "Note: * statistically significant (p<0.05)")

latitude_final_excel <- bind_rows(latitude_final, note_row)

write_xlsx(latitude_final_excel, "Tables/Sup_table_6.xlsx")

#-----------------------------------------------------
# 5. Model  - Sup table 7 - univariable and full
#-----------------------------------------------------
HWN_uni_full <- read_xlsx("Model_results/univariable_HWN.xlsx")
HWF_uni_full <- read_xlsx("Model_results/univariable_HWF.xlsx")
HWD_uni_full <- read_xlsx("Model_results/univariable_HWD.xlsx")

str(HWN_uni_full)
HWN_uni_full <- HWN_uni_full %>%
  mutate(`Heatwave` = sub("^HWN_", "", `Heatwave`)) %>% 
  rename("Heatwave definition" = `Heatwave`, HWN = "Univariable estimate (CI 95%)") %>% 
  select("Heatwave definition", "City characteristic", HWN)

HWF_uni_full <- HWF_uni_full %>%
  mutate(`Heatwave` = sub("^HWF_", "", `Heatwave`)) %>% 
  rename("Heatwave definition" = `Heatwave`, HWF = "Univariable estimate (CI 95%)") %>% 
  select("Heatwave definition", "City characteristic", HWF)

HWD_uni_full <- HWD_uni_full %>%
  mutate(`Heatwave` = sub("^HWD_", "", `Heatwave`)) %>% 
  rename("Heatwave definition" = `Heatwave`, HWD = "Univariable estimate (CI 95%)") %>% 
  select("Heatwave definition", "City characteristic", HWD)

Sup_table_7 <- HWN_uni_full %>% 
  left_join(HWF_uni_full, by = c("Heatwave definition", "City characteristic")) %>% 
  left_join(HWD_uni_full, by = c("Heatwave definition", "City characteristic"))     

note_row7 <- tibble("Heatwave definition" = "Note: * statistically significant (p<0.05)")

Sup_table_7_final <- bind_rows(Sup_table_7, note_row7)

Sup_table_7_final <- Sup_table_7_final %>%
  group_by(`Heatwave definition`) %>%
  mutate(`Heatwave definition` = if_else(row_number() == 1, `Heatwave definition`, NA_character_)) %>%
  ungroup()

write_xlsx(Sup_table_7_final, "Tables/Sup_table_7.xlsx")

#-----------------------------------------------------
# 6. Model  - Sup table 8 - hybrid
#-----------------------------------------------------
HWN_hybrid <- read_xlsx("Model_results/HWN_hybrid_models.xlsx")
HWF_hybrid <- read_xlsx("Model_results/HWF_hybrid_models.xlsx")
HWD_hybrid <- read_xlsx("Model_results/HWD_hybrid_models.xlsx")

# separate tables and filter the results
# HWN
HWN_hybrid_btw <- HWN_hybrid %>%
  filter(str_detect(term, "_between_")) %>% 
  rename(Between_HWN = final) %>% 
  mutate(outcome = sub("^HWN_", "", outcome)) %>% 
  select(outcome, predictor, Between_HWN)

HWN_hybrid_wt <- HWN_hybrid %>%
  filter(str_detect(term, "_within_")) %>% 
  rename(Within_HWN = final) %>% 
  mutate(outcome = sub("^HWN_", "", outcome)) %>% 
  select(outcome, predictor, Within_HWN)

#HWF
HWF_hybrid_btw <- HWF_hybrid %>%
  filter(str_detect(term, "_between_")) %>% 
  rename(Between_HWF = final) %>% 
  mutate(outcome = sub("^HWF_", "", outcome)) %>% 
  select(outcome, predictor, Between_HWF)

HWF_hybrid_wt <- HWF_hybrid %>%
  filter(str_detect(term, "_within_")) %>% 
  rename(Within_HWF = final) %>% 
  mutate(outcome = sub("^HWF_", "", outcome)) %>% 
  select(outcome, predictor, Within_HWF)

#HWD
HWD_hybrid_btw <- HWD_hybrid %>%
  filter(str_detect(term, "_between_")) %>% 
  rename(Between_HWD = final) %>% 
  mutate(outcome = sub("^HWD_", "", outcome)) %>% 
  select(outcome, predictor, Between_HWD)

HWD_hybrid_wt <- HWD_hybrid %>%
  filter(str_detect(term, "_within_")) %>% 
  rename(Within_HWD = final) %>% 
  mutate(outcome = sub("^HWD_", "", outcome)) %>% 
  select(outcome, predictor, Within_HWD)

# join
HWN_hybrid_FINAL <- HWN_hybrid_btw %>% left_join(HWN_hybrid_wt, by =c("outcome", "predictor"))
HWF_hybrid_FINAL <- HWF_hybrid_btw %>% left_join(HWF_hybrid_wt, by =c("outcome", "predictor"))
HWD_hybrid_FINAL <- HWD_hybrid_btw %>% left_join(HWD_hybrid_wt, by =c("outcome", "predictor"))

hybrid_all <- HWN_hybrid_FINAL %>% 
  left_join(HWF_hybrid_FINAL, by = c("outcome", "predictor")) %>%
  left_join(HWD_hybrid_FINAL, by = c("outcome", "predictor"))

# organize
hybrid_all <- hybrid_all %>% 
    mutate(predictor = case_when(
    predictor == "total_pop" ~ "City population",
    predictor == "pop_density" ~ "Population density",
    predictor == "GDP" ~ "GDP per capita",
    predictor == "pop_over65" ~ "Population ≥65 years (%)",
    predictor == "education" ~ "Completed primary education (%)",
    predictor == "urban_area_pct" ~ "Urban area (%)",
    predictor == "NDVI" ~ "NDVI",
    TRUE ~ predictor)) %>% 
  rename("Heatwave definition" = outcome, "City characteristic" = predictor) 
  
note_row8 <- tibble("Heatwave definition" = "Note: * statistically significant (p<0.05)")

Sup_table_8 <- bind_rows(hybrid_all, note_row8)

Sup_table_8_final <- Sup_table_8 %>%
  group_by(`Heatwave definition`) %>%
  mutate(`Heatwave definition` = if_else(row_number() == 1, `Heatwave definition`, NA_character_)) %>%
  ungroup()

write_xlsx(Sup_table_8_final, "Tables/Sup_table_8.xlsx")
