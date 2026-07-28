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

data <- read_parquet("Data/data_final_model.parquet")

#-----------------------------------------------------
# 1. Sup table 2
#-----------------------------------------------------
# 1. functions and prepare data
add_total <- function(data, summary_expr, value_name) {
  
  by_country <- data %>%
    summarize(
      !!value_name := {{ summary_expr }},
      .by = Country)
  
  total <- data %>%
    summarize(
      !!value_name := {{ summary_expr }}) %>%
    mutate(Country = "Total")
  
  bind_rows(by_country, total) %>%
    pivot_wider(
      names_from = Country,
      values_from = !!sym(value_name))
}

to_char <- function(df){
  df %>% mutate(across(everything(), as.character))
}

city_level <- data %>%
  group_by(Country, SALID1) %>%
  summarise(
    total_pop = mean(total_pop, na.rm = TRUE)/1000,
    pop_over65 = mean(pop_over65, na.rm = TRUE)*100,
    pop_density_guf = mean(pop_density_guf, na.rm = TRUE)/1000,
    GDP = mean(GDP, na.rm = TRUE)/1000,
    education = mean(education, na.rm = TRUE),
    urban_area_pct = mean(urban_area_pct, na.rm = TRUE),
    NDVI = mean(NDVI, na.rm = TRUE),
    CLZ = first(CLZ),.groups = "drop")

# 2. Number of cities
sup_table_2a <- add_total(city_level,
                          n_distinct(SALID1),
                          "n_cities") %>%
  mutate(variables="n_cities", .before=1) %>%
  to_char()

# 3. City population
sup_table_2b <- add_total(
  city_level,
  glue("{round(median(total_pop))} ({round(quantile(total_pop,.1))}, {round(quantile(total_pop,.9))})"),
  "city_pop") %>%
  mutate(variables="city_pop", .before=1) %>%
  to_char()

# 4. HWN, HWF, and HWD
sup_table_2c <- bind_rows(
  add_total(data,
            glue("{round(median(HWN_EHF),0)} ({round(quantile(HWN_EHF,.1),0)}, {round(quantile(HWN_EHF,.9),0)})"),
            "HWN_EHF") %>%
    mutate(variables="HWN_EHF", .before=1),
  add_total(data,
            glue("{round(median(HWF_EHF),0)} ({round(quantile(HWF_EHF,.1),0)}, {round(quantile(HWF_EHF,.9),0)})"),
            "HWF_EHF") %>%
    mutate(variables="HWF_EHF", .before=1),
  add_total(data,
            glue("{round(median(HWD_EHF),0)} ({round(quantile(HWD_EHF,.1),0)}, {round(quantile(HWD_EHF,.9),0)})"),
            "HWD_EHF") %>%
    mutate(variables="HWD_EHF", .before=1)) %>% to_char()

# 5. Climate zones
sup_table_2d <- city_level %>%
  distinct(Country, CLZ, SALID1) %>%
  count(Country, CLZ, name="n_cities") %>%
  bind_rows(
    city_level %>%
      distinct(CLZ, SALID1) %>%
      count(CLZ, name="n_cities") %>%
      mutate(Country="Total")) %>%
  group_by(Country) %>%
  mutate(total=sum(n_cities),
         pct_clz=100*n_cities/total) %>%
  ungroup() %>%
  select(Country, CLZ, pct_clz) %>%
  pivot_wider(names_from=Country,
              values_from=pct_clz,
              values_fill=0) %>%
  mutate(across(-CLZ, ~format(round(.x,1), nsmall=1))) %>%
  rename(variables=CLZ) %>%
  to_char()

# 6. Cities variables
var <- c("pop_over65","pop_density_guf","GDP",
         "education","urban_area_pct","NDVI")

result_list <- lapply(var,function(v){
  
  decimals <- ifelse(v=="NDVI",2,1)
  
  by_country <- city_level %>%
    group_by(Country) %>%
    summarise(
      med=median(.data[[v]],na.rm=TRUE),
      p10=quantile(.data[[v]],0.1,na.rm=TRUE),
      p90=quantile(.data[[v]],0.9,na.rm=TRUE),
      .groups="drop")
  
  total <- city_level %>%
    summarise(
      Country="Total",
      med=median(.data[[v]],na.rm=TRUE),
      p10=quantile(.data[[v]],0.1,na.rm=TRUE),
      p90=quantile(.data[[v]],0.9,na.rm=TRUE))
  
  bind_rows(by_country,total) %>%
    mutate(formatted=glue("{format(round(med,decimals),nsmall=decimals)} ({format(round(p10,decimals),nsmall=decimals)}, {format(round(p90,decimals),nsmall=decimals)})")) %>%
    select(Country,formatted) %>%
    pivot_wider(names_from=Country,values_from=formatted) %>%
    mutate(variables=v,.before=1)
})

sup_table_2e <- bind_rows(result_list)

# 7. Join tables
sup_table2 <- bind_rows(sup_table_2a, sup_table_2c, sup_table_2b, sup_table_2e, sup_table_2d)

# 8. Prepare to final table 1
var_labels <- c(
  n_cities        = "Number of cities",
  HWN_EHF         = "Annual HWN (EHF)*",
  HWF_EHF         = "Annual HWF (EHF)*",
  HWD_EHF         = "Annual HWD (EHF)*",
  city_pop        = "City population (thousands)",
  pop_density_guf = "Population density (1000 people per km2)",
  GDP             = "GDP per capita (US$ thousands)",
  pop_over65      = "Population ≥65 years (%)",
  education       = "Completed primary education (%)",
  urban_area_pct  = "Urban area (%)",
  NDVI            = "Greenness (NDVI)")

sup_table2_names <- sup_table2 %>%
  mutate(variables = dplyr::recode(variables, !!!as.list(var_labels)))

# add lines
sup_table2_names <- sup_table2_names %>%
  add_row(variables = "City social and environmental characteristics**",
          .before = which(sup_table2_names$variables == "City population (thousands)"))

sup_table2_names <- sup_table2_names %>%
  add_row(variables = "Climate Zones (%n of cities)",
          .before = which(sup_table2_names$variables %in% unique(sup_table_2d$variables))[1])
# note
col_1 <- names(sup_table2_names)[1]

sup_table2_names <- sup_table2_names %>%
  add_row(!!col_1 := "* Values represent the median (10th, 90th percentiles) calculated from all annual city observations (2000–2024).
          ** Values represent the median (10th, 90th percentiles) of city-specific means within each country across the study period (2000–2024).")

# organize colunms and rename
sup_table2_names <- sup_table2_names %>%
  select("variables", "Total", "Argentina", "Brazil", "Central America", 
         "Chile", "Colombia", "Mexico", "Peru") %>% 
  rename("  " = variables)

# 9. export as excel
write_xlsx(sup_table2_names, "Tables/Sup_Table_2.xlsx")

#-----------------------------------------------------
# 2. Sup table 3
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
footer_note <- "Note: Values represent the median (10th, 90th percentiles) calculated from all annual city observations (2000–2024)."
footer_row <- nrow(table_all_hw) + 3
writeData(wb, sheet = "HW_stats", x = footer_note, startRow = footer_row, startCol = 1)

saveWorkbook(wb, file = "Tables/Sup_table_3.xlsx", overwrite = TRUE)

#-----------------------------------------------------
# 3. Sup table 4
#-----------------------------------------------------
data_py <- read_parquet("Data/data_final_model.parquet")

person_years_vars  <- names(data_py)[grepl("^py_H", names(data_py))]
person_events_vars <- names(data_py)[grepl("^person_events_H", names(data_py))]

pop_country_total <- data_py %>%
  group_by(Country) %>%
  summarise(across(all_of(c(person_years_vars, person_events_vars)),
                   ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")
sum(pop_country_total$py_HWF_EHF)

# % by country and total
make_wide_with_share <- function(data_total, vars) {
  data_total %>%
    select(Country, all_of(vars)) %>%
    pivot_longer(cols = all_of(vars),
                 names_to  = "metric",
                 values_to = "value") %>%
    group_by(metric) %>%
    mutate(
      Total = sum(value, na.rm = TRUE),
      pct   = 100 * value / Total) %>%
    ungroup() %>%
    mutate(
      pct   = paste0(round(pct, 1), "%"),
      Total = round(Total / 1e6, 1)) %>%
    select(metric, Country, pct, Total) %>%
    pivot_wider(
      names_from  = Country,
      values_from = pct) %>%
    mutate(Total = unique(Total)) %>%
    select(metric, Total, everything())
}

table_person_days   <- make_wide_with_share(pop_country_total, person_years_vars)
table_person_events <- make_wide_with_share(pop_country_total, person_events_vars)

table_person_days_final <- table_person_days %>%
  mutate(metric = gsub("^py_HWF_", "", metric))

table_person_events_final <- table_person_events %>%
  mutate(metric = gsub("^person_events_HWN_", "", metric))

table_person_days_final <- table_person_days_final %>%
  mutate(across(everything(), as.character))

table_person_events_final <- table_person_events_final %>%
  mutate(across(everything(), as.character))

person_days_row <- table_person_days_final[1, ]
person_days_row[1, ] <- "Person-years exposure"
person_days_row[-1] <- ""

table_person_days_final <- bind_rows(
  person_days_row,
  table_person_days_final)

person_events_row <- table_person_events_final[1, ]
person_events_row[1, ] <- "Person-events exposure"
person_events_row[-1] <- ""

table_person_events_final <- bind_rows(
  person_events_row,
  table_person_events_final)

# Join
sup_table_final_4 <- bind_rows(
  table_person_days_final,
  table_person_events_final) %>% 
  rename("Total (millions)" = Total)

# export
write_xlsx(sup_table_final_4, "Tables/Sup_table_4.xlsx")

#-----------------------------------------------------
# 4. Sup table 5
#-----------------------------------------------------
HWN_trends <- read_xlsx("Model_results/HWN_trends.xlsx")
HWF_trends <- read_xlsx("Model_results/HWF_trends.xlsx")
HWD_trends <- read_xlsx("Model_results/HWD_trends.xlsx")

# join
data_trends <- rbind(HWN_trends, HWF_trends, HWD_trends)

# note and export
note_row <- tibble(
  Model = "Note: * statistically significant (p < 0.05). HWN = number of heatwaves; HWF = heatwave frequency; HWD = heatwave duration.")

data_trends_excel <- bind_rows(data_trends, note_row)
data_trends_excel <- data_trends_excel %>% 
  rename(`Estimate (95% CI)` = "Estimate..95..CI.",
         "  " = Model)

write_xlsx(data_trends_excel, "Tables/Sup_table_5.xlsx")

#-----------------------------------------------------
# 5. Sup table 6
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
         "Differences at baseline IRR (95% CI) per decade - HWF" = "Differences at baseline IRR (95% CI) per decade",
         "Temporal trend IRR (95% CI) per decade - HWF" =  "Temporal trend IRR (95% CI) per decade") %>% 
  mutate(model = sub("^HWF_", "", model)) 

HWD_CLZ <- HWD_CLZ %>%
  
  rename("model" = "...1", 
         "clz" = "...2",
         "Differences at baseline IRR (95% CI) per decade - HWD" = "Differences at baseline IRR (95% CI) per decade",
         "Temporal trend IRR (95% CI) per decade - HWD" =  "Temporal trend IRR (95% CI) per decade") %>% 
  mutate(model = sub("^HWD_", "", model)) 

CLZ_all <- bind_cols(HWN_CLZ, HWF_CLZ, HWD_CLZ)

CLZ_all <- CLZ_all %>%
  select("...1", "...2",
         "Differences at baseline IRR (95% CI) per decade", "Temporal trend IRR (95% CI) per decade",
         "Differences at baseline IRR (95% CI) per decade - HWF", "Temporal trend IRR (95% CI) per decade - HWF",
         "Differences at baseline IRR (95% CI) per decade - HWD", "Temporal trend IRR (95% CI) per decade - HWD")

note_row6 <- tibble("...1" = "Note: * statistically significant (p<0.05)")

CLZ_final_excel <- bind_rows(CLZ_all, note_row6)

CLZ_final_excel <- CLZ_final_excel %>%
  rename("  " = "...1", "    " = "...2")

write_xlsx(CLZ_final_excel, "Tables/Sup_table_6.xlsx")

#-----------------------------------------------------
# 7. Model  - Sup table 7
#-----------------------------------------------------
HWN_lat <- read_xlsx("Model_results/lat_HWN.xlsx")
HWF_lat <- read_xlsx("Model_results/lat_HWF.xlsx")
HWD_lat <- read_xlsx("Model_results/lat_HWD.xlsx")

HWN_lat <- HWN_lat %>%
  mutate(Model = sub("^HWN_", "", Model)) %>% 
  select(1, 2, 4, 5) %>% 
  add_row(Model = "HWF", .before = Inf) #for the next table

HWF_lat <- HWF_lat %>%
  mutate(Model = sub("^HWF_", "", Model)) %>% 
  select(1, 2, 4, 5) %>%
  add_row(Model = "HWD",.before = Inf)

HWD_lat <- HWD_lat %>%
  mutate(Model = sub("^HWD_", "", Model)) %>% 
  select(1, 2, 4, 5)

# join tables
latitude_final <- bind_rows(HWN_lat, HWF_lat, HWD_lat)

latitude_final <- latitude_final %>% 
  rename("  " = "Model")

note_row7a <- tibble("  " = "Note: * statistically significant (p<0.05)")
note_row7b <- tibble("  " = "      + Models with convergence warnings")

latitude_final_excel <- bind_rows(latitude_final, note_row7a, note_row7b)

write_xlsx(latitude_final_excel, "Tables/Sup_table_7.xlsx")

#-----------------------------------------------------
# 7. Model  - Sup table 8
#-----------------------------------------------------
HWN_hybrid <- read_xlsx("Model_results/HWN_hybrid_models.xlsx")
HWF_hybrid <- read_xlsx("Model_results/HWF_hybrid_models.xlsx")
HWD_hybrid <- read_xlsx("Model_results/HWD_hybrid_models.xlsx")

str(HWN_hybrid)
# separate tables and filter the results
# HWN
HWN_hybrid_btw <- HWN_hybrid %>%
  filter(str_detect(term, "_between_")) %>% 
  rename(Between_HWN = final) %>% 
  mutate(dep_var = sub("^HWN_", "", dep_var)) %>% 
  select(dep_var, covariate, Between_HWN)

HWN_hybrid_wt <- HWN_hybrid %>%
  filter(str_detect(term, "_within_")) %>% 
  rename(Within_HWN = final) %>% 
  mutate(dep_var = sub("^HWN_", "", dep_var)) %>% 
  select(dep_var, covariate, Within_HWN)

#HWF
HWF_hybrid_btw <- HWF_hybrid %>%
  filter(str_detect(term, "_between_")) %>% 
  rename(Between_HWF = final) %>% 
  mutate(dep_var = sub("^HWF_", "", dep_var)) %>% 
  select(dep_var, covariate, Between_HWF)

HWF_hybrid_wt <- HWF_hybrid %>%
  filter(str_detect(term, "_within_")) %>% 
  rename(Within_HWF = final) %>% 
  mutate(dep_var = sub("^HWF_", "", dep_var)) %>% 
  select(dep_var, covariate, Within_HWF)

#HWD
HWD_hybrid_btw <- HWD_hybrid %>%
  filter(str_detect(term, "_between_")) %>% 
  rename(Between_HWD = final) %>% 
  mutate(dep_var = sub("^HWD_", "", dep_var)) %>% 
  select(dep_var, covariate, Between_HWD)

HWD_hybrid_wt <- HWD_hybrid %>%
  filter(str_detect(term, "_within_")) %>% 
  rename(Within_HWD = final) %>% 
  mutate(dep_var = sub("^HWD_", "", dep_var)) %>% 
  select(dep_var, covariate, Within_HWD)

# join
HWN_hybrid_FINAL <- HWN_hybrid_btw %>% left_join(HWN_hybrid_wt, by =c("dep_var", "covariate"))
HWF_hybrid_FINAL <- HWF_hybrid_btw %>% left_join(HWF_hybrid_wt, by =c("dep_var", "covariate"))
HWD_hybrid_FINAL <- HWD_hybrid_btw %>% left_join(HWD_hybrid_wt, by =c("dep_var", "covariate"))

hybrid_all <- HWN_hybrid_FINAL %>% 
  left_join(HWF_hybrid_FINAL, by = c("dep_var", "covariate")) %>%
  left_join(HWD_hybrid_FINAL, by = c("dep_var", "covariate"))

# organize
hybrid_all <- hybrid_all %>% 
    mutate(covariate = case_when(
      covariate == "total_pop" ~ "City population",
      covariate == "pop_density" ~ "Population density",
      covariate == "GDP" ~ "GDP per capita",
      covariate == "pop_over65" ~ "Population ≥65 years",
      covariate == "education" ~ "Completed primary education",
      covariate == "urban_area_pct" ~ "Urban area",
      covariate == "NDVI" ~ "NDVI",
    TRUE ~ covariate)) %>% 
  rename("Heatwave definition" = dep_var, "City characteristic" = covariate) 
  
note_row8a <- tibble("Heatwave definition" = "Note: * statistically significant (p<0.05)")
note_row8b <- tibble("Heatwave definition" = "      + Models with convergence warnings
                     Estimates are expressed per 1-SD higher standardized value of each continuous city-level predictor.")

Sup_table_8 <- bind_rows(hybrid_all, note_row8a, note_row8b)

Sup_table_8_final <- Sup_table_8 %>%
  group_by(`Heatwave definition`) %>%
  mutate(`Heatwave definition` = if_else(row_number() == 1, `Heatwave definition`, NA_character_)) %>%
  ungroup()

write_xlsx(Sup_table_8_final, "Tables/Sup_table_8.xlsx")
