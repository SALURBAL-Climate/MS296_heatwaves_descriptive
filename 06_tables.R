##################################################################
# MS296: Descriptive Heatwaves - L1AD
#
# This script creates the manuscript tables
##################################################################

library(tidyverse); library(dplyr); library(arrow)
library(glue); library(rlang); library(writexl)
library(flextable)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296")

#---------------------------------------------
# Table 1
#---------------------------------------------
data <- read_parquet("Data/data_final_model.parquet")

# 1. prepare to add total and country as columns

add_total <- function(data, summary_expr, value_name) {
  
  by_country <- data %>%
    summarize(!!value_name := {{ summary_expr }}, .by = Country)
  
  total <- data %>%
    summarize(!!value_name := {{ summary_expr }}) %>%
    mutate(Country = "Total")
  bind_rows(by_country, total) %>%
    pivot_wider(
      names_from = Country,
      values_from = !!sym(value_name))
}

to_char <- function(df) {
  df %>% mutate(across(-variables, as.character))}

# 2. Number of cities
table_1a <- add_total(data, n_distinct(SALID1),"n_cities") %>%
  mutate(variables = "n_cities", .before = 1) %>%
  to_char()

# 3. City population
base_pop <- data %>% 
  summarize(pop = first(total_pop), .by = c(Country, SALID1, YEAR)) %>% 
  summarize(pop = pop/1000, .by = c(Country, SALID1, YEAR)) %>% 
  summarize(pop = mean(pop), .by = c(Country, SALID1))

table_1b <- add_total(base_pop,
  glue("{round(median(pop))} ({round(quantile(pop,.1))}, {round(quantile(pop,.9))})"),
  "city_pop") %>%
  mutate(variables = "city_pop", .before = 1) %>%
  to_char()

# 4. Climate zones
table_1d <- data %>%
  distinct(Country, CLZ, SALID1) %>% 
  count(Country, CLZ, name = "n_cities") %>%
  bind_rows(data %>%
              distinct(Country, CLZ, SALID1) %>% 
              count(CLZ, name = "n_cities") %>%
              mutate(Country = "Total")) %>%
  group_by(Country) %>%
  mutate(total = sum(n_cities), pct_clz = 100 * n_cities / total) %>%
  ungroup() %>%
  select(Country, CLZ, pct_clz) %>%
  pivot_wider(names_from = Country,
              values_from = pct_clz,
              values_fill = 0) %>%
  mutate(across(-CLZ, ~ format(round(.x, 1), nsmall = 1))) %>%
  rename(variables = CLZ) %>%
  to_char()

# 5. HWN, HWF, and HWD
table_1e <- bind_rows(
  add_total(data,
            glue("{round(median(HWN_EHF),0)} ({round(quantile(HWN_EHF,.1),0)}, {round(quantile(HWN_EHF,.9),0)})"),"HWN_EHF") %>%
    mutate(variables = "HWN_EHF", .before = 1),
  add_total(data,
            glue("{round(median(HWF_EHF),0)} ({round(quantile(HWF_EHF,.1),0)}, {round(quantile(HWF_EHF,.9),0)})"),"HWF_EHF") %>%
    mutate(variables = "HWF_EHF", .before = 1),
  add_total(data,
            glue("{round(median(HWD_EHF),0)} ({round(quantile(HWD_EHF,.1),0)}, {round(quantile(HWD_EHF,.9),0)})"),"HWD_EHF") %>%
  mutate(variables = "HWD_EHF", .before = 1)) %>% 
  to_char()


# 6. Cities variables
data <- data %>%
  mutate(GDP = GDP / 1000,
         pop_density_guf = pop_density_guf / 1000,
         pop_over65 = pop_over65 * 100)

var <- c("pop_over65",
         "pop_density_guf",
         "GDP",
         "education",
         "urban_area_pct",
         "NDVI")

result_list <- lapply(var, function(v) {
  
  decimals <- ifelse(v %in% c("NDVI"), 2, 1)
  base <- data %>% 
    summarize(mean_val = mean(.data[[v]], na.rm = TRUE), .by = c(Country, SALID1))
  add_total(base,
            glue("{format(round(median(mean_val), decimals), nsmall = decimals)} ",
                 "({format(round(quantile(mean_val, .1), decimals), nsmall = decimals)}, ",
                 "{format(round(quantile(mean_val, .9), decimals), nsmall = decimals)})"), v) %>%
    mutate(variables = v, .before = 1) %>%
    to_char()
})

table_1g <- bind_rows(result_list)

# 7. Join tables
table1 <- bind_rows(table_1a, table_1e, table_1b, table_1g, table_1d)

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

table1_names <- table1 %>%
  mutate(variables = dplyr::recode(variables, !!!as.list(var_labels)))

# add lines
table1_names <- table1_names %>%
  add_row(variables = "City social and environmental characteristics*",
          .before = which(table1_names$variables == "City population (thousands)"))

table1_names <- table1_names %>%
  add_row(variables = "Climate Zones (%n cities)",
          .before = which(table1_names$variables %in% unique(table_1d$variables))[1])
# note
col_1 <- names(table1_names)[1]

table1_names <- table1_names %>%
  add_row(!!col_1 := "* Values represent the median (10th, 90th percentiles) across cities within each country")

# organize colunms and rename
table1_names <- table1_names %>%
  select("variables", "Total", "Argentina", "Brazil", "Central America", 
         "Chile", "Colombia", "Mexico", "Peru") %>% 
  rename("  " = variables)

# 9. export as excel
write_xlsx(table1_names, "Tables/Table_1.xlsx")

#---------------------------------------------
# Table 2
#---------------------------------------------
HWN_CLZ <- read_xlsx("Model_results/CLZ_HWN.xlsx")

str(HWN_CLZ)

# select only the results for HWN - EHF and select columns
table_2 <- HWN_CLZ %>% 
  slice(1:4) %>% 
  select("...2", "Differences at baseline IRR (95% CI)", "Changes over time IRR (95% CI)")

note_row2 <- tibble("...2" = "Note: * statistically significant (p<0.05)")

CLZ_final_excel <- bind_rows(table_2, note_row2)

CLZ_final_excel <- CLZ_final_excel %>%
  rename("  " = "...1")

#  export as excel
write_xlsx(CLZ_final_excel, "Tables/Table_2.xlsx")

#---------------------------------------------
# Table 3
#---------------------------------------------
univariable_HWN <- read_xlsx("Model_results/univariable_HWN.xlsx")

table_3 <- univariable_HWN %>% 
  filter(Heatwave == "EHF") %>% 
  select("City characteristic", "Univariable estimate (CI 95%)")

note_row3 <- tibble("City characteristic" = "Note: * statistically significant (p<0.05)")
table3_final <- bind_rows(table_3, note_row3)

table3_final <- table3_final %>%
  rename("  " = "City characteristic")

#  export as excel
write_xlsx(table3_final, "Tables/Table_3.xlsx")

#---------------------------------------------
# Table 4
#---------------------------------------------
hybrid_HWN <- read_xlsx("Model_results/HWN_hybrid_models.xlsx")

hybrid_HWN <- hybrid_HWN %>% 
  filter(outcome == "HWN_EHF")

HWN_hybrid_btw <- hybrid_HWN %>%
  filter(str_detect(term, "_between_")) %>% 
  rename("Between-city difference in IRR (CI 95%)" = final) %>% 
  select(predictor, "Between-city difference in IRR (CI 95%)")

HWN_hybrid_wt <- hybrid_HWN %>%
  filter(str_detect(term, "_within_")) %>% 
  rename("Within-city difference in IRR (CI 95%)" = final) %>% 
  select(predictor, "Within-city difference in IRR (CI 95%)")

table4 <- HWN_hybrid_btw %>% 
  left_join(HWN_hybrid_wt, by =c("predictor"))

# organize
table4 <- table4 %>% 
  mutate(predictor = case_when(
    predictor == "total_pop" ~ "City population",
    predictor == "pop_density" ~ "Population density",
    predictor == "GDP" ~ "GDP per capita",
    predictor == "pop_over65" ~ "Population ≥65 years (%)",
    predictor == "education" ~ "Completed primary education (%)",
    predictor == "urban_area_pct" ~ "Urban area (%)",
    predictor == "NDVI" ~ "NDVI",
    TRUE ~ predictor)) %>% 
  rename("City characteristic" = predictor) 


note_row4 <- tibble("City characteristic" = "Note: * statistically significant (p<0.05)")

table4_final <- bind_rows(table4, note_row4)

#  export as excel
write_xlsx(table4_final, "Tables/Table_4.xlsx")
