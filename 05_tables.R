##################################################################
# MS296: Descriptive Heatwaves - L1AD
#
# This script creates the manuscript tables
##################################################################

library(tidyverse); library(dplyr); library(arrow)
library(glue); library(rlang); library(writexl)
library(flextable); library(readxl)

#---------------------------------------------
# Table 1
#---------------------------------------------
data <- read_parquet("Data/data_final_model.parquet")

pop_country <- data %>%
  group_by(Country, YEAR) %>%
  summarise(pop_total   = sum(total_pop, na.rm = TRUE),
            pop_exposed = sum(pop_exposed_HWN_EHF, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(pct_exposed = 100 * pop_exposed / pop_total)

pop_overall <- data %>%
  group_by(YEAR) %>%
  summarise(`Total population` = sum(total_pop, na.rm = TRUE),
            `Population exposed (%)` = 100 * sum(pop_exposed_HWN_EHF, na.rm = TRUE) / 
              sum(total_pop, na.rm = TRUE), .groups = "drop")

pop_country_wide <- pop_country %>%
  select(YEAR, Country, pct_exposed) %>%
  pivot_wider(names_from = Country,
              values_from = pct_exposed)

table_1 <- pop_overall %>%
  left_join(pop_country_wide, by = "YEAR") %>%
  mutate(`Total population` = format(round(`Total population` / 1000),
                                     big.mark = ",", scientific = FALSE),
         across(where(is.numeric) & !YEAR, 
                ~ ifelse(abs(. - 100) < 1e-9, "100%",
                         paste0(sprintf("%.1f", .), "%")))) %>%
  rename(Year = YEAR, `Total population (thousands)` = `Total population`)

note_row1 <- tibble(Year = "Note: Population exposed (%) represents the percentage of the total population living in study cities exposed to at least one heatwave event each year. Country-specific percentages represent the percentage of the population living in the study cities within each country exposed in that year.")

table_1$Year <- as.character(table_1$Year)
table_1_final_excel <- bind_rows(table_1, note_row1)

# Export
write_xlsx(table_1_final_excel, "Tables/Table_1.xlsx")

#---------------------------------------------
# Table 2
#---------------------------------------------
HWN_CLZ <- read_xlsx("Model_results/CLZ_HWN.xlsx")

# select only the results for HWN - EHF and select columns
table_2 <- HWN_CLZ %>% 
  slice(1:3) %>% 
  rename(clz = "...2") %>% 
  select("clz", "Differences at baseline IRR (95% CI) per decade", "Temporal trend IRR (95% CI) per decade")

table_2 <- table_2 %>%
  mutate(clz = recode(clz,
                      "Arid" = "Arid (76)",
                      "Temperate" = "Temperate (123)",
                      "Tropical" = "Tropical (154)"))

note_row2 <- tibble(clz = "Note: * statistically significant (p<0.05)")
note_row2a <- tibble(clz =  "For the reference category (Tropical), the value corresponds to the main effect of time only.")

table_2_final_excel <- bind_rows(table_2, note_row2, note_row2a)

table_2_final_excel <- table_2_final_excel %>%
  rename("Climate zones (n of cities)" = clz)

#  export as excel
write_xlsx(table_2_final_excel, "Tables/Table_2.xlsx")

# check n of cities
# clz_n <- read_parquet("Data/data_final_model.parquet")
# 
# clz_number <- clz_n %>%
#   distinct(SALID1, CLZ) %>%
#   count(CLZ, name = "n_cities") %>%
#   rename(variables = CLZ)

#---------------------------------------------
# Table 3
#---------------------------------------------
hybrid_HWN <- read_xlsx("Model_results/HWN_hybrid_models.xlsx")

hybrid_HWN <- hybrid_HWN %>% 
  filter(dep_var == "HWN_EHF")

HWN_hybrid_btw <- hybrid_HWN %>%
  filter(str_detect(term, "_between_")) %>% 
  rename("Between-city difference in IRR (CI 95%) per decade" = final) %>% 
  select(covariate, "Between-city difference in IRR (CI 95%) per decade")

HWN_hybrid_wt <- hybrid_HWN %>%
  filter(str_detect(term, "_within_")) %>% 
  rename("Within-city difference in IRR (CI 95%) per decade" = final) %>% 
  select(covariate, "Within-city difference in IRR (CI 95%) per decade")

hybrid_table <- HWN_hybrid_btw %>% 
  left_join(HWN_hybrid_wt, by ="covariate")

# organize
table3 <- hybrid_table %>% 
  mutate(covariate = case_when(
    covariate == "total_pop" ~ "City population (thousands)",
    covariate == "pop_density" ~ "Population density (1000 people per km2)",
    covariate == "GDP" ~ "GDP per capita (US$ thousands)",
    covariate == "pop_over65" ~ "Population ≥65 years (%)",
    covariate == "education" ~ "Completed primary education (%)",
    covariate == "urban_area_pct" ~ "Urban area (%)",
    covariate == "NDVI" ~ "NDVI",
    TRUE ~ covariate)) %>% 
  rename("City characteristic" = covariate) 

note_row3 <- tibble("City characteristic" = "Note: * statistically significant (p<0.05);")
note_row3a <- tibble("City characteristic" = "Estimates are expressed per 1-SD higher standardized value of each continuous city-level predictor;")
note_row3b <- tibble("City characteristic" = "Each city-level characteristic was evaluated in a separate model.")

table3_final <- bind_rows(table3, note_row3, note_row3a, note_row3b)

#  export as excel
write_xlsx(table3_final, "Tables/Table_3.xlsx")
