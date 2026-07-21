##################################################################
# MS296: Descriptive Heatwaves- L1AD level
#
# This script plot supplementary figures
##################################################################

library(dplyr); library(arrow); library(RColorBrewer)
library(tidyr); library(tidyverse); library(gtsummary)
library(rlang); library(ggthemes); library(ggplot2)
library(flextable); library(purrr); library(sf)
library(ggspatial); library(RColorBrewer)
library(patchwork); library(ggsn)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296")

data <- read_parquet("Data/data_hw_descriptive.parquet")
data_pop <- read_parquet("Data/data_final_model.parquet")

shapefile <- st_read("Data/SHP/L1AD_city_center.shp")
shp_SALURBAL_countries <- st_read("Data/SHP/LA_countries.shp")
shp_base <- st_read("Data/SHP/LA_base.shp")

shp_SALURBAL_countries <- st_transform(shp_SALURBAL_countries, crs = 4326)

# Join
#shapefile$SALID1 <- as.character(shapefile$SALID1)
data_map <- data %>% left_join(shapefile, by = c("SALID1"))

#---------------------------------------------
# Supplementary figure 1
#---------------------------------------------
# 1. Box plots - HWN
HWN_country_90P_2d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWN_90P_2d),
    color = "#fb6a4a", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWN_90P_2d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 36) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "A1") +
  xlab("") +
  ylab("HWN_90P_2d")

HWN_country_90P_3d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWN_90P_3d),
    color = "#fb6a4a", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWN_90P_3d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 36) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "A2") +
  xlab("") +
  ylab("HWN_90P_3d")

HWN_country_90P_4d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWN_90P_4d),
    color = "#fb6a4a", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  # Depois as caixas (agora ficarão na frente)
  geom_boxplot(
    aes(x = Country, y = HWN_90P_4d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 36) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "A3") +
  xlab("") +
  ylab("HWN_90P_4d")

HWN_country_95P_2d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWN_95P_2d),
    color = "#ef3b2c", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWN_95P_2d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 24) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "B1") +
  xlab("") +
  ylab("HWN_95P_2d")

HWN_country_95P_3d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWN_95P_3d),
    color = "#ef3b2c", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWN_95P_3d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 24) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "B2") +
  xlab("") +
  ylab("HWN_95P_3d")

HWN_country_95P_4d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWN_95P_4d),
    color = "#ef3b2c", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  # Depois as caixas (agora ficarão na frente)
  geom_boxplot(
    aes(x = Country, y = HWN_95P_4d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 24) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "B3") +
  xlab("") +
  ylab("HWN_95P_4d")

HWN_country_97.5P_2d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWN_97.5P_2d),
    color = "#cb181d", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWN_97.5P_2d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 18) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "C1") +
  xlab("") +
  ylab("HWN_97.5P_2d")

HWN_country_97.5P_3d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWN_97.5P_3d),
    color = "#cb181d", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWN_97.5P_3d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 18) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "C2") +
  xlab("") +
  ylab("HWN_97.5P_3d")

HWN_country_97.5P_4d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWN_97.5P_4d),
    color = "#cb181d", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  # Depois as caixas (agora ficarão na frente)
  geom_boxplot(
    aes(x = Country, y = HWN_97.5P_4d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 18) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "C3") +
  xlab("") +
  ylab("HWN_97.5P_4d")

# Combine plots
combined_plot <- (HWN_country_90P_2d | HWN_country_90P_3d | HWN_country_90P_4d) /
  (HWN_country_95P_2d | HWN_country_95P_3d | HWN_country_95P_4d) /
  (HWN_country_97.5P_2d | HWN_country_97.5P_3d | HWN_country_97.5P_4d)

combined_plot

# Export
ggsave("Figures/Sup_fig_1.png", plot = combined_plot, width = 20, height = 15, dpi = 300, bg = "white")

#---------------------------------------------
# Supplementary figure - 2
#---------------------------------------------
# 2. Box plots - HWF
HWF_country_90P_2d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWF_90P_2d),
    color = "#fb6a4a", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWF_90P_2d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 200) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "A1") +
  xlab("") +
  ylab("HWF_90P_2d")

HWF_country_90P_3d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWF_90P_3d),
    color = "#fb6a4a", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWF_90P_3d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 200) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "A2") +
  xlab("") +
  ylab("HWF_90P_3d")

HWF_country_90P_4d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWF_90P_4d),
    color = "#fb6a4a", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  # Depois as caixas (agora ficarão na frente)
  geom_boxplot(
    aes(x = Country, y = HWF_90P_4d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 200) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "A3") +
  xlab("") +
  ylab("HWF_90P_4d")

HWF_country_95P_2d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWF_95P_2d),
    color = "#ef3b2c", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWF_95P_2d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 130) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "B1") +
  xlab("") +
  ylab("HWF_95P_2d")

HWF_country_95P_3d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWF_95P_3d),
    color = "#ef3b2c", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWF_95P_3d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 130) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "B2") +
  xlab("") +
  ylab("HWF_95P_3d")

HWF_country_95P_4d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWF_95P_4d),
    color = "#ef3b2c", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  # Depois as caixas (agora ficarão na frente)
  geom_boxplot(
    aes(x = Country, y = HWF_95P_4d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 130) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "B3") +
  xlab("") +
  ylab("HWF_95P_4d")

HWF_country_97.5P_2d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWF_97.5P_2d),
    color = "#cb181d", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWF_97.5P_2d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 84) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "C1") +
  xlab("") +
  ylab("HWF_97.5P_2d")

HWF_country_97.5P_3d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWF_97.5P_3d),
    color = "#cb181d", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  geom_boxplot(
    aes(x = Country, y = HWF_97.5P_3d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 84) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "C2") +
  xlab("") +
  ylab("HWF_97.5P_3d")

HWF_country_97.5P_4d <- data %>% 
  ggplot() +
  geom_jitter(
    aes(x = Country, y = HWF_97.5P_4d),
    color = "#cb181d", width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
  # Depois as caixas (agora ficarão na frente)
  geom_boxplot(
    aes(x = Country, y = HWF_97.5P_4d),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 84) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "C3") +
  xlab("") +
  ylab("HWF_97.5P_4d")

# Combine plots
combined_plot2 <- (HWF_country_90P_2d | HWF_country_90P_3d | HWF_country_90P_4d) /
  (HWF_country_95P_2d | HWF_country_95P_3d | HWF_country_95P_4d) /
  (HWF_country_97.5P_2d | HWF_country_97.5P_3d | HWF_country_97.5P_4d)

combined_plot2

# Export
ggsave("Figures/Sup_fig_2.png", plot = combined_plot2, width = 20, height = 15, dpi = 300, bg = "white")

#---------------------------------------------
# Supplementary figures - 3 to 5
#---------------------------------------------
# 3. Lines plot - HWN
df_country_avg <- data %>%
  group_by(Country, YEAR) %>%
  summarize(
    mean_HWN_90P_2d = mean(HWN_90P_2d, na.rm = TRUE),
    mean_HWN_90P_3d = mean(HWN_90P_3d, na.rm = TRUE),
    mean_HWN_90P_4d = mean(HWN_90P_4d, na.rm = TRUE),
    mean_HWN_95P_2d = mean(HWN_95P_2d, na.rm = TRUE),
    mean_HWN_95P_3d = mean(HWN_95P_3d, na.rm = TRUE),
    mean_HWN_95P_4d = mean(HWN_95P_4d, na.rm = TRUE),
    mean_HWN_97.5P_2d = mean(HWN_97.5P_2d, na.rm = TRUE),
    mean_HWN_97.5P_3d = mean(HWN_97.5P_3d, na.rm = TRUE),
    mean_HWN_97.5P_4d = mean(HWN_97.5P_4d, na.rm = TRUE),
    .groups = "drop")

line_90P_2d <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_90P_2d, group = SALID1), 
            color = "#fb6a4a", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_90P_2d), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 36)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "HWN_90P_2d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_90P_3d <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_90P_3d, group = SALID1), 
            color = "#fb6a4a", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_90P_3d), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 36)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "HWN_90P_3d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_90P_4d <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_90P_4d, group = SALID1), 
            color = "#fb6a4a", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_90P_4d), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 36)) +
  labs(title = "C") +
  labs(x = "Year",
       y = "HWN_90P_4d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_95P_2d <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_95P_2d, group = SALID1), 
            color = "#fb6a4a", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_95P_2d), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 24)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "HWN_95P_2d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_95P_3d <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_95P_3d, group = SALID1), 
            color = "#fb6a4a", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_95P_3d), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 24)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "HWN_95P_3d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_95P_4d <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_95P_4d, group = SALID1), 
            color = "#fb6a4a", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_95P_4d), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 24)) +
  labs(title = "C") +
  labs(x = "Year",
       y = "HWN_95P_4d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_97.5P_2d <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_97.5P_2d, group = SALID1), 
            color = "#fb6a4a", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_97.5P_2d), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 16)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "HWN_97.5P_2d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_97.5P_3d <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_97.5P_3d, group = SALID1), 
            color = "#fb6a4a", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_97.5P_3d), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 16)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "HWN_97.5P_3d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_97.5P_4d <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_97.5P_4d, group = SALID1), 
            color = "#fb6a4a", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_97.5P_4d), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 16)) +
  labs(title = "C") +
  labs(x = "Year",
       y = "HWN_97.5P_4d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

# Combine plots and export
combined_plot_6 <- (line_90P_2d) / (line_90P_3d) / (line_90P_4d)
ggsave(filename = "Figures/Sup_fig_3.png",
       width = 30, height = 18, dpi = 300, bg = "white")  

# Combine plots
combined_plot_7 <- (line_95P_2d) / (line_95P_3d) / (line_95P_4d)
ggsave(filename = "Figures/Sup_fig_4.png",
       width = 30, height = 18, dpi = 300, bg = "white")  

# Combine plots
combined_plot_8 <- (line_97.5P_2d) / (line_97.5P_3d) / (line_97.5P_4d)
ggsave(filename = "Figures/Sup_fig_5.png",
       width = 30, height = 18, dpi = 300, bg = "white")  

#---------------------------------------------
# Supplementary figures 6 to 8
#---------------------------------------------
# HWF
df_country_avg_F <- data %>%
  group_by(Country, YEAR) %>%
  summarize(
    mean_HWF_90P_2d = mean(HWF_90P_2d, na.rm = TRUE),
    mean_HWF_90P_3d = mean(HWF_90P_3d, na.rm = TRUE),
    mean_HWF_90P_4d = mean(HWF_90P_4d, na.rm = TRUE),
    mean_HWF_95P_2d = mean(HWF_95P_2d, na.rm = TRUE),
    mean_HWF_95P_3d = mean(HWF_95P_3d, na.rm = TRUE),
    mean_HWF_95P_4d = mean(HWF_95P_4d, na.rm = TRUE),
    mean_HWF_97.5P_2d = mean(HWF_97.5P_2d, na.rm = TRUE),
    mean_HWF_97.5P_3d = mean(HWF_97.5P_3d, na.rm = TRUE),
    mean_HWF_97.5P_4d = mean(HWF_97.5P_4d, na.rm = TRUE),
    .groups = "drop")

line_90P_2d_F <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_90P_2d, group = SALID1), 
            color = "#ef3b2c", alpha = 0.4) +
  geom_line(data = df_country_avg_F, aes(x = YEAR, y = mean_HWF_90P_2d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 200)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "HWF_90P_2d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_90P_3d_F <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_90P_3d, group = SALID1), 
            color = "#ef3b2c", alpha = 0.4) +
  geom_line(data = df_country_avg_F, aes(x = YEAR, y = mean_HWF_90P_3d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 200)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "HWF_90P_3d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_90P_4d_F <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_90P_4d, group = SALID1), 
            color = "#ef3b2c", alpha = 0.4) +
  geom_line(data = df_country_avg_F, aes(x = YEAR, y = mean_HWF_90P_4d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 200)) +
  labs(title = "C") +
  labs(x = "Year",
       y = "HWF_90P_4d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_95P_2d_F <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_95P_2d, group = SALID1), 
            color = "#ef3b2c", alpha = 0.4) +
  geom_line(data = df_country_avg_F, aes(x = YEAR, y = mean_HWF_95P_2d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 130)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "HWF_95P_2d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_95P_3d_F <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_95P_3d, group = SALID1), 
            color = "#ef3b2c", alpha = 0.4) +
  geom_line(data = df_country_avg_F, aes(x = YEAR, y = mean_HWF_95P_3d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 130)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "HWF_95P_3d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_95P_4d_F <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_95P_4d, group = SALID1), 
            color = "#ef3b2c", alpha = 0.4) +
  geom_line(data = df_country_avg_F, aes(x = YEAR, y = mean_HWF_95P_4d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 130)) +
  labs(title = "C") +
  labs(x = "Year",
       y = "HWF_95P_4d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_97.5P_2d_F <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_97.5P_2d, group = SALID1), 
            color = "#ef3b2c", alpha = 0.4) +
  geom_line(data = df_country_avg_F, aes(x = YEAR, y = mean_HWF_97.5P_2d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 84)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "HWF_97.5P_2d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_97.5P_3d_F <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_97.5P_3d, group = SALID1), 
            color = "#ef3b2c", alpha = 0.4) +
  geom_line(data = df_country_avg_F, aes(x = YEAR, y = mean_HWF_97.5P_3d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 84)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "HWF_97.5P_3d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_97.5P_4d_F <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_97.5P_4d, group = SALID1), 
            color = "#ef3b2c", alpha = 0.4) +
  geom_line(data = df_country_avg_F, aes(x = YEAR, y = mean_HWF_97.5P_4d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 84)) +
  labs(title = "C") +
  labs(x = "Year",
       y = "HWF_97.5P_4d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

# Combine plots and export
combined_plot_1F <- (line_90P_2d_F) / (line_90P_3d_F) / (line_90P_4d_F)
ggsave(filename = "Figures/Sup_fig_6.png",
       width = 30, height = 20, dpi = 300, bg = "white")  

# Combine plots
combined_plot_2F <- (line_95P_2d_F) / (line_95P_3d_F) / (line_95P_4d_F)
ggsave(filename = "Figures/Sup_fig_7.png",
       width = 30, height = 18, dpi = 300, bg = "white")  

# Combine plots
combined_plot_3F <- (line_97.5P_2d_F) / (line_97.5P_3d_F) / (line_97.5P_4d_F)
ggsave(filename = "Figures/Sup_fig_8.png",
       width = 30, height = 18, dpi = 300, bg = "white")  

#---------------------------------------------
# Supplementary figures - 9 to 11
#---------------------------------------------
# HWD
df_country_avg_D <- data %>%
  group_by(Country, YEAR) %>%
  summarize(
    mean_HWD_EHF = mean(HWD_EHF, na.rm = TRUE),
    mean_HWD_90P_2d = mean(HWD_90P_2d, na.rm = TRUE),
    mean_HWD_90P_3d = mean(HWD_90P_3d, na.rm = TRUE),
    mean_HWD_90P_4d = mean(HWD_90P_4d, na.rm = TRUE),
    mean_HWD_95P_2d = mean(HWD_95P_2d, na.rm = TRUE),
    mean_HWD_95P_3d = mean(HWD_95P_3d, na.rm = TRUE),
    mean_HWD_95P_4d = mean(HWD_95P_4d, na.rm = TRUE),
    mean_HWD_97.5P_2d = mean(HWD_97.5P_2d, na.rm = TRUE),
    mean_HWD_97.5P_3d = mean(HWD_97.5P_3d, na.rm = TRUE),
    mean_HWD_97.5P_4d = mean(HWD_97.5P_4d, na.rm = TRUE),
    .groups = "drop")

line_EHF_D <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWD_EHF, group = SALID1),
            color = "#fd8d3c", alpha = 0.4) +
  geom_line(data = df_country_avg_D, aes(x = YEAR, y = mean_HWD_EHF),
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +
  coord_cartesian(ylim = c(0, 88)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "HWD_EHF") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

line_90P_2d_D <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWD_90P_2d, group = SALID1), 
            color = "#fd8d3c", alpha = 0.4) +
  geom_line(data = df_country_avg_D, aes(x = YEAR, y = mean_HWD_90P_2d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 88)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "HWD_90P_2d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_90P_3d_D <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWD_90P_3d, group = SALID1), 
            color = "#fd8d3c", alpha = 0.4) +
  geom_line(data = df_country_avg_D, aes(x = YEAR, y = mean_HWD_90P_3d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 88)) +
  labs(title = "C") +
  labs(x = "Year",
       y = "HWD_90P_3d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_90P_4d_D <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWD_90P_4d, group = SALID1), 
            color = "#fd8d3c", alpha = 0.4) +
  geom_line(data = df_country_avg_D, aes(x = YEAR, y = mean_HWD_90P_4d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 88)) +
  labs(title = "D") +
  labs(x = "Year",
       y = "HWD_90P_4d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_95P_2d_D <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWD_95P_2d, group = SALID1), 
            color = "#fd8d3c", alpha = 0.4) +
  geom_line(data = df_country_avg_D, aes(x = YEAR, y = mean_HWD_95P_2d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 68)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "HWD_95P_2d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_95P_3d_D <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWD_95P_3d, group = SALID1), 
            color = "#fd8d3c", alpha = 0.4) +
  geom_line(data = df_country_avg_D, aes(x = YEAR, y = mean_HWD_95P_3d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 68)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "HWD_95P_3d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_95P_4d_D <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWD_95P_4d, group = SALID1), 
            color = "#fd8d3c", alpha = 0.4) +
  geom_line(data = df_country_avg_D, aes(x = YEAR, y = mean_HWD_95P_4d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 68)) +
  labs(title = "C") +
  labs(x = "Year",
       y = "HWD_95P_4d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_97.5P_2d_D <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWD_97.5P_2d, group = SALID1), 
            color = "#fd8d3c", alpha = 0.4) +
  geom_line(data = df_country_avg_D, aes(x = YEAR, y = mean_HWD_97.5P_2d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 63)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "HWD_97.5P_2d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_97.5P_3d_D <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWD_97.5P_3d, group = SALID1), 
            color = "#fd8d3c", alpha = 0.4) +
  geom_line(data = df_country_avg_D, aes(x = YEAR, y = mean_HWD_97.5P_3d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 63)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "HWD_97.5P_3d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

line_97.5P_4d_D <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWD_97.5P_4d, group = SALID1), 
            color = "#fd8d3c", alpha = 0.4) +
  geom_line(data = df_country_avg_D, aes(x = YEAR, y = mean_HWD_97.5P_4d), 
            color = "black", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 63)) +
  labs(title = "C") +
  labs(x = "Year",
       y = "HWD_97.5P_4d") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

# Combine plots and export
combined_plot_1D <- (line_EHF_D) / (line_90P_2d_D) / (line_90P_3d_D) / (line_90P_4d_D)
ggsave(filename = "Figures/Sup_Fig_9.png",
       width = 30, height = 20, dpi = 300, bg = "white")  

# Combine plots
combined_plot_2D <- (line_95P_2d_D) / (line_95P_3d_D) / (line_95P_4d_D)
ggsave(filename = "Figures/Sup_Fig_10.png",
       width = 30, height = 18, dpi = 300, bg = "white")  

# Combine plots
combined_plot_3D <- (line_97.5P_2d_D) / (line_97.5P_3d_D) / (line_97.5P_4d_D)
ggsave(filename = "Figures/Sup_Fig_11.png",
       width = 30, height = 18, dpi = 300, bg = "white")  


#---------------------------------------------
# Supplementary figures - 12
#---------------------------------------------
# 3. Map HWF
breaks_HWF <- c(0, 20, 40, 60, 80, 100, Inf)
labels_HWF <- c("0", "20","40", "60", "80", "≥100")
cores_classes <- c("#fcbba1", "#fc9272", "#ef3b2c", "#cb181d", "#a50f15", "#67000d")
size_circ <- c(1.3, 1.8, 2.1, 2.8, 3, 3.4)

data_map <- data_map %>%
  mutate(hwn_class = cut(
    HWF_EHF,
    breaks = breaks_HWF,
    labels = labels_HWF,
    include.lowest = TRUE,
    right = FALSE))

data_map <- data_map %>%
  arrange(desc(as.numeric(hwn_class)))

data_map_YEARs <- data_map %>%
  filter(between(YEAR, 2000, 2004) | between(YEAR, 2020, 2024)) %>%
  arrange(desc(as.numeric(hwn_class)))

# Map
Sup_fig_12 <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(
    data = data_map_YEARs,
    aes(geometry = geometry, size = hwn_class, fill = hwn_class),
    shape = 21, color = "#737373", stroke = 0.1, alpha = 1) +
  scale_size_manual(name = "HWF - EHF", values = size_circ,
                    labels = labels_HWF, drop = FALSE) +
  scale_fill_manual( name = "HWF - EHF",
                     values = cores_classes,
                     labels = labels_HWF,
                     na.value = "grey80") +
  guides(fill = guide_legend(nrow = 1),
         size = guide_legend(nrow = 1)) +
  facet_wrap(~YEAR, ncol = 5) +
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))+
  ggsn::scalebar(data = shp_base,
                 location = "bottomleft",
                 dist = 500,
                 dist_unit = "km",
                 transform = TRUE,
                 model = "WGS84",
                 height = 0.03,
                 st.size = 2,
                 st.dist = 0.04,
                 box.fill = c("white", "white"),
                 box.color = "gray30",
                 border.size = 0.2,
                 st.bottom = TRUE)
Sup_fig_12

# Save
ggsave("Figures/Sup_fig_12_teste.png", Sup_fig_12, width = 21, height = 12, dpi = 700, bg = "white")

#---------------------------------------------
# Supplementary figures - 13 
#---------------------------------------------
# Pop exposure
# at least one heatwave
df_plot_90_1 <- data_pop %>%
  group_by(YEAR) %>%
  summarise(
    HWN_EHF = sum(pop_exposed_HWN_EHF, na.rm = TRUE) / 1e6,
    HWN_90P_2d = sum(pop_exposed_HWN_90P_2d, na.rm = TRUE) / 1e6,
    HWN_90P_3d = sum(pop_exposed_HWN_90P_3d, na.rm = TRUE) / 1e6,
    HWN_90P_4d = sum(pop_exposed_HWN_90P_4d, na.rm = TRUE) / 1e6) %>%
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "value")

df_plot_95_1 <- data_pop %>%
  group_by(YEAR) %>%
  summarise(
    HWN_95P_2d = sum(pop_exposed_HWN_95P_2d, na.rm = TRUE) / 1e6,
    HWN_95P_3d = sum(pop_exposed_HWN_95P_3d, na.rm = TRUE) / 1e6,
    HWN_95P_4d = sum(pop_exposed_HWN_95P_4d, na.rm = TRUE) / 1e6) %>%
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "value")

df_plot_97.5_1 <- data_pop %>%
  group_by(YEAR) %>%
  summarise(
    HWN_97.5P_2d = sum(pop_exposed_HWN_97.5P_2d, na.rm = TRUE) / 1e6,
    HWN_97.5P_3d = sum(pop_exposed_HWN_97.5P_3d, na.rm = TRUE) / 1e6,
    HWN_97.5P_4d = sum(pop_exposed_HWN_97.5P_4d, na.rm = TRUE) / 1e6) %>%
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "value")


hw90P_1hw <- ggplot(df_plot_90_1, aes(YEAR, value, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "A",
       y = "Population exposed (millions)",
       x = "Year",
       color = "") +
  scale_color_manual(values = c(
    "HWN_90P_2d" = "#fed976",
    "HWN_90P_3d" = "#feb24c",
    "HWN_90P_4d" = "#fd8d3c",
    "HWN_EHF" = "#fc4e2a")) +
  scale_y_continuous(limits = c(0, 318)) +  
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12))


hw95P_1hw <- ggplot(df_plot_95_1, aes(YEAR, value, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "B",
       y = "Population exposed (millions)",
       x = "Year",
       color = "") +
  scale_color_manual(values = c(
    "HWN_95P_2d" = "#fcbba1",
    "HWN_95P_3d" = "#fb6a4a",
    "HWN_95P_4d" = "#ef3b2c")) +
  scale_y_continuous(limits = c(0, 318)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12))


hw97.55P_1hw <- ggplot(df_plot_97.5_1, aes(YEAR, value, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "C",
       y = "Population exposed (millions)",
       x = "Year",
       color = "") +
  scale_color_manual(values = c(
    "HWN_97.5P_2d" = "red",
    "HWN_97.5P_3d" = "#cb181d",
    "HWN_97.5P_4d" = "#67000d")) +
  scale_y_continuous(limits = c(0, 318)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12))


# Combine plots
combined_plot_pop <-  hw90P_1hw/ hw95P_1hw / hw97.55P_1hw
combined_plot_pop

# Export
ggsave("Figures/Sup_fig_13.png", plot = combined_plot_pop, width = 10, height = 15, dpi = 300, bg = "white")

#---------------------------------------------
# Supplementary figures - 14
#---------------------------------------------
# number of heatwaves
df_plot_90 <- data_pop %>%
  group_by(YEAR) %>%
  summarise(
    HWN_EHF = sum(person_events_HWN_EHF, na.rm = TRUE) / 1e6,
    HWN_90P_2d = sum(person_events_HWN_90P_2d, na.rm = TRUE) / 1e6,
    HWN_90P_3d = sum(person_events_HWN_90P_3d, na.rm = TRUE) / 1e6,
    HWN_90P_4d = sum(person_events_HWN_90P_4d, na.rm = TRUE) / 1e6) %>%
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "value")

df_plot_95 <- data_pop %>%
  group_by(YEAR) %>%
  summarise(
    HWN_95P_2d = sum(person_events_HWN_95P_2d, na.rm = TRUE) / 1e6,
    HWN_95P_3d = sum(person_events_HWN_95P_3d, na.rm = TRUE) / 1e6,
    HWN_95P_4d = sum(person_events_HWN_95P_4d, na.rm = TRUE) / 1e6) %>%
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "value")

df_plot_97.5 <- data_pop %>%
  group_by(YEAR) %>%
  summarise(
    HWN_97.5P_2d = sum(person_events_HWN_97.5P_2d, na.rm = TRUE) / 1e6,
    HWN_97.5P_3d = sum(person_events_HWN_97.5P_3d, na.rm = TRUE) / 1e6,
    HWN_97.5P_4d = sum(person_events_HWN_97.5P_4d, na.rm = TRUE) / 1e6) %>%
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "value")


hw90P <- ggplot(df_plot_90, aes(YEAR, value, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "A",
    y = "Population exposed (millions)",
    x = "Year",
    color = "") +
  scale_color_manual(values = c(
    "HWN_90P_2d" = "#fed976",
    "HWN_90P_3d" = "#feb24c",
    "HWN_90P_4d" = "#fd8d3c",
    "HWN_EHF" = "#fc4e2a")) +
  scale_y_continuous(limits = c(0, 3500)) + 	
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12))


hw95P <- ggplot(df_plot_95, aes(YEAR, value, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "B",
       y = "Population exposed (millions)",
       x = "Year",
       color = "") +
  scale_color_manual(values = c(
    "HWN_95P_2d" = "#fcbba1",
    "HWN_95P_3d" = "#fb6a4a",
    "HWN_95P_4d" = "#ef3b2c")) +
  scale_y_continuous(limits = c(0, 3500)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12))


hw97.5P <- ggplot(df_plot_97.5, aes(YEAR, value, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "C",
       y = "Population exposed (millions)",
       x = "Year",
       color = "") +
  scale_color_manual(values = c(
    "HWN_97.5P_2d" = "red",
    "HWN_97.5P_3d" = "#cb181d",
    "HWN_97.5P_4d" = "#67000d")) +
  scale_y_continuous(limits = c(0, 3500)) +
  theme_minimal() +
  theme(legend.position = "bottom",
    plot.title = element_text(size = 24, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    legend.title = element_text(size = 12),  
    legend.text = element_text(size = 12))


# Combine plots
combined_plot_pop <-  hw90P/ hw95P / hw97.5P
combined_plot_pop

# Export
ggsave("Figures/Sup_fig_14.png", plot = combined_plot_pop, width = 10, height = 15, dpi = 300, bg = "white")

#---------------------------------------------
# Supplementary figures - 15
#---------------------------------------------
# Pop exposure - days
df_plot_90_f <- data_pop %>%
  group_by(YEAR) %>%
  summarise(
    HWF_EHF = sum(person_days_HWF_EHF, na.rm = TRUE) / 1e6,
    HWF_90P_2d = sum(person_days_HWF_90P_2d, na.rm = TRUE) / 1e6,
    HWF_90P_3d = sum(person_days_HWF_90P_3d, na.rm = TRUE) / 1e6,
    HWF_90P_4d = sum(person_days_HWF_90P_4d, na.rm = TRUE) / 1e6) %>%
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "value")

df_plot_95_f <- data_pop %>%
  group_by(YEAR) %>%
  summarise(
    HWF_95P_2d = sum(person_days_HWF_95P_2d, na.rm = TRUE) / 1e6,
    HWF_95P_3d = sum(person_days_HWF_95P_3d, na.rm = TRUE) / 1e6,
    HWF_95P_4d = sum(person_days_HWF_95P_4d, na.rm = TRUE) / 1e6) %>%
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "value")

df_plot_97.5_f <- data_pop %>%
  group_by(YEAR) %>%
  summarise(
    HWF_97.5P_2d = sum(person_days_HWF_97.5P_2d, na.rm = TRUE) / 1e6,
    HWF_97.5P_3d = sum(person_days_HWF_97.5P_3d, na.rm = TRUE) / 1e6,
    HWF_97.5P_4d = sum(person_days_HWF_97.5P_4d, na.rm = TRUE) / 1e6) %>%
  pivot_longer(cols = -YEAR, names_to = "variable", values_to = "value")


hw90P_f <- ggplot(df_plot_90_f, aes(YEAR, value, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "A",
       y = "Population exposed (millions)",
       x = "Year",
       color = "") +
  scale_color_manual(values = c(
    "HWF_90P_2d" = "#fed976",
    "HWF_90P_3d" = "#feb24c",
    "HWF_90P_4d" = "#fd8d3c",
    "HWF_EHF" = "#fc4e2a")) +
  scale_y_continuous(limits = c(0, 25000)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12))


hw95P_f <- ggplot(df_plot_95_f, aes(YEAR, value, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "B",
       y = "Population exposed (millions)",
       x = "Year",
       color = "") +
  scale_color_manual(values = c(
    "HWF_95P_2d" = "#fcbba1",
    "HWF_95P_3d" = "#fb6a4a",
    "HWF_95P_4d" = "#ef3b2c")) +
  scale_y_continuous(limits = c(0, 25000)) +
  theme_minimal() +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12))

hw97.5P_f <- ggplot(df_plot_97.5_f, aes(YEAR, value, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "C",
       y = "Population exposed (millions)",
       x = "Year",
       color = "") +
  scale_color_manual(values = c(
    "HWF_97.5P_2d" = "red",
    "HWF_97.5P_3d" = "#cb181d",
    "HWF_97.5P_4d" = "#67000d")) +
  scale_y_continuous(limits = c(0, 25000)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12))


# Combine plots
combined_plot_pop_f <-  hw90P_f/ hw95P_f / hw97.5P_f
combined_plot_pop_f

# Export
ggsave("Figures/Sup_fig_15.png", plot = combined_plot_pop_f, width = 10, height = 15, dpi = 300, bg = "white")

