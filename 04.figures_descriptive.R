##################################################################
# MS296: Descriptive Heatwaves- L1AD level - tmean_pw
#
# This script plot figures for descriptive analysis for NHW
# 
# Sara Lopes de Moraes
# Last updated: July 2025
##################################################################

# Load libraries
library(dplyr); library(arrow); library(RColorBrewer)
library(tidyr); library(tidyverse); library(gtsummary)
library(rlang); library(ggthemes); library(ggplot2)
library(flextable); library(purrr); library(sf)
library(ggspatial); library(RColorBrewer)


rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Figures")

data <- read_parquet("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/data_hw.parquet")

glimpse(data)

#---------------------------------------------
# Box plots - HWN
#---------------------------------------------

# 1. by country

NHW_country <- data %>% 
  ggplot() +
    geom_boxplot(aes(x = Country, y = HWN_EHF, fill = Country), 
               outlier.shape = NA, linewidth = 0.5, width = 0.6) +
  scale_fill_brewer(palette = "Reds") +
  geom_jitter(aes(x = Country, y = HWN_EHF), 
              color = "#737373", width = 0.2, na.rm = TRUE, size = 0.4) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  xlab("") +
  ylab("Number of heatwaves")

NHW_country

ggsave("NHW_country.png", plot = NHW_country, 
       dpi = 300, width = 10, height = 7, units = "in", 
       bg = "white")


# 2. by CLZ

NHW_clz <- data %>% 
  ggplot() +
  geom_boxplot(aes(x = CLZ, y = HWN_EHF, fill = CLZ), 
               outlier.shape = NA, linewidth = 0.5, width = 0.6) +
  scale_fill_brewer(palette = "OrRd") +
  geom_jitter(aes(x = CLZ, y = HWN_EHF), 
              color = "#737373", width = 0.2, na.rm = TRUE, size = 0.4) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  xlab("") +
  ylab("Number of heatwaves")

NHW_clz

ggsave("NHW_clz.png", plot = NHW_clz, 
       dpi = 300, width = 10, height = 7, units = "in", 
       bg = "white")


# 3. By country per YEAR

NHW_country_YEAR <- data %>%
  ggplot() +
  geom_boxplot(aes(x = factor(YEAR), y = HWN_EHF), 
               fill = "#fb6a4a",
               outlier.shape = NA, linewidth = 0.4, width = 0.5) +
  facet_wrap(~ Country, ncol = 3) + 
  ylim(0, 20) +  
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold")
  ) +
  xlab("YEAR") +
  ylab("Number of heatwaves")

NHW_country_YEAR

ggsave("NHW_country_YEAR.png", plot = NHW_country_YEAR, 
       dpi = 300, width = 10, height = 7, units = "in", 
       bg = "white")


# 4. By CLZ per YEAR

NHW_CLZ_YEAR <- data %>%
  ggplot() +
  geom_boxplot(aes(x = factor(YEAR), y = HWN_EHF), 
               fill = "#fcbba1",
               outlier.shape = NA, linewidth = 0.4, width = 0.5) +
  facet_wrap(~ CLZ, ncol = 2) + 
  ylim(0, 20) +  
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold")
  ) +
  xlab("YEAR") +
  ylab("Number of heatwaves")

NHW_CLZ_YEAR

ggsave("NHW_CLZ_YEAR.png", plot = NHW_CLZ_YEAR, 
       dpi = 300, width = 10, height = 7, units = "in", 
       bg = "white")


#---------------------------------------------
# Line plots - HWN
#---------------------------------------------

# 1. For each city by country

df_country_avg <- data %>%
  group_by(Country, YEAR) %>%
  summarize(mean_NHW = mean(HWN_EHF, na.rm = TRUE), .groups = "drop")

ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_EHF, group = SALID1), 
            color = "#525252", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_NHW), 
            color = "red", linewidth = 0.8) +
  facet_wrap(~Country) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(subtitle = "Black lines = cities | Red line = country average",
       x = "YEAR",
       y = "NHW") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "lines_NHW_by_country.png",
  width = 12, height = 8, dpi = 300, bg = "white")


# 1. For each city by CLZ

df_CLZ_avg <- data %>%
  group_by(CLZ, YEAR) %>%
  summarize(mean_NHW = mean(HWN_EHF, na.rm = TRUE), .groups = "drop")

ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_EHF, group = SALID1), 
            color = "#525252", alpha = 0.4) +
  geom_line(data = df_CLZ_avg, aes(x = YEAR, y = mean_NHW), 
            color = "red", linewidth = 0.8) +
  facet_wrap(~CLZ) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(subtitle = "Black lines = cities | Red line = CLZ average",
       x = "YEAR",
       y = "NHW") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "lines_NHW_by_CLZ.png",
       width = 12, height = 8, dpi = 300, bg = "white")

#---------------------------------------------
# scatter plots - HWN
#---------------------------------------------

# 1.  by country

NHW_scatter_country <- data %>% 
ggplot(aes(x = YEAR, y = HWN_EHF)) +
  geom_point(color = "red", alpha = 0.4, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  facet_wrap(~ Country) +
  coord_cartesian(ylim = c(0, 20)) +  
  labs(x = "YEAR", y = "NHW") +
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1))
NHW_scatter_country

ggsave("NHW_scatter_country.png", plot = NHW_scatter_country, 
       width = 12, height = 8, dpi = 300, bg = "white")


# 2. all countries one plot

unique(data$Country)

NHW_scatter_all_countries <- data %>% 
ggplot(aes(x = YEAR, y = HWN_EHF, color = Country)) +
  geom_point(color = "#525252", alpha = 0.4, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(
    values = c(
      "Argentina" = "#fee0d2",
      "Brazil" = "#fcbba1",
      "Central America" = "#fdae61",
      "Chile" = "#fee08b",
      "Colombia" = "#ef3b2c",
      "Mexico"  = "#cb181d",
      "Peru"  = "#99000d")) +
  coord_cartesian(ylim = c(0, 20), xlim = c(min(data$YEAR), 2024)) +
  labs(x = "YEAR", y = "NHW", color = "Country") +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

NHW_scatter_all_countries

ggsave("NHW_scatter_all_countries.png", plot = NHW_scatter_all_countries, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 3. CLZ

NHW_scatter_CLZ <- data %>% 
  ggplot(aes(x = YEAR, y = HWN_EHF)) +
  geom_point(color = "red", alpha = 0.4, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  facet_wrap(~ CLZ) +
  coord_cartesian(ylim = c(0, 20)) +  
  labs(x = "YEAR", y = "NHW") +
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1))
NHW_scatter_CLZ

ggsave("NHW_scatter_CLZ.png", plot = NHW_scatter_CLZ, 
       width = 12, height = 8, dpi = 300, bg = "white")


# 4. all CLZ one plot

unique(data$CLZ)

NHW_scatter_all_CLZ <- data %>% 
  ggplot(aes(x = YEAR, y = HWN_EHF, color = CLZ)) +
  geom_point(color = "#525252", alpha = 0.4, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(
    values = c(
      "Polar" = "#4575b4",
      "Arid" = "#fee08b",
      "Temperate" = "#fc9272",
      "Tropical"  = "#cb181d")) +
  coord_cartesian(ylim = c(0, 20), xlim = c(min(data$YEAR), 2024)) +
  labs(x = "YEAR", y = "NHW", color = "CLZ") +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

NHW_scatter_all_CLZ

ggsave("NHW_scatter_all_CLZ.png", plot = NHW_scatter_all_CLZ, 
       width = 12, height = 8, dpi = 300, bg = "white")


# 5. for each city
#unique(data$Country)

countries <- c("Argentina", "Brazil", "Central America", 
               "Chile", "Colombia", "Mexico", "Peru")

# Function to create trend plots in batches of 20 cities
plot_trends_by_country <- function(data, country_name, max_cities_per_plot = 20) {
  # Filter data for the current country
  country_data <- data[data$Country == country_name, ]
  
  if(nrow(country_data) == 0) {
    warning(paste("No data found for:", country_name))
    return(NULL)
  }
  
  unique_cities <- unique(country_data$SALID1)
  total_cities <- length(unique_cities)
  
  # Split the cities into blocks
  city_blocks <- split(unique_cities, ceiling(seq_along(unique_cities) / max_cities_per_plot))
  
  # Generate a plot for each block
  for(i in seq_along(city_blocks)) {
    cities_block <- city_blocks[[i]]
    block_data <- country_data[country_data$SALID1 %in% cities_block, ]
    
    p <- ggplot(block_data, aes(x = YEAR, y = HWN_EHF)) +
      geom_point(alpha = 0.7, size = 1.5, color = "#525252") +
      geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
      facet_wrap(~ SALID1, ncol = 4) +
      theme_minimal(base_size = 12) +
      labs(
        title = paste("HWN Trend in", country_name), 
        subtitle = paste("Cities", ((i - 1) * max_cities_per_plot + 1), "to", min(i * max_cities_per_plot, total_cities), "of", total_cities),
        y = "HWN",
        x = "YEAR"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"),
        strip.background = element_rect(fill = "lightgray", color = "white"),
        strip.text = element_text(color = "black", face = "bold"),
        panel.background = element_rect(fill = "white", color = "gray80"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
      )
    
    print(p)
    
    n_rows <- ceiling(length(cities_block) / 4)
    
    ggsave(
      filename = paste0("HWN_", country_name, "_", i, ".png"),
      plot = p,
      width = 12,
      height = 2 + 2.5 * n_rows,
      bg = "white",
      limitsize = FALSE
    )
  }
}

# Loop through each country
for(country_name in countries) {
  cat("Processing:", country_name, "\n")
  plot_trends_by_country(data, country_name, max_cities_per_plot = 20)
}

# Reset graphics device
dev.off()

#---------------------------------------------
# Maps - HWN
#---------------------------------------------
#Load shp files

shapefile <- st_read("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/SHP/L1AD_centroid.shp")
shp_SALURBAL_countries <- st_read("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/SHP/LA_countries.shp")
shp_base <- st_read("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/SHP/LA_base.shp")

shp_SALURBAL_countries <- st_transform(shp_SALURBAL_countries, crs = 4326)

# Join
#shapefile$SALID1 <- as.character(shapefile$SALID1)
data_map <- data %>% left_join(shapefile, by = c("SALID1"))

str(data)

# 1. Annual 
# Breaks and labels
breaks_annual <- c(0, 0.1, 5, 10, 15, Inf)
labels_annual <- c("0", "1 - 4", "5 - 9", "10 - 14", "≥15")

data_map <- data_map %>%
  mutate(
    hwn_class = cut(
      HWN_EHF,
      breaks = breaks_annual,
      labels = labels_annual,
      include.lowest = TRUE,
      right = FALSE
    )
  )

data_map <- data_map %>%
  arrange(desc(as.numeric(hwn_class)))
# Colors
cores_classes <- c( "#fee0d2", "#fcbba1", "#ef3b2c",  "#a50f15", "#67000d")

# size
size_circ <- c(0.8, 1.3, 2, 3, 4)

# Map
map_HWN_annual <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(
    data = data_map,
    aes(geometry = geometry, size = hwn_class, fill = hwn_class),
    shape = 21,              
    color = "#737373",       
    stroke = 0.1,
    alpha = 1               
  ) +
  scale_size_manual(
    name = "HWN - EHF",
    values = size_circ,
    labels = labels_annual,
    drop = FALSE
  ) +
  scale_fill_manual(
    name = "HWN - EHF",
    values = cores_classes,
    labels = labels_annual,
    na.value = "grey80"      
  ) +
  guides(
    fill = guide_legend(nrow = 1),
    size = guide_legend(nrow = 1)
  ) +
  facet_wrap(~YEAR, ncol = 5) +
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20))

#print(map_HWN_annual)
ggsave("map_HWN_annual.png", map_HWN_annual, width = 21, height = 25, dpi = 700, bg = "white")
ggsave("map_HWN_annual.pdf", map_HWN_annual, width = 21, height = 25, dpi = 700, device = "pdf")


# 2. First 5 YEARs and 5 last YEARs
# select YEARs
data_map_YEARs <- data_map %>%
  filter(between(YEAR, 2000, 2004) | between(YEAR, 2020, 2024)) %>%
  arrange(desc(as.numeric(hwn_class)))  # garante que círculos maiores fiquem por baixo

# Map
map_HWN_5YEARs <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(
    data = data_map_YEARs,
    aes(geometry = geometry, size = hwn_class, fill = hwn_class),
    shape = 21,
    color = "#737373",
    stroke = 0.1,
    alpha = 1
  ) +
  scale_size_manual(
    name = "HWN - EHF",
    values = size_circ,
    labels = labels_annual,
    drop = FALSE
  ) +
  scale_fill_manual(
    name = "HWN - EHF",
    values = cores_classes,
    labels = labels_annual,
    na.value = "grey80"
  ) +
  guides(
    fill = guide_legend(nrow = 1),
    size = guide_legend(nrow = 1)
  ) +
  facet_wrap(~YEAR, ncol = 5) +
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20))

# export
ggsave("map_HWN_5YEARs.png", map_HWN_5YEARs, width = 21, height = 12, dpi = 700, bg = "white")
ggsave("map_HWN_5YEARs.pdf", map_HWN_5YEARs, width = 21, height = 12, dpi = 700, device = "pdf")


#------------------------------------ HWF --------------------------------------------------------------------------------------------------------

#---------------------------------------------
# Box plots - HWF
#---------------------------------------------

# 1. by country

FHW_country <- data %>% 
  ggplot() +
  geom_boxplot(aes(x = Country, y = HWF_EHF, fill = Country), 
               outlier.shape = NA, linewidth = 0.5, width = 0.6) +
  scale_fill_brewer(palette = "Reds") +
  geom_jitter(aes(x = Country, y = HWF_EHF), 
              color = "#737373", width = 0.2, na.rm = TRUE, size = 0.4) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  xlab("") +
  ylab("Number of heatwaves")

FHW_country

ggsave("HWF_country.png", plot = FHW_country, 
       dpi = 300, width = 10, height = 7, units = "in", 
       bg = "white")


# 2. by CLZ

FHW_clz <- data %>% 
  ggplot() +
  geom_boxplot(aes(x = CLZ, y = HWF_EHF, fill = CLZ), 
               outlier.shape = NA, linewidth = 0.5, width = 0.6) +
  scale_fill_brewer(palette = "OrRd") +
  geom_jitter(aes(x = CLZ, y = HWF_EHF), 
              color = "#737373", width = 0.2, na.rm = TRUE, size = 0.4) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  xlab("") +
  ylab("Frequency of heatwaves")

FHW_clz

ggsave("FHW_clz.png", plot = FHW_clz, 
       dpi = 300, width = 10, height = 7, units = "in", 
       bg = "white")


# 3. By country per YEAR

FHW_country_YEAR <- data %>%
  ggplot() +
  geom_boxplot(aes(x = factor(YEAR), y = HWF_EHF), 
               fill = "#fb6a4a",
               outlier.shape = NA, linewidth = 0.4, width = 0.5) +
  facet_wrap(~ Country, ncol = 3) + 
  ylim(0, 180) +  
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold")
  ) +
  xlab("YEAR") +
  ylab("Frequency of heatwaves")

FHW_country_YEAR

ggsave("HWF_country_YEAR.png", plot = FHW_country_YEAR, 
       dpi = 300, width = 10, height = 7, units = "in", 
       bg = "white")


# 4. By CLZ per YEAR

FHW_CLZ_YEAR <- data %>%
  ggplot() +
  geom_boxplot(aes(x = factor(YEAR), y = HWF_EHF), 
               fill = "#fcbba1",
               outlier.shape = NA, linewidth = 0.4, width = 0.5) +
  facet_wrap(~ CLZ, ncol = 2) + 
  ylim(0, 180) +  
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold")
  ) +
  xlab("YEAR") +
  ylab("Frequency of heatwaves")

FHW_CLZ_YEAR

ggsave("HWF_CLZ_YEAR.png", plot = FHW_CLZ_YEAR, 
       dpi = 300, width = 10, height = 7, units = "in", 
       bg = "white")


#---------------------------------------------
# Line plots - HWF
#---------------------------------------------

# 1. For each city by country

df_country_avg <- data %>%
  group_by(Country, YEAR) %>%
  summarize(mean_FHW = mean(HWF_EHF, na.rm = TRUE), .groups = "drop")

ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_EHF, group = SALID1), 
            color = "#525252", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_FHW), 
            color = "red", linewidth = 0.8) +
  facet_wrap(~Country) +
  coord_cartesian(ylim = c(0, 180)) +
  labs(subtitle = "Black lines = cities | Red line = country average",
       x = "YEAR",
       y = "HWF") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "lines_HWF_by_country.png",
       width = 12, height = 8, dpi = 300, bg = "white")


# 1. For each city by CLZ

df_CLZ_avg <- data %>%
  group_by(CLZ, YEAR) %>%
  summarize(mean_FHW = mean(HWF_EHF, na.rm = TRUE), .groups = "drop")

ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_EHF, group = SALID1), 
            color = "#525252", alpha = 0.4) +
  geom_line(data = df_CLZ_avg, aes(x = YEAR, y = mean_FHW), 
            color = "red", linewidth = 0.8) +
  facet_wrap(~CLZ) +
  coord_cartesian(ylim = c(0, 180)) +
  labs(subtitle = "Black lines = cities | Red line = CLZ average",
       x = "YEAR",
       y = "HWF") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "lines_HWF_by_CLZ.png",
       width = 12, height = 8, dpi = 300, bg = "white")

#---------------------------------------------
# scatter plots - HWF
#---------------------------------------------

# 1.  by country

FHW_scatter_country <- data %>% 
  ggplot(aes(x = YEAR, y = HWF_EHF)) +
  geom_point(color = "red", alpha = 0.4, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  facet_wrap(~ Country) +
  coord_cartesian(ylim = c(0, 180)) +  
  labs(x = "YEAR", y = "HWF") +
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1))
FHW_scatter_country

ggsave("HWF_scatter_country.png", plot = FHW_scatter_country, 
       width = 12, height = 8, dpi = 300, bg = "white")


# 2. all countries one plot

unique(data$Country)

FHW_scatter_all_countries <- data %>% 
  ggplot(aes(x = YEAR, y = HWF_EHF, color = Country)) +
  geom_point(color = "#525252", alpha = 0.4, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(
    values = c(
      "Argentina" = "#fee0d2",
      "Brazil" = "#fcbba1",
      "Central America" = "#fdae61",
      "Chile" = "#fee08b",
      "Colombia" = "#ef3b2c",
      "Mexico"  = "#cb181d",
      "Peru"  = "#99000d")) +
  coord_cartesian(ylim = c(0, 180), xlim = c(min(data$YEAR), 2024)) +
  labs(x = "YEAR", y = "HWF", color = "Country") +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

FHW_scatter_all_countries

ggsave("HWF_scatter_all_countries.png", plot = FHW_scatter_all_countries, 
       width = 12, height = 8, dpi = 300, bg = "white")

# 3. CLZ

FHW_scatter_CLZ <- data %>% 
  ggplot(aes(x = YEAR, y = HWF_EHF)) +
  geom_point(color = "red", alpha = 0.4, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  facet_wrap(~ CLZ) +
  coord_cartesian(ylim = c(0, 180)) +  
  labs(x = "YEAR", y = "HWF") +
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1))
FHW_scatter_CLZ

ggsave("HWF_scatter_CLZ.png", plot = FHW_scatter_CLZ, 
       width = 12, height = 8, dpi = 300, bg = "white")


# 4. all CLZ one plot

unique(data$CLZ)

FHW_scatter_all_CLZ <- data %>% 
  ggplot(aes(x = YEAR, y = HWF_EHF, color = CLZ)) +
  geom_point(color = "#525252", alpha = 0.4, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(
    values = c(
      "Polar" = "#4575b4",
      "Arid" = "#fee08b",
      "Temperate" = "#fc9272",
      "Tropical"  = "#cb181d")) +
  coord_cartesian(ylim = c(0, 180), xlim = c(min(data$YEAR), 2024)) +
  labs(x = "YEAR", y = "FHW", color = "CLZ") +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1))

FHW_scatter_all_CLZ

ggsave("HWF_scatter_all_CLZ.png", plot = FHW_scatter_all_CLZ, 
       width = 12, height = 8, dpi = 300, bg = "white")


# 5. for each city
#unique(data$Country)

countries <- c("Argentina", "Brazil", "Central America", 
               "Chile", "Colombia", "Mexico", "Peru")

# Function to create trend plots in batches of 20 cities
plot_trends_by_country <- function(data, country_name, max_cities_per_plot = 20) {
  # Filter data for the current country
  country_data <- data[data$Country == country_name, ]
  
  if(nrow(country_data) == 0) {
    warning(paste("No data found for:", country_name))
    return(NULL)
  }
  
  unique_cities <- unique(country_data$SALID1)
  total_cities <- length(unique_cities)
  
  # Split the cities into blocks
  city_blocks <- split(unique_cities, ceiling(seq_along(unique_cities) / max_cities_per_plot))
  
  # Generate a plot for each block
  for(i in seq_along(city_blocks)) {
    cities_block <- city_blocks[[i]]
    block_data <- country_data[country_data$SALID1 %in% cities_block, ]
    
    p <- ggplot(block_data, aes(x = YEAR, y = HWF_EHF)) +
      geom_point(alpha = 0.7, size = 1.5, color = "#525252") +
      geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
      facet_wrap(~ SALID1, ncol = 4) +
      theme_minimal(base_size = 12) +
      labs(
        title = paste("HWF Trend in", country_name), 
        subtitle = paste("Cities", ((i - 1) * max_cities_per_plot + 1), "to", min(i * max_cities_per_plot, total_cities), "of", total_cities),
        y = "HWF",
        x = "YEAR"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"),
        strip.background = element_rect(fill = "lightgray", color = "white"),
        strip.text = element_text(color = "black", face = "bold"),
        panel.background = element_rect(fill = "white", color = "gray80"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
      )
    
    print(p)
    
    n_rows <- ceiling(length(cities_block) / 4)
    
    ggsave(
      filename = paste0("HWF_", country_name, "_", i, ".png"),
      plot = p,
      width = 12,
      height = 2 + 2.5 * n_rows,
      bg = "white",
      limitsize = FALSE
    )
  }
}

# Loop through each country
for(country_name in countries) {
  cat("Processing:", country_name, "\n")
  plot_trends_by_country(data, country_name, max_cities_per_plot = 20)
}

# Reset graphics device
dev.off()

#---------------------------------------------
# Maps - HWF
#---------------------------------------------
#Load shp files

shapefile <- st_read("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/SHP/L1AD_centroid.shp")
shp_SALURBAL_countries <- st_read("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/SHP/LA_countries.shp")
shp_base <- st_read("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296/Data/SHP/LA_base.shp")

shp_SALURBAL_countries <- st_transform(shp_SALURBAL_countries, crs = 4326)

# Join
#shapefile$SALID1 <- as.character(shapefile$SALID1)
data_map <- data %>% left_join(shapefile, by = c("SALID1"))


# 1. Annual 
# Breaks and labels
breaks_annual <- c(0, 0.1, 25, 50, 75, 100, Inf)
labels_annual <- c("0", "1 - 24", "25 - 49", "50 - 74", "75 - 99", "≥100")

data_map <- data_map %>%
  mutate(
    HWF_class = cut(
      HWF_EHF,
      breaks = breaks_annual,
      labels = labels_annual,
      include.lowest = TRUE,
      right = FALSE
    )
  )

data_map <- data_map %>%
  arrange(desc(as.numeric(HWF_class)))
# Colors
cores_classes <- c( "#fee0d2", "#fcbba1", "#fc9272", "#ef3b2c",  "#a50f15", "#67000d")

# size
size_circ <- c(0.8, 1, 1.3, 2, 3, 4)

# Map
map_HWF_annual <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(
    data = data_map,
    aes(geometry = geometry, size = HWF_class, fill = HWF_class),
    shape = 21,              
    color = "#737373",       
    stroke = 0.1,
    alpha = 1               
  ) +
  scale_size_manual(
    name = "HWF - EHF",
    values = size_circ,
    labels = labels_annual,
    drop = FALSE
  ) +
  scale_fill_manual(
    name = "HWF - EHF",
    values = cores_classes,
    labels = labels_annual,
    na.value = "grey80"      
  ) +
  guides(
    fill = guide_legend(nrow = 1),
    size = guide_legend(nrow = 1)
  ) +
  facet_wrap(~YEAR, ncol = 5) +
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20))

#print(map_HWF_annual)
ggsave("map_HWF_annual.png", map_HWF_annual, width = 21, height = 25, dpi = 700, bg = "white")
ggsave("map_HWF_annual.pdf", map_HWF_annual, width = 21, height = 25, dpi = 700, device = "pdf")


# 2. First 5 YEARs and 5 last YEARs
# select YEARs
data_map_YEARs <- data_map %>%
  filter(between(YEAR, 2000, 2004) | between(YEAR, 2020, 2024)) %>%
  arrange(desc(as.numeric(HWF_class)))  # garante que círculos maiores fiquem por baixo

# Map
map_HWF_5YEARs <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(
    data = data_map_YEARs,
    aes(geometry = geometry, size = HWF_class, fill = HWF_class),
    shape = 21,
    color = "#737373",
    stroke = 0.1,
    alpha = 1
  ) +
  scale_size_manual(
    name = "HWF - EHF",
    values = size_circ,
    labels = labels_annual,
    drop = FALSE
  ) +
  scale_fill_manual(
    name = "HWF - EHF",
    values = cores_classes,
    labels = labels_annual,
    na.value = "grey80"
  ) +
  guides(
    fill = guide_legend(nrow = 1),
    size = guide_legend(nrow = 1)
  ) +
  facet_wrap(~YEAR, ncol = 5) +
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20))

# export
ggsave("map_HWF_5YEARs.png", map_HWF_5YEARs, width = 21, height = 12, dpi = 700, bg = "white")
ggsave("map_HWF_5YEARs.pdf", map_HWF_5YEARs, width = 21, height = 12, dpi = 700, device = "pdf")
