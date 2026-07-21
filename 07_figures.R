##################################################################
# MS296: Descriptive Heatwaves
#
# This script plot the manuscript figures 
##################################################################

library(dplyr); library(arrow); library(RColorBrewer)
library(tidyr); library(tidyverse); library(gtsummary)
library(rlang); library(ggthemes); library(ggplot2)
library(flextable); library(purrr); library(sf)
library(ggspatial); library(RColorBrewer); library(patchwork)

rm(list= ls())

setwd("C:/Users/saral/Desktop/SALURBAL-CLIMATE/SALURBAL-C/MS296")

data <- read_parquet("Data/data_hw_descriptive.parquet")

glimpse(data)

#---------------------------------------------
# Figure 1
#---------------------------------------------
data <- data %>%
  mutate(
    period_5yr = case_when(
      YEAR >= 2000 & YEAR <= 2004 ~ "2000–2004",
      YEAR >= 2005 & YEAR <= 2009 ~ "2005–2009",
      YEAR >= 2010 & YEAR <= 2014 ~ "2010–2014",
      YEAR >= 2015 & YEAR <= 2019 ~ "2015–2019",
      YEAR >= 2020 & YEAR <= 2024 ~ "2020–2024"))

HWN_5yr_avg <- data %>%
  group_by(period_5yr) %>%
  summarize(mean_HWN = mean(HWN_EHF, na.rm = TRUE), .groups = "drop")

HWF_5yr_avg <- data %>%
  group_by(period_5yr) %>%
  summarize(mean_HWF = mean(HWF_EHF, na.rm = TRUE), .groups = "drop")

HWD_5yr_avg <- data %>%
  group_by(period_5yr) %>%
  summarize(mean_HWD = mean(HWD_EHF, na.rm = TRUE), .groups = "drop")

bar_a <- ggplot(HWN_5yr_avg, aes(x = period_5yr, y = mean_HWN)) +
  geom_col(fill = "#fb6a4a") +
  labs(title = "A",
       x = "", y = "Average of the number of heatwave events (HWN EHF)") +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(hjust = 0.5, size = 8))

bar_a

bar_b <- ggplot(HWF_5yr_avg, aes(x = period_5yr, y = mean_HWF)) +
  geom_col(fill = "#fb6a4a") +
  labs(title = "B",
       x = "", y = "Average of the number of heatwave days (HWF EHF)") +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(hjust = 0.5, size = 8))

bar_b

bar_c <- ggplot(HWD_5yr_avg, aes(x = period_5yr, y = mean_HWD)) +
  geom_col(fill = "#fb6a4a") +
  labs(title = "C",
       x = "", y = "Average of the longest heatwave duration (HWD EHF)") +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(hjust = 0.5, size = 8))

bar_c

Figure_1 <- bar_a | bar_b | bar_c
ggsave(filename = "Figures/Figure_1.png", width = 14, height = 6, dpi = 300, bg = "white")


#---------------------------------------------
# Figure 2
#---------------------------------------------
# 1. Boxplot
Figure_2a <- data %>%
  ggplot() +
  geom_jitter(aes(x = Country, y = HWN_EHF),
              color = "#fb6a4a", width = 0.2, na.rm = TRUE, size = 1, height = 0.8, alpha = 0.2) +
  geom_boxplot(aes(x = Country, y = HWN_EHF),
               color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "A") +
  xlab("") +
  ylab("HWN EHF")

Figure_2a

Figure_2b <- data %>%
  ggplot() +
  geom_jitter(aes(x = Country, y = HWF_EHF),
              color = "#ef3b2c", width = 0.2, na.rm = TRUE, size = 1, height = 0.8, alpha = 0.2) +
  geom_boxplot(aes(x = Country, y = HWF_EHF),
               color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "B") +
  xlab("") +
  ylab("HWF EHF")

Figure_2b

Figure_2 <- (Figure_2a | Figure_2b )
Figure_2

ggsave("Figures/Figure_2.png", plot = Figure_2, 
       dpi = 300, width = 16, height = 7, units = "in", 
       bg = "white")

#---------------------------------------------
# Figure 3
#---------------------------------------------
df_country_avg <- data %>%
  group_by(Country, YEAR) %>%
  summarize(mean_HWN_EHF = mean(HWN_EHF, na.rm = TRUE),
            mean_HWF_EHF = mean(HWF_EHF, na.rm = TRUE),.groups = "drop")

Figure_3a <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_EHF, group = SALID1), 
            color = "#fb6a4a", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_EHF), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 20)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "HWN EHF") +
  theme_bw(base_size = 16) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))
Figure_3a

Figure_3b <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_EHF, group = SALID1), 
            color = "#ef3b2c", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWF_EHF), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 176)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "HWF EHF") +
  theme_bw(base_size = 16) +
  theme(strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))
Figure_3b

Figure_3 <- (Figure_3a) / (Figure_3b)
Figure_3

ggsave("Figures/Figure_3.png", plot = Figure_3, width = 26, height = 12, dpi = 300, bg = "white")

#---------------------------------------------
# Figure 4
#---------------------------------------------
shapefile <- st_read("Data/SHP/L1AD_city_center.shp")
shp_SALURBAL_countries <- st_read("Data/SHP/LA_countries.shp")
shp_base <- st_read("Data/SHP/LA_base.shp")

shp_SALURBAL_countries <- st_transform(shp_SALURBAL_countries, crs = 4326)

# Join
data_map <- data %>% left_join(shapefile, by = c("SALID1"))

breaks_HWN <- c(0, 1, 5, 10, 15, Inf)
labels_HWN <- c("0", "1 - 4", "5 - 9", "10 - 14", "≥15")

data_map <- data_map %>%
  mutate(hwn_class = cut(
    HWN_EHF,
    breaks = breaks_HWN,
    labels = labels_HWN,
    include.lowest = TRUE,
    right = FALSE))

data_map <- data_map %>%
  arrange(desc(as.numeric(hwn_class)))

data_map_YEARs <- data_map %>%
  filter(between(YEAR, 2000, 2004) | between(YEAR, 2020, 2024)) %>%
  arrange(desc(as.numeric(hwn_class)))

cores_classes <- c( "#fcbba1", "#fc9272", "#ef3b2c",  "#a50f15", "#67000d")
size_circ <- c(0.8, 1.3, 2, 3, 4)

Figure_4 <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(
    data = data_map_YEARs,
    aes(geometry = geometry, size = hwn_class, fill = hwn_class),
    shape = 21, color = "#737373", stroke = 0.1, alpha = 1) +
  scale_size_manual(name = "HWN - EHF", values = size_circ,
                    labels = labels_HWN, drop = FALSE) +
  scale_fill_manual( name = "HWN - EHF",
                     values = cores_classes,
                     labels = labels_HWN,
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
Figure_4

# export
ggsave("Figures/Figure_4.png", Figure_4, width = 21, height = 12, dpi = 700, bg = "white")
#ggsave("Figures/Figure_4.pdf", Figure_3, width = 21, height = 12, dpi = 700, device = "pdf")
