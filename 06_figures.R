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
library(ggtext); library(grid)

data <- read_parquet("Data/data_hw_descriptive.parquet")

#---------------------------------------------
# Figure 1
#---------------------------------------------
# 1. Bar plots
data <- data %>%
  mutate(
    period_5yr = case_when(
      YEAR >= 2000 & YEAR <= 2004 ~ "2000–2004",
      YEAR >= 2005 & YEAR <= 2009 ~ "2005–2009",
      YEAR >= 2010 & YEAR <= 2014 ~ "2010–2014",
      YEAR >= 2015 & YEAR <= 2019 ~ "2015–2019",
      YEAR >= 2020 & YEAR <= 2024 ~ "2020–2024"))

calc_summary <- function(df, var) {
  df %>%
    group_by(period_5yr) %>%
    summarize(mean = mean({{ var }}, na.rm = TRUE),
              sd = sd({{ var }}, na.rm = TRUE),
              n = sum(!is.na({{ var }})),
              se = sd / sqrt(n),
              lower = mean - 1.96 * se,
              upper = mean + 1.96 * se,.groups = "drop")
}

HWN_5yr_avg <- calc_summary(data, HWN_EHF)
HWF_5yr_avg <- calc_summary(data, HWF_EHF)
HWD_5yr_avg <- calc_summary(data, HWD_EHF)

plot_bar <- function(df, ylab, title, fill_color) {
  ggplot(df, aes(x = period_5yr, y = mean)) +
    geom_col(fill = fill_color) +
    geom_linerange(aes(ymin = lower, ymax = upper), linewidth = 0.6) +
    labs(title = title, x = "", y = ylab) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 26, hjust = 0.5, color = "black"),
      axis.text.y = element_text(size = 26, color = "black"),
      axis.title.y = element_text(size = 28),
      plot.title = element_text(size = 40, face = "bold"),)
}

figure1_a <- plot_bar(HWN_5yr_avg, 
                      "Average number of heatwave events (HWN-EHF)", "A",
                      "#fc9272")

figure1_b <- plot_bar(HWF_5yr_avg, 
                      "Average number of heatwave days (HWF-EHF)", "B",
                      "#fc8d59") 

# 2.  Boxplot
figure1_c <- data %>%
  ggplot() +
  geom_jitter(aes(x = Country, y = HWN_EHF),
              color = "#fc9272", width = 0.2, na.rm = TRUE, height = 0.5, size = 1, alpha = 0.2) +
  geom_boxplot(aes(x = Country, y = HWN_EHF),
    color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 20) + 
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 26, hjust = 0.5, angle = 0, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(size = 40, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "C", x = "", y = "Number of heatwave events (HWN-EHF)")

figure1_c

figure1_d <- data %>%
  ggplot() +
  geom_jitter(aes(x = Country, y = HWF_EHF),
              color = "#fc8d59", width = 0.2, na.rm = TRUE, size = 1, alpha = 0.2) +
  geom_boxplot(aes(x = Country, y = HWF_EHF),
               color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  ylim(0, 176) + 
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 26, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 28),
    plot.title = element_text(size = 40, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "D", x = "", y = "Number of heatwave days (HWF-EHF)")

Figure_1 <- (figure1_a | figure1_b) /
  plot_spacer() /
  (figure1_c | figure1_d) +
  plot_layout(heights = c(1, 0.08, 1))

Figure_1

# 4. Save
ggsave(filename = "Figures/Figure_1.png", 
       plot = Figure_1, width = 24, height = 22, dpi = 300, bg = "white")

#---------------------------------------------
# Figure 2
#---------------------------------------------
df_country_avg <- data %>%
  group_by(Country, YEAR) %>%
  summarize(mean_HWN_EHF = mean(HWN_EHF, na.rm = TRUE),
            mean_HWF_EHF = mean(HWF_EHF, na.rm = TRUE),
            mean_HWD_EHF = mean(HWD_EHF, na.rm = TRUE),.groups = "drop")

Figure_2a <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWN_EHF, group = SALID1), 
            color = "#fc9272", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWN_EHF), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 20)) +
  labs(title = "A") +
  labs(x = "Year",
       y = "Number of heatwave events (HWN-EHF)") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 20),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text.x = element_text(size = 18, angle = 90, hjust = 1, vjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.spacing = unit(1, "lines"))


Figure_2b <- ggplot() +
  geom_line(data = data, aes(x = YEAR, y = HWF_EHF, group = SALID1), 
            color = "#fc8d59", alpha = 0.4) +
  geom_line(data = df_country_avg, aes(x = YEAR, y = mean_HWF_EHF), 
            color = "#252525", linewidth = 0.8) +
  facet_wrap(~Country, ncol = 7) +  
  coord_cartesian(ylim = c(0, 176)) +
  labs(title = "B") +
  labs(x = "Year",
       y = "Number of heatwave days (HWF-EHF)") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    strip.text = element_text(face = "bold", size = 20),
    plot.title = element_text(size = 28, face = "bold"),
    axis.text.x = element_text(size = 18, angle = 90, hjust = 1, vjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.spacing = unit(1, "lines"))


Figure_2 <- (Figure_2a) / (Figure_2b)
Figure_2

ggsave("Figures/Figure_2.png", plot = Figure_2, 
       width = 26, height = 12, dpi = 300, bg = "white")

#---------------------------------------------
# Figure 3
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
size_circ <- c(2, 2, 2, 2, 2)

Figure_3 <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(
    data = data_map_YEARs,
    aes(geometry = geometry, size = hwn_class, fill = hwn_class),
    shape = 21, color = "#737373", stroke = 0.3, alpha = 1) +
  scale_size_manual(name = "HWN - EHF", values = size_circ,
                    labels = labels_HWN, drop = FALSE) +
  scale_fill_manual(name = "HWN - EHF",
                    values = cores_classes,
                    labels = labels_HWN,
                    na.value = "grey80") +
  guides(fill = guide_legend(nrow = 1),
         size = guide_legend(nrow = 1)) +
  facet_wrap(~YEAR, ncol = 5, strip.position = "top") +  # mantém "top"
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 6),         
    strip.text = element_text(                  
      size = 14, face = "bold",
      margin = margin(t = 0, b = -12)),
    strip.clip = "off",                        
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)) +
  annotation_custom(grob = grid::grobTree(grid::polylineGrob(
    x = unit(c(0.85, 0.84, 0.85, 0.86), "npc"),
    y = unit(c(0.85, 0.87, 0.85, 0.87), "npc") + unit(0.15, "cm"),
    id = c(1, 1, 2, 2),
    gp = grid::gpar(col = "#737373", lwd = 1)),
    grid::textGrob("S", x = unit(0.85, "npc"),
                   y = unit(0.83, "npc") + unit(0.18, "cm"),
                   gp = gpar(fontsize = 6, fontface = "plain")))) +
  ggsn::scalebar(data = shp_base,
                 location = "bottomleft",
                 dist = 500,
                 dist_unit = "km",
                 transform = TRUE,
                 model = "WGS84",
                 height = 0.03,
                 st.size = 3,
                 st.dist = 0.04,
                 box.fill = c("white", "white"),
                 box.color = "gray30",
                 border.size = 0.2,
                 st.bottom = TRUE)
Figure_3

# export
ggsave("Figures/Figure_3.png", Figure_3, width = 21, height = 12, dpi = 700, bg = "white")
#ggsave("Figures/Figure_3.pdf", Figure_3, width = 21, height = 12, dpi = 700, device = "pdf")
