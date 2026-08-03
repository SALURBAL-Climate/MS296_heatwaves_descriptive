##################################################################
# MS296: Descriptive Heatwaves- L1AD level
#
# This script plot supplementary figures
##################################################################

library(dplyr); library(arrow); library(RColorBrewer)
library(tidyr); library(tidyverse); library(patchwork)
library(rlang); library(ggplot2); library(ggspatial)
library(purrr); library(sf)

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

HWD_5yr_avg <- calc_summary(data, HWD_EHF)

plot_bar <- function(df, ylab, title, fill_color) {
  ggplot(df, aes(x = period_5yr, y = mean)) +
    geom_col(fill = fill_color) +
    geom_linerange(aes(ymin = lower, ymax = upper), linewidth = 0.6) +
    labs(title = title, x = "", y = ylab) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 20, hjust = 0.5, color = "black"),
      axis.text.y = element_text(size = 20, color = "black"),
      axis.title.y = element_text(size = 24),
      plot.title = element_text(size = 26, face = "bold"),)
}

Sup_figure_1_a <- plot_bar(HWD_5yr_avg,
                      "Average longest heatwave duration (HWD-EHF)", "A",
                      "#ef6548")
# 2.  Boxplot
Sup_figure_1_b <- data %>%
  ggplot() +
  geom_jitter(aes(x = Country, y = HWD_EHF),
              color = "#ef6548", width = 0.2, na.rm = TRUE, size = 1, alpha = 0.2) +
  geom_boxplot(aes(x = Country, y = HWD_EHF),
               color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x)) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 87)) + 
  theme(
    axis.text.x = element_text(size = 20, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 24),
    plot.title = element_text(size = 26, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs(title = "B", x = "", y = "Longest heatwave duration (HWD-EHF)")

# 3. Combine
Sup_figure_1 <- (Sup_figure_1_a | Sup_figure_1_b ) 
Sup_figure_1

# 4. Save
ggsave(filename = "Figures/Sup_fig_1.png", 
       plot = Sup_figure_1, width = 20, height = 10, dpi = 300, bg = "white")

#---------------------------------------------
# Supplementary figure 2
#---------------------------------------------
# 1. Box plots - HWN
# ---- Boxplot helper ----
make_boxplot <- function(data, var, color, label, ylim_max) {
  ggplot(data) +
    geom_jitter(aes(x = Country, y = .data[[var]]),
                color = color, width = 0.2, na.rm = TRUE, size = 0.8, alpha = 0.2) +
    geom_boxplot(aes(x = Country, y = .data[[var]]),
                 color = "#252525", fill = NA, linewidth = 0.5, width = 0.6, outlier.shape = NA) +
    coord_cartesian(ylim = c(0, ylim_max)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 9, hjust = 0.5, vjust = 0.5),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12),
      panel.grid.major.x = element_blank(),
      legend.position = "none") +
    labs(title = label, x = "", y = var)
}

boxplot_config <- tibble::tribble(
  ~var,             ~color,     ~label, ~ylim,
  "HWN_90P_2d",    "#fb6a4a",  "A1",   36,
  "HWN_90P_3d",    "#fb6a4a",  "A2",   36,
  "HWN_90P_4d",    "#fb6a4a",  "A3",   36,
  "HWN_95P_2d",    "#ef3b2c",  "B1",   24,
  "HWN_95P_3d",    "#ef3b2c",  "B2",   24,
  "HWN_95P_4d",    "#ef3b2c",  "B3",   24,
  "HWN_97.5P_2d",  "#cb181d",  "C1",   18,
  "HWN_97.5P_3d",  "#cb181d",  "C2",   18,
  "HWN_97.5P_4d",  "#cb181d",  "C3",   18)

plots <- purrr::pmap(boxplot_config, function(var, color, label, ylim) {
  make_boxplot(data, var, color, label, ylim)
})

combined_plot <- (plots[[1]] | plots[[2]] | plots[[3]]) /
  (plots[[4]] | plots[[5]] | plots[[6]]) /
  (plots[[7]] | plots[[8]] | plots[[9]])

combined_plot

ggsave("Figures/Sup_fig_2.png", plot = combined_plot, 
       width = 20, height = 15, dpi = 300, bg = "white")

#---------------------------------------------
# Supplementary figure - 3
#---------------------------------------------
# 2. Box plots - HWF
boxplot_config_f <- tibble::tribble(
  ~var,             ~color,     ~label, ~ylim,
  "HWF_90P_2d",    "#fb6a4a",  "A1",   200,
  "HWF_90P_3d",    "#fb6a4a",  "A2",   200,
  "HWF_90P_4d",    "#fb6a4a",  "A3",   200,
  "HWF_95P_2d",    "#ef3b2c",  "B1",   130,
  "HWF_95P_3d",    "#ef3b2c",  "B2",   130,
  "HWF_95P_4d",    "#ef3b2c",  "B3",   130,
  "HWF_97.5P_2d",  "#cb181d",  "C1",   84,
  "HWF_97.5P_3d",  "#cb181d",  "C2",   84,
  "HWF_97.5P_4d",  "#cb181d",  "C3",   84)

plots <- purrr::pmap(boxplot_config_f, function(var, color, label, ylim) {
  make_boxplot(data, var, color, label, ylim)
})

combined_plot_3 <- (plots[[1]] | plots[[2]] | plots[[3]]) /
  (plots[[4]] | plots[[5]] | plots[[6]]) /
  (plots[[7]] | plots[[8]] | plots[[9]])

combined_plot_3

ggsave("Figures/Sup_fig_3.png", plot = combined_plot_3, 
       width = 20, height = 15, dpi = 300, bg = "white")

#---------------------------------------------
# Supplementary figures - 4 to 6
#---------------------------------------------
# ---- Time series helper ----
make_timeseries <- function(data, avg_data, var, mean_var, color, label, ylim_max) {
  ggplot() +
    geom_line(data = data, aes(x = YEAR, y = .data[[var]], group = SALID1),
              color = color, alpha = 0.4) +
    geom_line(data = avg_data, aes(x = YEAR, y = .data[[mean_var]]),
              color = "#252525", linewidth = 0.8) +
    facet_wrap(~Country, ncol = 7) +
    coord_cartesian(ylim = c(0, ylim_max)) +
    labs(title = label, x = "Year", y = var) +
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
}

hw_vars_to_plot <- c("HWN_90P_2d", "HWN_90P_3d", "HWN_90P_4d",
                     "HWN_95P_2d", "HWN_95P_3d", "HWN_95P_4d",
                     "HWN_97.5P_2d", "HWN_97.5P_3d", "HWN_97.5P_4d")

df_country_avg <- data %>%
  group_by(Country, YEAR) %>%
  summarize(across(all_of(hw_vars_to_plot), ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"), .groups = "drop")

# plots
# 90P
line_90P_2d <- make_timeseries(data, df_country_avg, "HWN_90P_2d", "mean_HWN_90P_2d", "#fc9272", "A", 36)
line_90P_3d <- make_timeseries(data, df_country_avg, "HWN_90P_3d", "mean_HWN_90P_3d", "#fc9272", "B", 36)
line_90P_4d <- make_timeseries(data, df_country_avg, "HWN_90P_4d", "mean_HWN_90P_4d", "#fc9272", "C", 36)

# 95P
line_95P_2d <- make_timeseries(data, df_country_avg, "HWN_95P_2d", "mean_HWN_95P_2d", "#fc9272", "A", 24)
line_95P_3d <- make_timeseries(data, df_country_avg, "HWN_95P_3d", "mean_HWN_95P_3d", "#fc9272", "B", 24)
line_95P_4d <- make_timeseries(data, df_country_avg, "HWN_95P_4d", "mean_HWN_95P_4d", "#fc9272", "C", 24)

# 97.5P
line_97.5P_2d <- make_timeseries(data, df_country_avg, "HWN_97.5P_2d", "mean_HWN_97.5P_2d", "#fc9272", "A", 16)
line_97.5P_3d <- make_timeseries(data, df_country_avg, "HWN_97.5P_3d", "mean_HWN_97.5P_3d", "#fc9272", "B", 16)
line_97.5P_4d <- make_timeseries(data, df_country_avg, "HWN_97.5P_4d", "mean_HWN_97.5P_4d", "#fc9272", "C", 16)


combined_plot_4 <- (line_90P_2d) / (line_90P_3d) / (line_90P_4d)
combined_plot_4
ggsave(filename = "Figures/Sup_fig_4.png",
       plot = combined_plot_4,
       width = 30, height = 18, dpi = 300, bg = "white")

combined_plot_5 <- (line_95P_2d) / (line_95P_3d) / (line_95P_4d)
ggsave(filename = "Figures/Sup_fig_5.png",
       plot = combined_plot_5,
       width = 30, height = 18, dpi = 300, bg = "white")

combined_plot_6 <- (line_97.5P_2d) / (line_97.5P_3d) / (line_97.5P_4d)
ggsave(filename = "Figures/Sup_fig_6.png",
       plot = combined_plot_6,
       width = 30, height = 18, dpi = 300, bg = "white")

#---------------------------------------------
# Supplementary figures 7 to 9
#---------------------------------------------
# HWF
hw_vars_to_plot_f <- c("HWF_90P_2d", "HWF_90P_3d", "HWF_90P_4d",
                       "HWF_95P_2d", "HWF_95P_3d", "HWF_95P_4d",
                       "HWF_97.5P_2d", "HWF_97.5P_3d", "HWF_97.5P_4d")

df_country_avg_f <- data %>%
  group_by(Country, YEAR) %>%
  summarize(across(all_of(hw_vars_to_plot_f), ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"), .groups = "drop")

# plots
# 90P
line_f_90P_2d <- make_timeseries(data, df_country_avg_f, "HWF_90P_2d", "mean_HWF_90P_2d", "#fc8d59", "A", 200)
line_f_90P_3d <- make_timeseries(data, df_country_avg_f, "HWF_90P_3d", "mean_HWF_90P_3d", "#fc8d59", "B", 200)
line_f_90P_4d <- make_timeseries(data, df_country_avg_f, "HWF_90P_4d", "mean_HWF_90P_4d", "#fc8d59", "C", 200)

# 95P
line_f_95P_2d <- make_timeseries(data, df_country_avg_f, "HWF_95P_2d", "mean_HWF_95P_2d", "#fc8d59", "A", 130)
line_f_95P_3d <- make_timeseries(data, df_country_avg_f, "HWF_95P_3d", "mean_HWF_95P_3d", "#fc8d59", "B", 130)
line_f_95P_4d <- make_timeseries(data, df_country_avg_f, "HWF_95P_4d", "mean_HWF_95P_4d", "#fc8d59", "C", 130)

# 97.5P
line_f_97.5P_2d <- make_timeseries(data, df_country_avg_f, "HWF_97.5P_2d", "mean_HWF_97.5P_2d", "#fc8d59", "A", 84)
line_f_97.5P_3d <- make_timeseries(data, df_country_avg_f, "HWF_97.5P_3d", "mean_HWF_97.5P_3d", "#fc8d59", "B", 84)
line_f_97.5P_4d <- make_timeseries(data, df_country_avg_f, "HWF_97.5P_4d", "mean_HWF_97.5P_4d", "#fc8d59", "C", 84)


combined_plot_7 <- (line_f_90P_2d) / (line_f_90P_3d) / (line_f_90P_4d)
ggsave(filename = "Figures/Sup_fig_7.png",
       plot = combined_plot_7,
       width = 30, height = 18, dpi = 300, bg = "white")

combined_plot_8 <- (line_f_95P_2d) / (line_f_95P_3d) / (line_f_95P_4d)
ggsave(filename = "Figures/Sup_fig_8.png",
       plot = combined_plot_8,
       width = 30, height = 18, dpi = 300, bg = "white")

combined_plot_9 <- (line_f_97.5P_2d) / (line_f_97.5P_3d) / (line_f_97.5P_4d)
ggsave(filename = "Figures/Sup_fig_9.png",
       plot = combined_plot_9,
       width = 30, height = 18, dpi = 300, bg = "white") 

#---------------------------------------------
# Supplementary figures - 10 to 12
#---------------------------------------------
# HWD
hw_vars_to_plot_d <- c("HWD_EHF",
                       "HWD_90P_2d", "HWD_90P_3d", "HWD_90P_4d",
                       "HWD_95P_2d", "HWD_95P_3d", "HWD_95P_4d",
                       "HWD_97.5P_2d", "HWD_97.5P_3d", "HWD_97.5P_4d")

df_country_avg_d <- data %>%
  group_by(Country, YEAR) %>%
  summarize(across(all_of(hw_vars_to_plot_d), ~mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"), .groups = "drop")

# plots
# 90P
line_d_EHF <- make_timeseries(data, df_country_avg_d, "HWD_EHF", "mean_HWD_EHF", "#ef6548", "A", 88)
line_d_90P_2d <- make_timeseries(data, df_country_avg_d, "HWD_90P_2d", "mean_HWD_90P_2d", "#ef6548", "B", 88)
line_d_90P_3d <- make_timeseries(data, df_country_avg_d, "HWD_90P_3d", "mean_HWD_90P_3d", "#ef6548", "C", 88)
line_d_90P_4d <- make_timeseries(data, df_country_avg_d, "HWD_90P_4d", "mean_HWD_90P_4d", "#ef6548", "D", 88)

# 95P
line_d_95P_2d <- make_timeseries(data, df_country_avg_d, "HWD_95P_2d", "mean_HWD_95P_2d", "#ef6548", "A", 68)
line_d_95P_3d <- make_timeseries(data, df_country_avg_d, "HWD_95P_3d", "mean_HWD_95P_3d", "#ef6548", "B", 68)
line_d_95P_4d <- make_timeseries(data, df_country_avg_d, "HWD_95P_4d", "mean_HWD_95P_4d", "#ef6548", "C", 68)

# 97.5P
line_d_97.5P_2d <- make_timeseries(data, df_country_avg_d, "HWD_97.5P_2d", "mean_HWD_97.5P_2d", "#ef6548", "A", 63)
line_d_97.5P_3d <- make_timeseries(data, df_country_avg_d, "HWD_97.5P_3d", "mean_HWD_97.5P_3d", "#ef6548", "B", 63)
line_d_97.5P_4d <- make_timeseries(data, df_country_avg_d, "HWD_97.5P_4d", "mean_HWD_97.5P_4d", "#ef6548", "C", 63)

combined_plot_10 <- (line_d_EHF) / (line_d_90P_2d) / (line_d_90P_3d) / (line_d_90P_4d)
ggsave(filename = "Figures/Sup_fig_10.png",
       plot = combined_plot_10,
       width = 30, height = 18, dpi = 300, bg = "white")

combined_plot_11 <- (line_d_95P_2d) / (line_d_95P_3d) / (line_d_95P_4d)
ggsave(filename = "Figures/Sup_fig_11.png",
       plot = combined_plot_11,
       width = 30, height = 18, dpi = 300, bg = "white")

combined_plot_12 <- (line_d_97.5P_2d) / (line_d_97.5P_3d) / (line_d_97.5P_4d)
ggsave(filename = "Figures/Sup_fig_12.png",
       plot = combined_plot_12,
       width = 30, height = 18, dpi = 300, bg = "white")

#---------------------------------------------
# Supplementary figures - 13
#---------------------------------------------
# 3. Map HWF
breaks_HWF <- c(0, 20, 40, 60, 80, 100, Inf)
labels_HWF <- c("0-19", "20-39","40-59", "60-79", "80-99", "≥100")
cores_classes <- c("#fcbba1", "#fc9272", "#ef3b2c", "#cb181d", "#a50f15", "#67000d")
size_circ <- c(2, 2, 2, 2, 2, 2)

data_map <- data_map %>%
  mutate(hwf_class = cut(
    HWF_EHF,
    breaks = breaks_HWF,
    labels = labels_HWF,
    include.lowest = TRUE,
    right = FALSE))

data_map <- data_map %>%
  arrange(desc(as.numeric(hwf_class)))

data_map_YEARs <- data_map %>%
  filter(between(YEAR, 2000, 2004) | between(YEAR, 2020, 2024)) %>%
  arrange(desc(as.numeric(hwf_class)))

# Map
Sup_fig_13 <- ggplot() +
  geom_sf(data = shp_base, fill = "#bdbdbd", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(data = shp_SALURBAL_countries, fill = "white", color = "#bdbdbd", linewidth = 0.3) +
  geom_sf(
    data = data_map_YEARs,
    aes(geometry = geometry, size = hwf_class, fill = hwf_class),
    shape = 21, color = "#737373", stroke = 0.3, alpha = 1) +
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
Sup_fig_13

# Save
ggsave("Figures/Sup_fig_13.png", Sup_fig_13, width = 21, height = 12, dpi = 700, bg = "white")

#---------------------------------------------
# Supplementary figures - 14 
#---------------------------------------------
# Pop exposure
# at least one heatwave
hw_definitions <- c("HWN_EHF",
                    "HWN_90P_2d", "HWN_90P_3d", "HWN_90P_4d",
                    "HWN_95P_2d", "HWN_95P_3d", "HWN_95P_4d",
                    "HWN_97.5P_2d", "HWN_97.5P_3d", "HWN_97.5P_4d")

plot_labels <- LETTERS[1:length(hw_definitions)]

scenario_colors <- c(
  "Fixed population (2000)" = "#feb24c",
  "Observed population"     = "#ef3b2c")

make_hw_plot <- function(hw_def, label) {
  
  pop_col     <- paste0("pop_exposed_", hw_def)
  pop2000_col <- paste0("pop2000_exposed_", hw_def)
  
  df <- data_pop %>%
    group_by(YEAR) %>%
    summarise(
      obs = sum(.data[[pop_col]],     na.rm = TRUE) / 1e6,
      fix = sum(.data[[pop2000_col]], na.rm = TRUE) / 1e6,
      .groups = "drop") %>%
    pivot_longer(cols = c(obs, fix),
                 names_to = "scenario",
                 values_to = "exposure") %>%
    mutate(scenario = recode(scenario,
                             "fix" = "Fixed population (2000)",
                             "obs" = "Observed population"))
  
  ggplot(df, aes(x = YEAR, y = exposure,
                 color = scenario, fill = scenario)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE,
                span = 0.5, linewidth = 1.2, alpha = 0.2) +
    scale_color_manual(values = scenario_colors) +
    scale_fill_manual(values  = scenario_colors) +
    scale_y_continuous() +
    coord_cartesian(ylim = c(0, 350), xlim = c(1999, 2026)) +
    labs(title = label,
         x = "Year",
         y = paste0("Population exposed (millions)\n", gsub("HWN_", "", hw_def)),
         color = "", fill = "") +
    theme_minimal() +
    theme(
      plot.title      = element_text(size = 24, face = "bold"),
      axis.text       = element_text(size = 22, color = "black"),
      axis.title.x    = element_text(size = 22, hjust = 0.5),
      axis.title.y    = element_text(size = 22),
      legend.position = "bottom",
      legend.text     = element_text(size = 20))
}

plot_list <- map2(hw_definitions, plot_labels, make_hw_plot)

design <- "
#A#
BCD
EFG
HIJ
"

combined_plot14 <- wrap_plots(
  plot_list[[1]],   
  plot_list[[2]],   
  plot_list[[3]],   
  plot_list[[4]],  
  plot_list[[5]],   
  plot_list[[6]],   
  plot_list[[7]],   
  plot_list[[8]],   
  plot_list[[9]], 
  plot_list[[10]]) +
  plot_layout(design = design, guides = "collect") &
  theme(legend.position = "bottom")

combined_plot14

ggsave("Figures/Sup_fig_14.png", combined_plot14,
       width = 26, height = 28, dpi = 300)
#---------------------------------------------
# Supplementary figures - 15
#---------------------------------------------
make_hw_plot_pe <- function(hw_def, label) {
  
  pop2000_col_pe <- paste0("person_events_2000_", hw_def)
  pop_col_pe     <- paste0("person_events_", hw_def)
  
  df <- data_pop %>%
    group_by(YEAR) %>%
    summarise(
      obs = sum(.data[[pop_col_pe]],     na.rm = TRUE) / 1e6,
      fix = sum(.data[[pop2000_col_pe]], na.rm = TRUE) / 1e6,
      .groups = "drop") %>%
    pivot_longer(cols = c(obs, fix),
                 names_to = "scenario",
                 values_to = "exposure") %>%
    mutate(scenario = recode(scenario,
                             "fix" = "Fixed population (2000)",
                             "obs" = "Observed population"))
  
  ggplot(df, aes(x = YEAR, y = exposure,
                 color = scenario, fill = scenario)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE,
                span = 0.5, linewidth = 1.2, alpha = 0.2) +
    scale_color_manual(values = scenario_colors) +
    scale_fill_manual(values  = scenario_colors) +
    scale_y_continuous() +
    coord_cartesian(ylim = c(0, 4000), xlim = c(1999, 2026)) +
    labs(title = label,
         x = "Year",
         y = paste0("Person-events (millions)\n", gsub("HWN_", "", hw_def)),
         color = "", fill = "") +
    theme_minimal() +
    theme(
      plot.title      = element_text(size = 24, face = "bold"),
      axis.text       = element_text(size = 22, color = "black"),
      axis.title.x    = element_text(size = 22, hjust = 0.5),
      axis.title.y    = element_text(size = 22),
      legend.position = "bottom",
      legend.text     = element_text(size = 22))
}
plot_list <- map2(hw_definitions, plot_labels, make_hw_plot_pe)

design <- "
#A#
BCD
EFG
HIJ
"

combined_plot15 <- wrap_plots(
  plot_list[[1]],   
  plot_list[[2]],   
  plot_list[[3]],   
  plot_list[[4]],  
  plot_list[[5]],   
  plot_list[[6]],   
  plot_list[[7]],   
  plot_list[[8]],   
  plot_list[[9]], 
  plot_list[[10]]) +
  plot_layout(design = design, guides = "collect") &
  theme(legend.position = "bottom")

combined_plot15

ggsave("Figures/Sup_fig_15.png", combined_plot15,
       width = 26, height = 28, dpi = 300)

#---------------------------------------------
# Supplementary figures - 16
#---------------------------------------------
hw_definition_f <- c("HWF_EHF",
                     "HWF_90P_2d", "HWF_90P_3d", "HWF_90P_4d",
                     "HWF_95P_2d", "HWF_95P_3d", "HWF_95P_4d",
                     "HWF_97.5P_2d", "HWF_97.5P_3d", "HWF_97.5P_4d")

plot_labels <- LETTERS[1:length(hw_definition_f)]

make_hw_plot_py <- function(hw_def, label) {
  
  pop2000_col_py <- paste0("py_2000_", hw_def)  
  pop_col_py     <- paste0("py_", hw_def)        
  
  df <- data_pop %>%
    group_by(YEAR) %>%
    summarise(
      obs = sum(.data[[pop_col_py]],     na.rm = TRUE) / 1e6,
      fix = sum(.data[[pop2000_col_py]], na.rm = TRUE) / 1e6,
      .groups = "drop") %>%
    pivot_longer(cols = c(obs, fix),
                 names_to = "scenario",
                 values_to = "exposure") %>%
    mutate(scenario = recode(scenario,
                             "fix" = "Fixed population (2000)",
                             "obs" = "Observed population"))
  
  ggplot(df, aes(x = YEAR, y = exposure,
                 color = scenario, fill = scenario)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE,
                span = 0.5, linewidth = 1.2, alpha = 0.2) +
    scale_color_manual(values = scenario_colors) +
    scale_fill_manual(values  = scenario_colors) +
    scale_y_continuous() +
    coord_cartesian(ylim = c(0, NA), xlim = c(1999, 2026)) +
    labs(title = label,
         x = "Year",
         y = paste0("Person-years (millions)\n", gsub("HWF_", "", hw_def)),
         color = "", fill = "") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title      = element_text(size = 24, face = "bold"),
      axis.text       = element_text(size = 20, color = "black"),
      axis.title.x    = element_text(size = 20, hjust = 0.5),
      axis.title.y    = element_text(size = 20),
      legend.position = "bottom",
      legend.text     = element_text(size = 20))
}

plot_list <- map2(hw_definition_f, plot_labels, make_hw_plot_py)

design <- "
#A#
BCD
EFG
HIJ
"

combined_plot16 <- wrap_plots(
  plot_list[[1]],
  plot_list[[2]],
  plot_list[[3]],
  plot_list[[4]],
  plot_list[[5]],
  plot_list[[6]],
  plot_list[[7]],
  plot_list[[8]],
  plot_list[[9]],
  plot_list[[10]]
) +
  plot_layout(design = design, guides = "collect") &
  theme(legend.position = "bottom")

combined_plot16

ggsave("Figures/Sup_fig_16.png", combined_plot16,
       width = 26, height = 28, dpi = 300)
