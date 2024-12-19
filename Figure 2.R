
library(tidyverse)
library(viridis)
library(weathercan)

# Kerner Oceanity Index

stations_search("Pinawa")
climate <- normals_dl(5032162) %>%
  unnest(normals)

koi <- 100 *(filter(climate, period == "Oct")$temp_daily_average - filter(climate, period == "Apr")$temp_daily_average) / (max(climate$temp_daily_average) - min(climate$temp_daily_average))
koi 

# Read in data
data <-  read_csv("Data.csv")
k_data <-read_csv("Ksat data.csv")

# Models

model_data <- data %>%
  mutate(`Model 1` = 10^(-7.522 - 1.124*(log10(mid_depth/100)) - 1.964*(log10(bulk_density_g_cm3)) - 0.139*(von_post_2) + 0.675)) %>% ## raised bog, no KOI and no Microform
  mutate(`Model 2` = 10^(-4.957 - 0.784*(log10(mid_depth/100)) - 0.277*(von_post_2) + 0.700)) %>%
  mutate(`Model 3` = 10^(-6.261 - 1.911*(log10(mid_depth/100)) + 0.359)) %>%
  mutate(`Model 4` = 10^(-7.210 - 1.087*(log10(mid_depth/100)) - 2.252*(log10(bulk_density_g_cm3)))) %>%
  select(mid_depth, bucket, bulk_density_g_cm3, von_post_2, `Model 1`, `Model 2`, `Model 3`, `Model 4`) %>%
  pivot_longer(cols = c("Model 4", "Model 2", "Model 3", "Model 1"), names_to = "Model", values_to = "ksat") %>%
  filter(mid_depth < 200) %>%
  #filter(Model == "Model_4") %>%
  mutate(name = bucket)

# Bind model and measured data
plot_data <- k_data %>%
  mutate(Model = "Measured") %>%
  bind_rows(model_data)


k_plot <- ggplot(data = filter(plot_data, Model != "Measured"), aes(x = mid_depth, y = ksat, colour = Model)) +
  #geom_point() +
  geom_line(linewidth = 0.5) +
  geom_point(data = filter(plot_data, Model == "Measured"), aes(x = mid_depth, y = ksat)) +
  geom_line(data = filter(plot_data, Model == "Measured"), aes(x = mid_depth, y = ksat)) +
  theme_bw() +
  scale_x_reverse(expand = c(0.01,0), limits = c(200,0), breaks = seq(0, 200, 25)) +
  scale_y_log10() +
  coord_flip() +
  labs(x = "Depth (cm)", y = expression(paste("Hydraulic Conductivity (", m~s^{-1}, ")"))) +
  scale_color_manual(values = c("black", viridis::viridis(4))) +
  theme(legend.position.inside = c(0.8, 0.2), aspect.ratio = 1) +
  facet_wrap(~bucket)
k_plot

ggsave(plot = k_plot, filename = "Figure 2.png", height = 120, width = 174, units = "mm", dpi = 600)
