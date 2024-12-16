
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

plot_data <- data %>%
  mutate(`Model 1` = 10^(-7.522 - 1.124*(log10(mid_depth/100)) - 1.964*(log10(bulk_density_g_cm3)) - 0.139*(von_post_2) + 0.675)) %>% ## raised bog, no KOI and no Microform
  mutate(`Model 2` = 10^(-4.957 - 0.784*(log10(mid_depth/100)) - 0.277*(von_post_2) + 0.700)) %>%
  mutate(`Model 3` = 10^(-6.261 - 1.911*(log10(mid_depth/100)) + 0.359)) %>%
  mutate(`Model 4` = 10^(-7.210 - 1.087*(log10(mid_depth/100)) - 2.252*(log10(bulk_density_g_cm3)))) %>%
  select(bucket, mid_depth, `Model 1`, `Model 2`, `Model 3`, `Model 4`) %>%
  right_join(mutate(k_data, mid_depth = mid_depth -2.5), by = join_by(bucket, mid_depth)) %>%
  #select(-Model) %>%
  pivot_longer(cols = c(`Model 1`, `Model 2`, `Model 3`, `Model 4`), names_to = "Model", values_to = "value") %>%
  drop_na()

# MAE
mae <- plot_data %>%
  group_by(Model) %>%
  summarise(mae = scales::scientific((sum(abs(value - ksat)))/n(), digits = 3),
            mse = scales::scientific(((sum(ksat - value)^2)/n())^0.5, digits = 3), 
            max = max(ksat),
            min = min(ksat),
            n = n()) %>%
  mutate(label = paste0("MAE == ", mae))
mae

# Plotting

greens <- c("#9CD874", "#79C940", "#56932A", "#2C4C15", "#142707") %>%
  set_names(rev(LETTERS[1:5]))

p1 <- ggplot(data = plot_data, aes(x = ksat, y = value, colour = bucket)) +
  geom_point() +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10(limits = c(1e-8, 1e-3)) +
  scale_y_log10(limits = c(1e-7, 1e-2)) +
  coord_equal() +
  geom_text(data = mae, x = -Inf, y = +Inf, aes(label = label), parse = TRUE,
            hjust = -0.1, vjust = 1.5, inherit.aes = F) +
  labs(x = expression(paste("Measured Hydraulic Conductivity (", m~s^{-1}, ")")), y = expression(paste("Model Hydraulic Conductivity (", m~s^{-1}, ")")), colour = "Core") +
  scale_color_manual(values = greens) +
  facet_wrap(~Model)
p1

ggsave(plot = p1, filename = "Figure S4.png", height = 120, width = 174, units = "mm", dpi = 600)
