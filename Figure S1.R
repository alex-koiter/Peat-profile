#' # Load Libraries

library(tidyverse)
library(patchwork)

#' # Read in data 
data <-  read_csv("Data.csv") %>%
  pivot_longer(cols = c("particle_density_g_cm3", "bulk_density_g_cm3", "porosity"), names_to = "parameter", values_to = "value")

k_data <-  read_csv("Ksat data.csv") 

plot_data <- data %>%
  group_by(parameter, mid_depth) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>%
  ungroup() 

plot_data2 <- k_data %>%
  group_by(mid_depth) %>%
  summarise(mean = mean(K_sat, na.rm = TRUE),
            sd = sd(K_sat, na.rm = TRUE),
            n = n(),
            max = max(K_sat, na.rm = TRUE),
            min = min(K_sat, na.rm = TRUE)) %>%
  ungroup()

bd1 <- ggplot(data = filter(plot_data, parameter == "bulk_density_g_cm3"), aes(x = mid_depth, y = mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), fill = "grey70") +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 10) +
  scale_x_reverse(expand = c(0,5), breaks = seq(0, 200, 50)) +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
  coord_flip() +
  labs(x = "Depth (cm)", y = expression(paste("Bulk Density (", g~cm^{-3}, ")"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
bd1

pd1 <- ggplot(data = filter(plot_data, parameter == "particle_density_g_cm3"), aes(x = mid_depth, y = mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), fill = "grey70") +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  annotate("text", x = 150, y = 0.7, label =  "0.75~g~cm^{-3}", parse = TRUE, angle = 90, size = 2.5) +
  geom_hline(yintercept = 1.4, linetype = "dashed") +
  annotate("text", x = 25, y = 1.35, label =  "1.4~g~cm^{-3}", parse = TRUE, angle = 90, size = 2.5) +
  theme_bw(base_size = 10) +
  scale_x_reverse(expand = c(0,5), breaks = seq(0, 200, 50)) +
  scale_y_continuous(expand = c(0,0), limits = c(0.6, 1.8), breaks = seq(0.6, 1.8, 0.3)) +
  coord_flip() +
  labs(x = "Depth (cm)", y = expression(paste("Particle Density (", g~cm^{-3}, ")"))) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
pd1

p1 <- ggplot(data = filter(plot_data, parameter == "porosity"), aes(x = mid_depth, y = mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), fill = "grey70") +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 10) +
  scale_x_reverse(expand = c(0,5), breaks = seq(0, 200, 50)) +
  scale_y_continuous(expand = c(0,0), limits = c(0.8, 1), breaks = seq(0.8, 1, 0.05)) +
  coord_flip() +
  labs(x = "Depth (cm)", y = expression(paste("Porosity (", cm^{3}~cm^{-3}, ")"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
p1


k1<- ggplot(data = plot_data2, aes(x = mid_depth, y = mean)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "grey70") +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 10) +
  scale_x_reverse(expand = c(0,5), limits = c(200, 0), breaks = seq(0, 200, 50)) +
  scale_y_log10() +
  coord_flip() +
  labs(y = expression(paste("Hydraulic Conductivity (", m~s^{-1}, ")"))) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
k1


p3 <- bd1 + pd1 + p1 + k1 + 
  plot_layout(guides = 'collect', axes = "collect", axis_titles = "collect") + 
  plot_annotation(tag_levels = "a", tag_suffix = ")") & 
  theme(legend.position = 'bottom', plot.tag.location = "panel", plot.tag.position = c(0.045,0.955), aspect.ratio=1)
p3

ggsave(plot = p3, filename = "Figure Supp 1.png", height = 170, width = 174, units = "mm", dpi = 600)
