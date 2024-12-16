#' # Load Libraries

library(tidyverse)
library(patchwork)

#' # Read in data 
data <-  read_csv("Data.csv") 

plot_data <-  read_csv("Ksat data.csv") %>%
  mutate(mid_depth = mid_depth -2.5) %>%
  right_join(data, by = join_by(mid_depth, bucket))%>%
  pivot_longer(cols = c("particle_density_g_cm3", "bulk_density_g_cm3", "porosity", "ksat"), names_to = "parameter", values_to = "value")

bd <- ggplot(data = filter(plot_data, parameter == "bulk_density_g_cm3"), aes(x = value, y = factor(von_post_2))) +
  geom_boxplot() +
  theme_bw(base_size = 10) +
  labs(y = "von Post", x = expression(paste("Bulk Density (", g~cm^{-3}, ")"))) +
  scale_y_discrete(limits=rev)
bd

pd <- ggplot(data = filter(plot_data, parameter == "particle_density_g_cm3"), aes(x = value, y = factor(von_post_2))) +
  geom_boxplot() +
  theme_bw(base_size = 10) +
  labs(y = "von Post", x = expression(paste("Particle Density (", g~cm^{-3}, ")"))) +
  scale_y_discrete(limits=rev) +
  theme(axis.title.y = element_blank())
pd

p <- ggplot(data = filter(plot_data, parameter == "porosity"), aes(x = value, y = factor(von_post_2))) +
  geom_boxplot() +
  theme_bw(base_size = 10) +
  labs(y = "von Post", x = expression(paste("Particle Density (", g~cm^{-3}, ")"))) +
  scale_y_discrete(limits=rev)
p

k <- ggplot(data = filter(plot_data, parameter == "ksat"), aes(x = value, y = factor(von_post_2))) +
  geom_boxplot() +
  theme_bw(base_size = 10) +
  labs(y = "von Post", x = expression(paste("Hydraulic Conductivity (", m~s^{-1}, ")"))) +
  scale_y_discrete(limits=rev) +
  scale_x_log10() +
  theme(axis.title.y = element_blank())
k


p3 <- bd + pd + p + k + 
  plot_annotation(tag_levels = "a", tag_suffix = ")") & 
  theme(plot.tag.location = "panel", plot.tag.position = c(0.045,0.955), aspect.ratio=1)
p3

ggsave(plot = p3, filename = "Figure Supp 2.png", height = 170, width = 174, units = "mm", dpi = 600)
