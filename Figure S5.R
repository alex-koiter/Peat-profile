#' # Load Libraries

library(tidyverse)
library(patchwork)

#' # Read in data 
data <-  read_csv("Data.csv") 

p <- ggplot(data =  data, aes(x = particle_density_g_cm3, y = bulk_density_g_cm3, colour = mid_depth)) +
  geom_point() +
  theme_bw(base_size = 10) +
  labs(x = expression(paste("Particle Density (", g~cm^{-3}, ")")), y = expression(paste("Bulk Density (", g~cm^{-3}, ")")), colour = "Depth") +
  scale_colour_viridis_c(limits = c(0,200), breaks = seq(0,200, 50)) +
  scale_x_continuous(limits = c(0.5,2), breaks = seq(0.5, 2, 0.5), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05), expand = c(0,0)) +
  theme(legend.position = "bottom")
p


ggsave(plot = p, filename = "Figure S5.png", height = 100, width = 84, units = "mm", dpi = 600)
