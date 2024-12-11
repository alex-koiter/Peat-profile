#' # Load Libraries

library(tidyverse)
library(patchwork)

#' # Read in data 
data <-  read_csv("Data.csv") %>%
  pivot_longer(cols = c("particle_density_g_cm3", "bulk_density_g_cm3", "porosity"), names_to = "parameter", values_to = "value")

#' # Jitter the von Post data to prevent overlap when plotting
vp_data <- data %>%
  mutate(von_post_2 = as.numeric(von_post_2)) %>%
  mutate(von_post_3 = case_when(
    bucket == "A" ~ von_post_2-0.2,
    bucket == "B" ~ von_post_2-0.1,
    bucket == "C" ~ von_post_2,
    bucket == "D" ~ von_post_2+0.1,
    bucket == "E" ~ von_post_2+0.2))

greens <- rev(c("#9CD874", "#79C940", "#56932A", "#2C4C15", "#142707"))

bd <- ggplot(data = filter(data, parameter == "bulk_density_g_cm3"), aes(x = mid_depth, y = value, colour = bucket)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_reverse() +
  scale_y_continuous(expand = c(0,0), limits = c(-0.008,0.25), breaks = seq(0, 0.25, 0.05)) +
  coord_flip() +
  labs(tag = "a)", x = "Depth (cm)", y = expression(paste("Bulk Density (", g~cm^{-3}, ")")), colour = "Core") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_color_manual(values = greens)
bd

pd <- ggplot(data = filter(data, parameter == "particle_density_g_cm3"), aes(x = mid_depth, y = value, colour = bucket)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  annotate("text", x = 150, y = 0.7, label =  "0.75~g~cm^{-3}", parse = TRUE, angle = 90, size = 2.5) +
  geom_hline(yintercept = 1.4, linetype = "dashed") +
  annotate("text", x = 25, y = 1.35, label =  "1.4~g~cm^{-3}", parse = TRUE, angle = 90, size = 2.5) +
  scale_x_reverse() +
  scale_y_continuous(expand = c(0,0), limits = c(0.5,2), breaks = seq(0.5, 2, 0.25)) +
  coord_flip() +
  labs(x = "Depth (cm)", y = expression(paste("Particle Density (", g~cm^{-3}, ")")), colour = "Core") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.y = element_blank(),
        legend.position = "none")  +
  scale_color_manual(values = greens)
pd

p <- ggplot(data = filter(data, parameter == "porosity"), aes(x = mid_depth, y = value, colour = bucket)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_reverse() +
  scale_y_continuous(expand = c(0,0), limits = c(0.8,1), breaks = seq(0.8, 1, 0.05)) +
  coord_flip() +
  labs(tag = "c)", x = "Depth (cm)", y = expression(paste("Porosity (", cm^3~cm^{-3}, ")")), colour = "Core") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none") +
  scale_color_manual(values = greens)
p



vp <-  ggplot(data = vp_data, aes(x = mid_depth, y = as.numeric(von_post_3), colour = bucket)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_reverse(limits = c(200, 0), breaks = seq(0, 200, 50)) +
  scale_y_continuous(limits = c(0.5, 5.2), breaks = seq(1,5,1)) +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  labs(x = "Depth (cm)", y = "von Post", colour = "Core") +
  scale_color_manual(values = greens)
vp

p2 <- bd + pd + p + vp + 
  plot_layout(guides = 'collect', axes = "collect", axis_titles = "collect") + 
  plot_annotation(tag_levels = "a", tag_suffix = ")") & 
  theme(legend.position = 'bottom', plot.tag.location = "panel", plot.tag.position = c(0.045,0.955), aspect.ratio=1)
p2

ggsave(plot = p2, filename = "Figure 1.png", height = 170, width = 174, units = "mm", dpi = 600)
