#' # Load Libraries

library(tidyverse)
library(viridis)
library(patchwork)

#' # Read in data 

data <-  read.csv("Data.csv")

plot_data2 <- data %>%
  pivot_longer(cols = c("particle_density_g_cm3", "bulk_density_g_cm3", "porosity"), names_to = "parameter", values_to = "value") 
## Table 1
data %>%
  group_by(bucket, sampler) %>%
  summarise(min_depth = min(start_depth_2),
            max_depth = max(end_depth_2),
            n = n())
data %>%
  filter(!is.na(von_post_2)) %>%
  group_by(bucket) %>%
  summarise(n = n())


## Figure 1

plot_data3 <- plot_data2 %>%
  group_by(parameter, mid_depth) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(parameter = fct_recode(parameter, "Bulk Density" = "bulk_density_g_cm3", "Particle Density" = "particle_density_g_cm3", "Porosity" = "porosity"))

p5_bd <- ggplot(data = filter(plot_data3, parameter == "Bulk Density"), aes(x = mid_depth, y = mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), fill = "grey70") +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 10) +
  scale_x_reverse(expand = c(0,5), breaks = seq(0, 200, 50)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.25), breaks = seq(0, 0.25, 0.05)) +
  coord_flip() +
  labs(tag = "a)", x = "Depth (cm)", y = expression(paste("Bulk Density (", g~cm^{-3}, ")"))) +
  theme(plot.tag.position = c(0.08, 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p5_pd <- ggplot(data = filter(plot_data3, parameter == "Particle Density"), aes(x = mid_depth, y = mean)) +
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
  labs(tag = "b)", x = "Depth (cm)", y = expression(paste("Particle Density (", g~cm^{-3}, ")"))) +
  theme(axis.title.y = element_blank(),
        plot.tag.position = c(0, 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p5_p <- ggplot(data = filter(plot_data3, parameter == "Porosity"), aes(x = mid_depth, y = mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), fill = "grey70") +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 10) +
  scale_x_reverse(expand = c(0,5), breaks = seq(0, 200, 50)) +
  scale_y_continuous(expand = c(0,0), limits = c(0.8, 1), breaks = seq(0.8, 1, 0.05)) +
  coord_flip() +
  labs(tag = "c)", y = expression(paste("Porosity (", cm^{3}~cm^{-3}, ")"))) +
  theme(axis.title.y = element_blank(),
        plot.tag.position = c(0, 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p5 <- p5_bd + p5_pd + p5_p
p5
#ggsave(plot = p5, filename = "Figs/Figure 1.png", height = 80, width = 174, units = "mm", dpi = 600)


## Figure 2           

p3_bd <- ggplot(data = filter(plot_data2, !is.na(von_post_2), parameter == "bulk_density_g_cm3"), aes(x = reorder(von_post_2, desc(von_post_2)), y = value)) +
  geom_boxplot(varwidth = T) +
  theme_bw(base_size = 10) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.1), breaks = seq(0, 0.1, 0.025)) +
  coord_flip() +
  labs(tag = "a)", x = "von Post", y = expression(paste("Bulk Density (", g~cm^{-3}, ")"))) +
  theme(plot.tag.position = c(0.08, 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p3_pd <- ggplot(data = filter(plot_data2, !is.na(von_post_2), parameter == "particle_density_g_cm3"), aes(x = reorder(von_post_2, desc(von_post_2)), y = value)) +
  geom_boxplot(varwidth = T) +
  theme_bw(base_size = 10) +
  scale_y_continuous(expand = c(0,0), limits = c(0.6,1.4), breaks = seq(0.6, 1.4, 0.2)) +
  coord_flip() +
  labs(tag = "b)", x = "Depth (cm)", y = expression(paste("Particle Density (", g~cm^{-3}, ")"))) +
  theme(axis.title.y = element_blank(),
        plot.tag.position = c(0, 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p3_p <- ggplot(data = filter(plot_data2, !is.na(von_post_2), parameter == "porosity"), aes(x = reorder(von_post_2, desc(von_post_2)), y = value)) +
  geom_boxplot(varwidth = T) +
  theme_bw(base_size = 10) +
  scale_y_continuous(expand = c(0,0), limits = c(0.9,1), breaks = seq(0.9, 1, 0.02)) +
  coord_flip() +
  labs(tag = "c)", y = expression(paste("Porosity (", cm^{3}~cm^{-3}, ")"))) +
  theme(axis.title.y = element_blank(),
        plot.tag.position = c(0, 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p3 <- p3_bd + p3_pd + p3_p
p3
#ggsave(plot = p3, filename = "Figs/Figure 2.png", height = 80, width = 174, units = "mm", dpi = 600)


## Figure S1
p2_bd <- ggplot(data = filter(plot_data2, parameter == "bulk_density_g_cm3"), aes(x = mid_depth, y = value, colour = bucket)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_reverse() +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.25), breaks = seq(0, 0.25, 0.05)) +
  coord_flip() +
  labs(tag = "a)", x = "Depth (cm)", y = expression(paste("Bulk Density (", g~cm^{-3}, ")")), colour = "Profile") +
  theme(plot.tag.position = c(0.08, 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p2_pd <- ggplot(data = filter(plot_data2, parameter == "particle_density_g_cm3"), aes(x = mid_depth, y = value, colour = bucket)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_reverse() +
  scale_y_continuous(expand = c(0,0), limits = c(0.5,2), breaks = seq(0.5, 2, 0.5)) +
  coord_flip() +
  labs(tag = "b)", x = "Depth (cm)", y = expression(paste("Particle Density (", g~cm^{-3}, ")")), colour = "Profile") +
  theme(plot.tag.position = c(0.0, 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.y = element_blank())

p2_p <- ggplot(data = filter(plot_data2, parameter == "porosity"), aes(x = mid_depth, y = value, colour = bucket)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_reverse() +
  scale_y_continuous(expand = c(0,0), limits = c(0.8,1), breaks = seq(0.8, 1, 0.05)) +
  coord_flip() +
  labs(tag = "c)", x = "Depth (cm)", y = expression(paste("Porosity (", cm^3~cm^{-3}, ")")), colour = "Profile") +
  theme(plot.tag.position = c(0.0, 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.y = element_blank())

p2 <- p2_bd + p2_pd + p2_p + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
p2
#ggsave(plot = p2, filename = "Figs/Fig S1.png", height = 120, width = 174, units = "mm", dpi = 600)


## Figure S2
p3_2 <- ggplot(data = filter(data, !is.na(von_post_2)), aes(y = von_post_2, x = mid_depth, colour = bucket)) +
  geom_point() +
  theme_bw() +
  scale_x_reverse() +
  coord_flip() +
  labs( y = "von Post", x = "Depth (cm)") +
  theme(legend.position = "none") +
  facet_grid(~bucket)
p3_2
#ggsave(plot = p3_2, filename = "Figs/Fig S2.png", height = 80, width = 174, units = "mm", dpi = 600)


## Figure S3

cor.test(data$particle_density_g_cm3, data$bulk_density_g_cm3)

p6 <- ggplot(data =  data, aes(x = particle_density_g_cm3, y = bulk_density_g_cm3, colour = mid_depth)) +
  geom_point() +
  theme_bw(base_size = 10) +
  labs(x = expression(paste("Particle Density (", g~cm^{-3}, ")")), y = expression(paste("Bulk Density (", g~cm^{-3}, ")")), colour = "Depth") +
  scale_colour_viridis_c(limits = c(0,200), breaks = seq(0,200, 50)) +
  scale_x_continuous(limits = c(0.5,2), breaks = seq(0.5, 2, 0.5), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,0.25), breaks = seq(0, 0.25, 0.05), expand = c(0,0)) +
  theme(legend.position = "bottom")
p6

#ggsave(plot = p6, filename = "Figs/Fig S3.png", height = 100, width = 84, units = "mm", dpi = 600)
