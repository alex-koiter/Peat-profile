#' # Load Libraries

library(tidyverse)
library(patchwork)

#' # Read in data 
data <- read_csv("Ksat data.csv") %>%
  mutate(mid_depth = mid_depth -2.5) %>%
  right_join(read_csv("Data.csv"), by = join_by(mid_depth, bucket)) %>%
  pivot_longer(cols = c("particle_density_g_cm3", "bulk_density_g_cm3", "porosity", "K_sat"), names_to = "parameter", values_to = "value")

greens <- c("#9CD874", "#79C940", "#56932A", "#2C4C15", "#142707") %>%
  set_names(rev(LETTERS[1:5]))
#greens <- colorspace::sequential_hcl(palette = "Greens 2", n = 5)


bd <- ggplot(data = filter(data, !is.na(von_post_2), parameter == "bulk_density_g_cm3"), 
             aes(y = reorder(von_post_2, desc(von_post_2)), x = value, fill = fct_rev(bucket))) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0), limits = c(-0.008,0.25), breaks = seq(0, 0.25, 0.05)) +
  labs(y = "von Post", x = expression(paste("Bulk Density (", g~cm^{-3}, ")")), fill = "Core") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), aspect.ratio = 1) +
  scale_fill_manual(values = greens, guide = guide_legend(reverse = TRUE)) 
bd

pd <- ggplot(data = filter(data, !is.na(von_post_2), parameter == "particle_density_g_cm3"), 
             aes(y = reorder(von_post_2, desc(von_post_2)), x = value, fill = fct_rev(bucket))) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0), limits = c(0.5,2), breaks = seq(0.5, 2, 0.5)) +
  labs(y = "von Post", x = expression(paste("Particle Density (", g~cm^{-3}, ")")), fill = "Core") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        aspect.ratio = 1,
        axis.title.y = element_blank()) +
  scale_fill_manual(values = greens, guide = guide_legend(reverse = TRUE)) 
pd

p <- ggplot(data = filter(data, !is.na(von_post_2), parameter == "porosity"), 
            aes(y = reorder(von_post_2, desc(von_post_2)), x = value,  fill = fct_rev(bucket))) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0), limits = c(0.8,1), breaks = seq(0.8, 1, 0.05)) +
  labs(y = "von Post", x = expression(paste("Porosity (", cm^{3}~cm^{-3}, ")")), fill = "Core") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), aspect.ratio = 1) +
  scale_fill_manual(values = greens, guide = guide_legend(reverse = TRUE))
p

k <- ggplot(data = filter(data, !is.na(von_post_2), parameter == "K_sat"), 
            aes(y = reorder(von_post_2, desc(von_post_2)), x = value,  fill = fct_rev(bucket))) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  theme_bw() +
  scale_x_log10() +
  labs(y = "von Post", x = expression(paste("Hydraulic Conductivity (", m~s^{-1}, ")")), fill = "Core") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        aspect.ratio = 1,
        axis.title.y = element_blank()) +
  scale_fill_manual(values = greens, guide = guide_legend(reverse = TRUE))
k

p3 <- bd + pd + p + k+
  plot_layout(guides = 'collect', axes = "collect", axis_titles = "collect") + 
  plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(legend.position = 'bottom', plot.tag.location = "panel", plot.tag.position = c(0.045,0.955))
p3

ggsave(plot = p3, filename = "Figure new_S.png", height = 170, width = 174, units = "mm", dpi = 600)
