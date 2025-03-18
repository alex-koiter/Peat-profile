library(tidyverse)

data <- read.csv("SupKfiguredata.csv")

text_data  <- data |>  
  mutate(visual = case_when(site_depth == "A70" ~ paste("VI =", visual, "(not used)"),
                            site_depth == "A80" ~ paste("VI =", visual, "(not used)"),
                            TRUE ~ paste("VI =", visual))) |>
  group_by(site_depth, visual) |>
  summarise(x = max(time) * 0.9,
            y = min(log) *0.25)
  

p1 <- ggplot(data = data, aes(x = time, y = log)) +
  geom_point() +
  geom_line(colour = "blue") +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_smooth(method = "lm", se = FALSE, formula = y~x+0, colour = "black") +
  ggpmisc::stat_poly_eq(formula = y~x+0,  label.x = 0.9, rr.digits = 4, hjust = 1) +
  labs(x = "Time (hrs)", y = bquote(log[10](H-h/H-H[0]))) +
  geom_text(data = text_data, aes(x = x, y=y, label = visual), hjust = 1) +
  theme(plot.margin = margin(t = 5.5,  # Top margin
                             r = 10,  # Right margin
                             b = 5.5,  # Bottom margin
                             l = 5.5)) +
  facet_wrap(~site_depth, scales = "free", ncol = 2)
p1

ggsave(plot = p1, filename = "Figure S1.png", height = 200, width = 150, units = "mm", dpi = 600)
