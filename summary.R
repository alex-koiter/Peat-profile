
library(tidyverse)
library(patchwork)


data <- read_csv("Data.csv") %>%
  pivot_longer(cols = c("particle_density_g_cm3", "bulk_density_g_cm3", "porosity"), names_to = "parameter", values_to = "value")


#' # overall summary
data %>%
  group_by(parameter) %>%
  summarise(average = mean(value),
            median = median(value),
            sd = sd(value),
            max = max(value),
            min = min(value),
            n = n())

#' # summary by core
data %>%
  group_by(parameter, bucket) %>%
  summarise(average = mean(value),
            median = median(value),
            sd = sd(value),
            max = max(value),
            min = min(value),
            n = n())

#' # top 25 cm            
data %>%
  filter(mid_depth <25) %>%
  group_by(parameter) %>%
  summarise(average = mean(value),
            median = median(value),
            sd = sd(value),
            max = max(value),
            min = min(value),
            n = n())

#' # top 25 cm, removing suspected particle density outlier          
data %>%
  filter(mid_depth <25) %>%
  filter(!(parameter == "particle_density_g_cm3"& value > 1)) %>%
  group_by(parameter) %>%
  summarise(average = mean(value),
            median = median(value),
            sd = sd(value),
            max = max(value),
            min = min(value),
            n = n())

#' # Porosity estimates
#' ## max BD and max PD
1- (max((filter(data, parameter == "bulk_density_g_cm3"))$value) / max((filter(data, parameter == "particle_density_g_cm3"))$value))
#' ## max BD and min PD
1- (max((filter(data, parameter == "bulk_density_g_cm3"))$value) / min((filter(data, parameter == "particle_density_g_cm3"))$value))
#' ## min BD and max PD
1- (min((filter(data, parameter == "bulk_density_g_cm3"))$value) / max((filter(data, parameter == "particle_density_g_cm3"))$value))
#' ## min BD and min PD
1- (min((filter(data, parameter == "bulk_density_g_cm3"))$value) / min((filter(data, parameter == "particle_density_g_cm3"))$value))
#' ## mean BD and mean PD
1- (mean((filter(data, parameter == "bulk_density_g_cm3"))$value) / mean((filter(data, parameter == "particle_density_g_cm3"))$value))
#' ## median BD and median PD
1- (median((filter(data, parameter == "bulk_density_g_cm3"))$value) / median((filter(data, parameter == "particle_density_g_cm3"))$value))

#' ## median BD and min PD
1- (median((filter(data, parameter == "bulk_density_g_cm3"))$value) / min((filter(data, parameter == "particle_density_g_cm3"))$value))
#' ## median BD and max PD
1- (median((filter(data, parameter == "bulk_density_g_cm3"))$value) / max((filter(data, parameter == "particle_density_g_cm3"))$value))

#' ## max BD and median PD
1- (max((filter(data, parameter == "bulk_density_g_cm3"))$value) / median((filter(data, parameter == "particle_density_g_cm3"))$value))
#' ## min BD and median PD
1- (min((filter(data, parameter == "bulk_density_g_cm3"))$value) / median((filter(data, parameter == "particle_density_g_cm3"))$value))

#' # Porosity figures

bd <- seq(min((filter(data, parameter == "bulk_density_g_cm3"))$value), max((filter(data, parameter == "bulk_density_g_cm3"))$value), 0.01)
median_PD <- c(median((filter(data, parameter == "particle_density_g_cm3"))$value))  
mean_PD <- c(mean((filter(data, parameter == "particle_density_g_cm3"))$value))   
min_PD <- c(min((filter(data, parameter == "particle_density_g_cm3"))$value))   
max_PD <- c(max((filter(data, parameter == "particle_density_g_cm3"))$value))   

temp <- data.frame(bd, median_PD, mean_PD, max_PD, min_PD) %>%
  pivot_longer(cols = -bd) %>%
  mutate(porosity = 1 - bd/value)

p1 <- ggplot(data = temp, aes(x = bd, porosity, colour = name)) +
  geom_point()+
  theme_bw() +
  theme(legend.title = element_blank()) +
  labs(x = expression(paste("Bulk Density (", g~cm^{-3}, ")")), y = expression(paste("Porosity (", cm^3~cm^{-3}, ")")))
p1

pd <- seq(min((filter(data, parameter == "particle_density_g_cm3"))$value), max((filter(data, parameter == "particle_density_g_cm3"))$value), 0.05)
median_BD <- c(median((filter(data, parameter == "bulk_density_g_cm3"))$value))  
mean_BD <- c(mean((filter(data, parameter == "bulk_density_g_cm3"))$value))   
min_BD <- c(min((filter(data, parameter == "bulk_density_g_cm3"))$value))   
max_BD <- c(max((filter(data, parameter == "bulk_density_g_cm3"))$value))   

temp2 <- data.frame(pd, median_BD, mean_BD, max_BD, min_BD) %>%
  pivot_longer(cols = -pd) %>%
  mutate(porosity = 1 - (value/pd))

p2 <- ggplot(data = temp2, aes(x = pd, porosity, colour = name)) +
  geom_point() +
  theme_bw() +
  theme(legend.title = element_blank()) +
  labs(x = expression(paste("Particle Density (", g~cm^{-3}, ")")), y = expression(paste("Porosity (", cm^3~cm^{-3}, ")"))) +
  scale_x_continuous(limits = c(0.6,1.9), breaks = seq(0.6, 1.9, 0.2), expand = c(0,0))
p2

p1/p2

#' # correlation
#' ## Correlation between BD and Porosity
cor.test(read_csv("Data.csv", show_col_types = FALSE)$bulk_density_g_cm3, read_csv("Data.csv", show_col_types = FALSE)$porosity)

#' ## Correlation between PD and Porosity
cor.test(read_csv("Data.csv", show_col_types = FALSE)$particle_density_g_cm3, read_csv("Data.csv", show_col_types = FALSE)$porosity)

