#------------------------------
# Summary depth profile plot of all parameters in all mesocosms on one day
#------------------------------

# Load packages
library(tidyverse)

# Bring in our final cleaned data frame
data <- cleaned_final

# Make a column with just the date
data <- data %>% mutate(date = date(profile.datetime))

# Make a subset of the data frame with just noon values on May 15
data_12 <- data %>% 
  subset(hour_profile.datetime == "12") %>%
  subset(date == "2023-05-03") %>%
  subset(enclosure != "L01")

data_12_long <- data_12 %>%
  pivot_longer(cols = c(water.temperature:photosynthetically.active.radiation.up), names_to = "parameter", values_to = "value")

labels <- as_labeller(
  c("water.temperature" = "Temperature",
    "oxygen.concentration" = "DO",
    "pH.value" = "pH",
    "conductivity" = "Conductivity",
    "chlorophyll.a" = "Chlorophyll-a",
    "phycocyanin" = "Phycocyanin",
    "photosynthetically.active.radiation.up" = "PAR"))
    
ggplot(data_12_long, aes(x = value, y = specified.depth, color = enclosure)) +
  #geom_point(alpha = 0.5) +
  geom_line(orientation = "y", alpha = 0.5, size = 0.5) +
  scale_y_reverse() +
  facet_wrap(~factor(parameter, c("water.temperature", "oxygen.concentration", "pH.value", "conductivity", "chlorophyll.a", "phycocyanin", "photosynthetically.active.radiation.up")), scales = "free_x", ncol = 7, labeller = labels) +
  theme_bw() +
  labs(x = "Parameter value", y = "Depth", color = "Mesocosm") +
  theme(panel.grid.major = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 7),
        axis.title = element_text(size = 12),
        legend.position = "right")
