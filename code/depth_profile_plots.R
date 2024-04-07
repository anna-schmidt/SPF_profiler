#------------------------------
# Depth profiles
#------------------------------

# Load packages
library(tidyverse)

# Bring in our data frame with the obvious outliers removed
data <- no_boxplot_outliers

# Make a column with just the date
data <- data %>% mutate(date = date(profile.datetime))

# Make date column character so the plotting works
data$date <- as.character(data$date)

# Make a subset of the data frame with just noon values, and look at a table to make sure all mesocosms have data for all the same dates
data_12 <- data %>% subset(hour_profile.datetime == "12")
table(data_12$date, data_12$enclosure)
# a few dates are missing from a few mesocosms (0s in the table) - deal with later
  
# # Depth profile plots version 1: faceted by date
# plot_profile_faceted <- function(data, x, enclosure.id) {
#   data <- data %>% subset(enclosure == enclosure.id) %>% arrange(date, specified.depth)
#   ggplot(data, aes(x = {{x}}, y = specified.depth, color = date)) +
#     geom_point(alpha = 0.5) +
#     geom_line(orientation = "y", alpha = 0.5) +
#     scale_y_reverse() +
#     facet_wrap(~date, ncol = 19)}
# 
# # Water temperature profiles for all 14 mesocosms (+ 1 lake site)
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E01")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E02")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E07")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E08")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E09")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E10")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E12")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E14")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E16")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E17")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E18")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E19")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E23")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "E24")
# plot_profile_faceted(data = data_12, x = water.temperature, enclosure.id = "L01")

# I don't see any outliers for temperature
# Next would be checking the other parameters

# # Depth profile plots version 2: all dates on top of each other
# plot_profile_overlay <- function(data, x, enclosure.id) {
#   data <- data %>% subset(enclosure == enclosure.id) %>% arrange(date, specified.depth)
#   ggplot(data, aes(x = {{x}}, y = specified.depth, color = date)) +
#     geom_point(alpha = 0.2) +
#     geom_line(orientation = "y", alpha = 0.2) +
#     scale_y_reverse()}
# 
# # Water temperature
# plot_profile_overlay(data = data_12, x = water.temperature, enclosure.id = "E01")
# plot_profile_overlay(data = data_12, x = water.temperature, enclosure.id = "E02")
# #....etc

##############

# make the above plots for all 24 hours

# establish enclosure and variables vectors

enclosure_ids <- c("E01", "E02", "E07", "E08", "E09", "E10", "E12", "E14", "E16", "E17", "E18", "E19", "E23", "E24", "L01")

variables <- c("water.temperature", "conductivity", "pH.value","oxygen.concentration","chlorophyll.a","phycocyanin","photosynthetically.active.radiation.up")

# Function to graph each date separately, with each hour as a different line. X=variable, y=specified.depth

data$hour_profile.datetime <- as.character(data$hour_profile.datetime)
plot_profile_faceted <- function(data, x, enclosure.id) {
  data <- data %>% subset(enclosure == enclosure.id) %>% arrange(date, specified.depth)
  ggplot(data, aes(x = {{x}}, y = specified.depth, color = hour_profile.datetime)) +
    geom_point(alpha = 0.4) +
    geom_line(orientation = "y", alpha = 0.4) +
    scale_y_reverse() +
    facet_wrap(~date, ncol = 13)}

####
# test to see if it works - it does
# example<- plot_profile_faceted(data,water.temperature,"E01")
# example
####

# make a for-loop to create these graphs for each variable, for each enclosure (7*15 plots total)

plots_list <- list()

for (enclosure_id in enclosure_ids) {
  enclosure_plots <- list()
  
  for (variable in variables) {
    plot <- plot_profile_faceted(data, get(variable), enclosure_id)  # Retrieve variable value by name
    plot_name <- paste0("profile_depth_plots_pngs/", enclosure_id, "_", variable, ".png")
    ggsave(filename = plot_name, plot = plot, width = 30, height = 18)
    
    enclosure_plots[[variable]] <- plot_name
  }
  
  plots_list[[enclosure_id]] <- enclosure_plots
}
