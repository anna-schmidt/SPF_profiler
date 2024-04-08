#------------------------------
# Daily variations in parameters
#------------------------------

# just a very preliminary look

data <- no_boxplot_outliers

subset <- data %>% subset(enclosure == "E01" & month_profiledatetime == 4)

subset$day_profiledatetime <- as.character(subset$day_profiledatetime)

ggplot(subset, aes(x = hour_profile.datetime, y = water.temperature, 
                   color = day_profiledatetime)) +
  geom_point(alpha = 0.2) +
  #geom_line(group = day_profiledatetime) +
  facet_wrap(~specified.depth)
  
       