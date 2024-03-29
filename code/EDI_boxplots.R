#------------------------------
# Exploratory data analysis - boxplots
#------------------------------

library(tidyverse)

# uses master_times data frame created in create_mastersheet.R and time_manipulations.R

# Boxplot of water.temperature each month
master_times %>%
  ggplot(aes(x = water.temperature, group = month_profiledatetime)) +
  geom_boxplot()

# Create subset of just our parameter columns
subset <- master_times %>%
  select(c(water.temperature:photosynthetically.active.radiation.up))

# Boxplots of our parameter values
par(mfrow = c(3, 3))  # setting up the plotting layout
lapply(names(subset), function(col) {
  boxplot(data[[col]], main = col, ylab = "Value")
})

# Most obvious outliers:
# chlorophyll.a > 100 (two outliers)
# phycocyanin > 4 (six outliers)
# conductivity < 200 (one outlier)

# Anna will work on creating boxplots by month, and work on violin plots