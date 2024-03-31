#------------------------------
# Exploratory data analysis - boxplots
#------------------------------

library(tidyverse)

# uses master_times data frame created in create_mastersheet.R and time_manipulations.R

# Create subset of just our parameter columns
subset <- master_times %>%
  select(c(water.temperature:photosynthetically.active.radiation.up))

# Boxplots of all of our parameter values on one plot
plot.new()
par(mfrow = c(3, 3))  # setting up the plotting layout
lapply(names(subset), function(col) {
  boxplot(subset[[col]], main = col, ylab = "Value")
})

# Most obvious outliers:
# chlorophyll.a > 100 (two outliers)
# phycocyanin > 4 (six outliers)
# conductivity < 200 (one outlier)

#####################################
# Boxplots by month for each variable
#####################################

# Make month a character instead of numeric variable
master_times$month_profiledatetime <- as.character(master_times$month_profiledatetime)

# Boxplot of water.temperature each month
master_times %>%
  ggplot(aes(x = month_profiledatetime, y = water.temperature)) +
  geom_boxplot()

# Boxplot of conductivity each month
master_times %>%
  ggplot(aes(x = month_profiledatetime, y = conductivity)) +
  geom_boxplot()

# Boxplot of pH.value each month
master_times %>%
  ggplot(aes(x = month_profiledatetime, y = pH.value)) +
  geom_boxplot()

# Boxplot of chlorophyll.a each month
master_times %>%
  ggplot(aes(x = month_profiledatetime, y = chlorophyll.a)) +
  geom_boxplot()

# Boxplot of phycocyanin each month
master_times %>%
  ggplot(aes(x = month_profiledatetime, y = phycocyanin)) +
  geom_boxplot()

# Boxplot of oxygen.concentration each month
master_times %>%
  ggplot(aes(x = month_profiledatetime, y = oxygen.concentration)) +
  geom_boxplot()

# Boxplot of photosynthetically.active.radiation.up each month
master_times %>%
  ggplot(aes(x = month_profiledatetime, y = photosynthetically.active.radiation.up)) +
  geom_boxplot()

########################################
# Violin plots by month for each variable
########################################

# Violin plot of water.temperature each month
master_times %>%
  ggplot(aes(y = water.temperature, x = month_profiledatetime, fill = month_profiledatetime)) +
  geom_violin()

# etc...for the other variables

# See http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization for more customization of violin plots
# Do we like violin plots or boxplots better?
# Could also split up by mesocosm, and/or specified.depth, instead of month, to do more EDI