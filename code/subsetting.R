#------------------------------
# subsetting data - EDA
#------------------------------

# Load packages
library(tidyverse)
library(ggplot2)
library(dplyr)

# Set up dataframe for these manipulations
by_time <- no_boxplot_outliers

# Are time of day and sampling depth correlated?

# make sure column values are in the right format for this analysis
by_time$hour_profile.datetime <- as.numeric(by_time$hour_profile.datetime)
by_time$specified.depth <- as.numeric(by_time$specified.depth)

# find the correlation coefficient
correlation_coefficient <- cor(by_time$hour_profile.datetime, by_time$specified.depth)
correlation_coefficient # -1.356235e-05

# visualize distribution of depths by hour
ggplot(by_time, aes(x=hour_profile.datetime, y=specified.depth))+
  geom_point()

# No linear correlation! Visually, looks even/no clustering. Therefore, we choose a single time point for depth-specific predictions
