#------------------------------
# Visualize correlated variables
#------------------------------

library(tidyverse)
library(lubridate)
library(corrplot)

# grabs master_cleaned_3 instead of master_times because we want the data frame before we removed the unnecessary columns

# Get rid of rows with dates before 04-25-2023
master_times <- master_cleaned_3 %>%
  filter(profile.datetime >= ymd("2023-04-25"))

# pH.value vs. pH..mV.
master_times %>%
  ggplot(aes(x = pH.value, y = pH..mV.)) +
  geom_point()

# chlorophyll.a vs. chlorophyll..RFU.
master_times %>%
  ggplot(aes(x = chlorophyll.a, y = chlorophyll..RFU.)) +
  geom_point()

# phycocyanin vs. phycocyanin..RFU.
master_times %>%
  ggplot(aes(x = phycocyanin, y = phycocyanin..RFU.)) +
  geom_point()

# oxygen.satuation vs. oxygen.concentration
master_times %>%
  ggplot(aes(x = oxygen.satuation, y = oxygen.concentration)) +
  geom_point()

# Correlation matrix

# subset to numeric columns only
subset <- master_times %>%
  select(c(water.temperature:oxygen.concentration))

# create correlation matrix
correlation_matrix <- cor(subset)

# plot correlation matrix
plot.new()
par(mfrow=c(1,1))
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", addrect = 8)