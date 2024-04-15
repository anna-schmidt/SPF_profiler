#------------------------------
# Daily variations in parameters
#------------------------------

# Bring in our final cleaned data frame
data <- cleaned_final

# Use E01 and six days in April as an example
data_subset <- data %>% subset(enclosure == "E01" & month_profiledatetime == 4)

# Make day a character so it plots better
data_subset$day_profiledatetime <- as.character(data_subset$day_profiledatetime)

## Temperature
ggplot(data_subset, aes(x = hour_profile.datetime, y = water.temperature, 
                   color = day_profiledatetime)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~specified.depth)

# visual interpretation: temperature varies over the 24h period only in the top ~7 meters (warmer during the day, cooler at night)

# trying out some statistical tests we could use here if we want
m_0.5 <- data_subset %>% subset(specified.depth == 0.5)
m_15 <- data_subset %>% subset(specified.depth == 15)
summary(aov(water.temperature ~ hour_profile.datetime, data = m_0.5))
summary(aov(water.temperature ~ hour_profile.datetime, data = m_15))

## Dissolved oxygen
ggplot(data_subset, aes(x = hour_profile.datetime, y = oxygen.concentration, 
                        color = day_profiledatetime)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~specified.depth)

# visual interpretation: DO doesn't vary much over the 24-hour period, stays pretty consistent

## pH
ggplot(data_subset, aes(x = hour_profile.datetime, y = pH.value, 
                        color = day_profiledatetime)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~specified.depth)

# visual interpretation: pH doesn't vary much over the 24-hour period, stays pretty consistent

## Conductivity
ggplot(data_subset, aes(x = hour_profile.datetime, y = conductivity, 
                        color = day_profiledatetime)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~specified.depth)

# visual interpretation: conductivity doesn't vary much over the 24-hour period, stays pretty consistent

## Chlorophyll-a
ggplot(data_subset, aes(x = hour_profile.datetime, y = chlorophyll.a, 
                        color = day_profiledatetime)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~specified.depth)

# visual interpretation: chlorophyll-a decreases in the middle of the day in the top ~9 meters - photosynthesis/quenching

## Phycocyanin
ggplot(data_subset, aes(x = hour_profile.datetime, y = phycocyanin, 
                        color = day_profiledatetime)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~specified.depth)

# visual interpretation: phycocyanin decreases in the middle of the day in the top ~13 meters - photosynthesis/quenching

## PAR
ggplot(data_subset, aes(x = hour_profile.datetime, y = photosynthetically.active.radiation.up, 
                        color = day_profiledatetime)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~specified.depth)

# visual interpretation: PAR increases during the day in the top ~7 meters as expected - could be cool to include in our paper as a heatmap perhaps