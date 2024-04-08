#------------------------------
# removing all outliers
#------------------------------

# Load libraries
library(ggplot2)
library(dplyr)

# start with dataframe no_boxplot_outliers from summary_stats.R, which already has some outliers removed based on visual assessment of boxplots for each variable across all times/enclosures combined
master_cleaning <- no_boxplot_outliers

# remove rest of outliers

# Finding outliers by depth and time for all time points, partitioned by mesocosm

##

# water.temperature by depth
temp1 <- ggplot(master_cleaning, aes(x = specified.depth, y = water.temperature)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "specified.depth", y = "water.temperature")
temp1

# water.temperature by time
temp2 <- ggplot(master_cleaning, aes(x = hour_profile.datetime, y = water.temperature)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "hour", y = "water.temperature")
temp2

# water temp = no outliers observed

##

# conductivity by depth
conduc1 <- ggplot(master_cleaning, aes(x = specified.depth, y = conductivity)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "specified.depth", y = "conductivity")
conduc1

# conductivity by time
conduc2 <- ggplot(master_cleaning, aes(x = hour_profile.datetime, y = conductivity)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "hour", y = "conductivity")
conduc2

# conductivity = possible outliers: E01 > 325, E02 > 300, E16 > 275, E23 > 280

####-------------
filtered_conduc <- master_cleaning %>%
  filter(!(enclosure == "E01" & conductivity > 300),
         !(enclosure == "E02" & conductivity > 300),
         !(enclosure == "E16" & conductivity > 275),
         !(enclosure == "E23" & conductivity > 280))
####-------------removed 37 points

##

# chlorophyll.a by depth
chloro1 <- ggplot(master_cleaning, aes(x = specified.depth, y = chlorophyll.a)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "specified.depth", y = "chlorophyll.a")
chloro1

# chlorophyll.a by time
chloro2 <- ggplot(master_cleaning, aes(x = hour_profile.datetime, y = chlorophyll.a)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "hour", y = "chlorophyll.a")
chloro2

# chlorophyll = possible outliers: E02 > 50, E07 > 75, E08 > 50, E09 > 50, E12 > 25, E17 > 75

####-------------
filtered_chloro <- filtered_conduc %>%
  filter(!(enclosure == "E02" & chlorophyll.a > 50),
         !(enclosure == "E07" & chlorophyll.a > 75),
         !(enclosure == "E08" & chlorophyll.a > 50),
         !(enclosure == "E09" & chlorophyll.a > 50),
         !(enclosure == "E12" & chlorophyll.a > 25),
         !(enclosure == "E17" & chlorophyll.a > 75))
####------------- # removed 12 more points

# phycocyanin by depth
phyco1 <- ggplot(master_cleaning, aes(x = specified.depth, y = phycocyanin)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "specified.depth", y = "phycocyanin")
phyco1

# phycocyanin by time
phyco2 <- ggplot(master_cleaning, aes(x = hour_profile.datetime, y = phycocyanin)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "hour", y = "phycocyanin")
phyco2

####-------------
filtered_phyco <- filtered_chloro %>%
  filter(!(enclosure == "E01" & phycocyanin > 2),
         !(enclosure == "E08" & phycocyanin > 3),
         !(enclosure == "E09" & phycocyanin > 2),
         !(enclosure == "E09" & phycocyanin > 2))
####------------- # removed 13 more points

##

# pH.value by depth
pH1 <- ggplot(master_cleaning, aes(x = specified.depth, y = pH.value)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "specified.depth", y = "pH")
pH1

# pH.value by time
pH2 <- ggplot(master_cleaning, aes(x = hour_profile.datetime, y = pH.value)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "hour", y = "pH")
pH2

# pH = E01 funky but no points jump out in particular for other plots

##

# oxygen.concentration by depth
oxygen1 <- ggplot(master_cleaning, aes(x = specified.depth, y = oxygen.concentration)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "specified.depth", y = "oxygen.concentration")
oxygen1

# oxygen.concentration by time
oxygen2 <- ggplot(master_cleaning, aes(x = hour_profile.datetime, y = oxygen.concentration)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "hour", y = "oxygen.concentration")
oxygen2

# oxygen = no clear outliers

##

# photosynthetically.active.radiation.up by depth
par1 <- ggplot(master_cleaning, aes(x = specified.depth, y = photosynthetically.active.radiation.up)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "specified.depth", y = "PAR")
par1

# photosynthetically.active.radiation.up by time
par2 <- ggplot(master_cleaning, aes(x = hour_profile.datetime, y = photosynthetically.active.radiation.up)) +
  geom_point() +
  facet_wrap(~ enclosure, nrow = 4) +
  labs(x = "hour", y = "PAR")
par2

# PAR = no clear outliers


######

# Removing further outliers based on profile_depth_plots_pngs

cleaned_chlorophyll <- filtered_phyco%>%
  filter(!(enclosure == "E01" & specified.depth > 15 & chlorophyll.a > 15),
         !(enclosure == "E02" & chlorophyll.a > 40),
         !(enclosure == "E02" & profile.datetime == "2023-05-11 00:00:00" & chlorophyll.a > 20),
         !(enclosure == "E07" & chlorophyll.a > 25),
         !(enclosure == "E08" & chlorophyll.a > 30),
         !(enclosure == "E09" & chlorophyll.a > 25),
         !(enclosure == "E10" & chlorophyll.a > 25),
         !(enclosure == "E12" & chlorophyll.a > 15),
         !(enclosure == "E14" & chlorophyll.a > 15),
         !(enclosure == "E14" & profile.datetime %in% c("2023-04-29 00:00:00", "2023-05-13 00:00:00", "2023-05-17 00:00:00", "2023-06-01 00:00:00") & chlorophyll.a > 10),
         !(enclosure == "E16" & profile.datetime %in% c("2023-05-19 00:00:00","2023-05-21 00:00:00") & specified.depth < 5 & chlorophyll.a > 10),
         !(enclosure == "E17" & chlorophyll.a > 25),
         !(enclosure == "E18" & chlorophyll.a > 15),
         !(enclosure == "E19" & chlorophyll.a > 25),
         !(enclosure == "E23" & profile.datetime %in% c("2023-05-01 00:00:00", "2023-05-12 00:00:00", "2023-05-19 00:00:00", "2023-05-20 00:00:00", "2023-05-23 00:00:00","2023-05-24 00:00:00") & chlorophyll.a > 20),
         !(enclosure == "E23" & profile.datetime %in% c("2023-05-25 00:00:00","2023-05-26 00:00:00","2023-05-27 00:00:00","2023-05-29 00:00:00","2023-06-01 00:00:00","2023-06-02 00:00:00") & chlorophyll.a > 25),
         !(enclosure == "E24" & chlorophyll.a > 30),
         !(enclosure == "L01" & chlorophyll.a > 15))
####------------- # removed 287 more points






