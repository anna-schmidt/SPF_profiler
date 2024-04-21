#------------------------------
# Heatmaps - individual
#------------------------------

# Load packages
library(tidyverse)
library(tidyverse)
library(scales)
library(rLakeAnalyzer)
library(metR)

# Bring in our data frame with all outliers removed
data <- cleaned_final

# Make a column with just the date
data <- data %>% mutate(sampledate = date(profile.datetime))

table(data$sampledate)

# Make date column character so the plotting works
#data$date <- as.character(data$date)

# Make a subset of the data frame with just noon values and just E01
data_12 <- data %>% subset(hour_profile.datetime == "12") %>% subset(enclosure == "L01")

# Define our parameter of interest
data_12$o2 <- data_12$water.temperature

#Function O2
interpDatao2 <- function(observationDF, date, maxdepth) {
  a = observationDF %>% filter(sampledate == date)
  if (sum(!is.na(a$o2)) == 0) {
    print('nothing')
    return(NULL)
  }
  
  b = a %>% filter(!is.na(o2))
  if (max(b$specified.depth) < (maxdepth/2)) {
    print('too shallow')
    return(NULL)
  }
  
  yout = approx(x = a$specified.depth, y = a$o2, xout = c(0:maxdepth), rule = 2)
  return(yout$y)
}

maxdepth = 19.5 # depth of the lowest sample
usedatesME = data_12 %>% dplyr::distinct(sampledate)

fMEo2 <- lapply(X = usedatesME$sampledate, 
                FUN = interpDatao2, 
                observationDF = data_12,
                maxdepth = maxdepth)

fMEo2 = as.data.frame(do.call(cbind, fMEo2))
names(fMEo2) = usedatesME$sampledate

# Bind list into dataframe
f2MEo2 = bind_cols(specified.depth = 0:maxdepth,fMEo2) %>%
  pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
  arrange(sampledate,specified.depth) %>%
  mutate(sampledate = as.Date(sampledate))

ggplot(f2MEo2) +
  geom_contour_filled(aes(x = sampledate, y = specified.depth, z = var), alpha = 0.8) +
                      #breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)) +
  scale_fill_gradientn(colours = c("#440154","#46327e","#365c8d","#277f8e",
                                   "#1fa187","#4ac16d","#a0da39","#fde725"),
                       super = metR::ScaleDiscretised,
                       guide = guide_colorsteps(barheight = 10)) +
  #guides(fill = guide_colorsteps(barheight = unit(5, "cm"), show.limits = TRUE)) +
  geom_point(data = data_12, aes(x = sampledate, y = specified.depth), size = 0.03, color = 'white') +
  scale_y_reverse()  +
  labs(y = "Depth (m)", x = "Date") +
  labs(fill = ((expression(paste(O[2], " (mg", , L^-1,")"))))) +
  theme_bw(base_size = 8) +
  scale_x_date(#breaks = "1 month", 
               #minor_breaks = "1 month", 
               #labels = function(x) format(x, "%b"),
               limits = as.Date(c("2023-04-25", "2023-06-01"))) +
  theme(legend.text = element_text(size = 11),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_blank(),
        legend.position = "left") +
  geom_line(data = data_12, aes(x=sampledate, y=thermo.depth), color = "black", lwd = 0.8) #thermocline depth line


#### calculate thermocline depth 
temp_TD <- data_12 %>%
  select(sampledate, specified.depth, water.temperature) %>%
  rename(datetime = sampledate) %>%
  pivot_wider(names_from = specified.depth, values_from = water.temperature) 

# renaming the column names to include 'wtr_'  Otherwise, rLakeAnaylzer will not run!
colnames(temp_TD)[-1] = paste0('wtr_',colnames(temp_TD)[-1])

# Calculate thermocline depth
thermo_depth <- ts.thermo.depth(temp_TD, na.rm = TRUE)

#bind thermocline depth back to main dataframe 
data_12 <- left_join(data_12, thermo_depth, by = c("sampledate" = "datetime")) |> 
  mutate(doy = yday(sampledate))

