#------------------------------
# Heatmaps - all parameters for E01
#------------------------------

# letting the contour_breaks be automatically assigned

# Load packages
library(tidyverse)
library(tidyverse)
library(scales)
library(rLakeAnalyzer)
library(metR)

# Bring in our final cleaned data frame
data <- cleaned_final

# Make a column with just the date
data <- data %>% mutate(sampledate = date(profile.datetime))

# Make a subset of the data frame with just noon values
data_12 <- data %>% subset(hour_profile.datetime == "12") 

# FUNCTION
test_function <- function(data, enclosure.id, parameter.name, max_depth) {
  
  # Subset data based on enclosure.id
  data_12 <- data %>% subset(enclosure == enclosure.id) 
  
  # Define our parameter of interest
  parameter_col <- data_12[[parameter.name]]
  
  # Function to interpolate data for the specified parameter
  interpData <- function(observationDF, date, maxdepth) {
    a <- observationDF %>% filter(sampledate == date)
    if (sum(!is.na(a[[parameter.name]])) == 0) {
      print('nothing')
      return(NULL)
    }
    
    b <- a %>% filter(!is.na(!!rlang::sym(parameter.name)))
    if (max(b$specified.depth) < (maxdepth/2)) {
      print('too shallow')
      return(NULL)
    }
    
    yout <- approx(x = a$specified.depth, y = a[[parameter.name]], xout = c(0:maxdepth), rule = 2)
    return(yout$y)
  }
  
  # Get unique sample dates
  usedates <- data_12 %>% dplyr::distinct(sampledate)
  
  # Interpolate data for each date
  interpolated_data <- lapply(usedates$sampledate, 
                              FUN = interpData, 
                              observationDF = data_12,
                              maxdepth = max_depth)
  
  # Combine interpolated data into a data frame
  interpolated_data <- as.data.frame(do.call(cbind, interpolated_data))
  names(interpolated_data) <- usedates$sampledate
  
  # Bind list into dataframe
  f2 <- bind_cols(specified.depth = 0:max_depth, interpolated_data) %>%
    pivot_longer(-1, names_to = 'sampledate', values_to = 'var') %>%
    arrange(sampledate, specified.depth) %>%
    mutate(sampledate = as.Date(sampledate))
  
  # Plotting
  ggplot(f2) +
    geom_contour_filled(aes(x = sampledate, y = specified.depth, z = var), alpha = 0.8) +
    scale_fill_gradientn(colours = c("#440154","#46327e","#365c8d","#277f8e",
                                     "#1fa187","#4ac16d","#a0da39","#fde725"),
                         super = metR::ScaleDiscretised,
                         guide = guide_colorsteps(barheight = 10)) +
    #geom_point(data = data_12, aes(x = sampledate, y = specified.depth), size = 0.03, color = 'white') +
    scale_y_reverse()  +
    labs(y = "Depth (m)", x = "Date") +
    labs(fill = bquote(.(as.name(parameter.name)))) +
    theme_bw(base_size = 8) +
    scale_x_date(limits = as.Date(c("2023-04-25", "2023-06-01"))) +
    theme(legend.text = element_text(size = 11),
          legend.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.grid = element_blank(),
          legend.position = "left")
}

# Iterating over all parameters for just E01:

# List of enclosure IDs and their corresponding max depths
enclosure_info <- list(
  "E01" = 19.5)

# List of parameter names and corresponding contour breaks lists
parameter_info <- list(
"oxygen.concentration" = c(0, 2, 4, 6, 8, 10, 12, 14, 16),
"water.temperature" = c(4, 6, 8, 10, 12, 14, 16, 18, 20),
"pH.value" = c(6, 7, 8, 9, 10),
"conductivity" = c(200, 225, 250, 275, 300),
"chlorophyll.a" = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26),
"phycocyanin" = c(-1, 0, 1, 2, 3),
"photosynthetically.active.radiation.up" = c(-3000, 0, 3000, 6000)
)

# Create plots for each enclosure ID and parameter
for (enclosure_id in names(enclosure_info)) {
  max_depth <- enclosure_info[[enclosure_id]]
  
  for (parameter_name in names(parameter_info)) {
    
    # Call test_function with current enclosure ID and parameters
    plot <- test_function(data = data_12, 
                          enclosure.id = enclosure_id, 
                          max_depth = max_depth, 
                          parameter.name = parameter_name)
    
    # Construct file name
    file_name <- paste0("heatmap_plots_pngs/E01/12/", enclosure_id, "_", parameter_name, "_heatmap.png")
    
    # Save the plot
    ggsave(filename = file_name, plot = plot)
  }
}