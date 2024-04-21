#------------------------------
# Heatmaps - chlorophyll-a for all mesocosms
#------------------------------

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

# Make a subset of the data frame with just noon values and with just the mesocosms we're putting in this figure
data_12 <- data %>% subset(hour_profile.datetime == "12") %>%
  #subset(enclosure %in% c("E07", "E17", "E19", "E23", "E01", "E09", "E18", "E24")) %>%
  subset(chlorophyll.a < 20)

# FUNCTION
test_function <- function(data, enclosure.id, parameter.name, max_depth, contour_breaks) {
  
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
    geom_contour_filled(aes(x = sampledate, y = specified.depth, z = var), alpha = 0.8,
                        breaks = contour_breaks) +
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
          axis.text = element_text(size = 22),
          axis.title = element_text(size = 24),
          panel.grid = element_blank(),
          legend.position = "left")
}

# Example usage:
# test_function(data = data_12, enclosure.id = "E01", max_depth = 20,
#               parameter.name = "chlorophyll.a",  
#               contour_breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26))

# Iterating over all mesocosms:

# List of enclosure IDs and their corresponding max depths
enclosure_info <- list(
  # "E01" = 19.5,
  # "E02" = 19.5,
  # "E07" = 16.5,
  # "E08" = 16.5,
  # "E09" = 16.5,
  # "E10" = 16.5,
  # "E12" = 17.5,
  # "E14" = 19.5,
  # "E16" = 19.0,
  # "E17" = 18.0,
  # "E18" = 17.5,
  # "E19" = 16.0,
  # "E23" = 15.5,
  # "E24" = 16.5,
  "L01" = 20.5)

# Set other parameters
parameter_name <- "chlorophyll.a"
contour_breaks <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)

# Create plots for each enclosure ID
for (enclosure_id in names(enclosure_info)) {
  max_depth <- enclosure_info[[enclosure_id]]
  
  # Call test_function with current enclosure ID and parameters
  plot <- test_function(data = data_12, 
                        enclosure.id = enclosure_id, 
                        max_depth = max_depth, 
                        parameter.name = parameter_name, 
                        contour_breaks = contour_breaks)
  
  # Construct file name
  file_name <- paste0("heatmap_plots_pngs/chlorophyll/12/legends/", enclosure_id, "_", parameter_name, ".png")

  # Save the plot
  ggsave(filename = file_name, plot = plot)
}