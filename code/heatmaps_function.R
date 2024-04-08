#------------------------------
# Heatmaps - iterative with a function
#------------------------------

# Load packages
library(tidyverse)
library(tidyverse)
library(scales)
library(rLakeAnalyzer)
library(metR)

# Bring in our data frame with the obvious outliers removed
data <- no_boxplot_outliers

# Make a column with just the date
data <- data %>% mutate(sampledate = date(profile.datetime))


# Make a subset of the data frame with just noon values and just E01
data_12 <- data %>% subset(hour_profile.datetime == "12") 

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

# Example usage:
test_function(data = data_12, enclosure.id = "E01", max_depth = 20,
              parameter.name = "oxygen.concentration",  
              contour_breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20))






library(purrr)

# Function to create plots iteratively
create_plots <- function(data, enclosure_ids, max_depths, parameter_names, contour_breaks_list) {
  plots_list <- list()
  
  for (enclosure_id in enclosure_ids) {
    enclosure_plots <- list()
    
    for (i in seq_along(parameter_names)) {
      parameter <- parameter_names[[i]]
      contour_breaks <- contour_breaks_list[[i]]$contour_breaks
      
      plot <- test_function(data = data, 
                            enclosure.id = enclosure_id, 
                            parameter.name = parameter, 
                            max_depth = max_depths[[i]], 
                            contour_breaks = contour_breaks)
      
      plot_name <- paste0("heatmap_plots_pngs/test/", enclosure_id, "_", parameter, ".png")
      ggsave(filename = plot_name, plot = plot, width = 30, height = 18)
      
      enclosure_plots[[parameter]] <- plot_name
    }
    
    plots_list[[enclosure_id]] <- enclosure_plots
  }
  
  return(plots_list)
}

# Example data
data_12 <- data_12

# # Example lists of parameters
# enclosure_ids <- c("E01", "E02", "E07", "E08", "E09", "E10", "E12", "E14", "E16", "E17", "E18", "E19", "E23", "E24", "L01")  # list of enclosure ids
# max_depths <- list(19.5, 19.5, 16.5, 16.5, 16.5, 16.5, 17.5, 19.5, 19.0, 18.0, 17.5, 16.0, 15.5, 16.5, 20.5)  # corresponding max depths

# # Example lists of parameters
enclosure_ids <- c("E01", "E02", "E07")  # list of enclosure ids
max_depths <- list(19.5, 19.5, 16.5)  # corresponding max depths


parameter_names <- c("oxygen.concentration", "water.temperature", "chlorophyll.a")  # list of parameter names
contour_breaks_list <- list(
  list(contour_breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18)),
  list(contour_breaks = c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22)),
  list(contour_breaks = c(6, 7, 8, 9, 10, 11)),
  list(contour_breaks = c(200, 250, 300, 350, 400, 450, 500)),
  list(contour_breaks = c(0, 20, 40, 60, 80, 100, 120)),
  list(contour_breaks = c(-1, 0, 1, 2, 3, 4, 5)),
  list(contour_breaks = c(-3000, 0, 3000, 6000))
)  # list of contour breaks for each parameter

# Create plots iteratively
plots <- create_plots(data = data_12,
                      enclosure_ids = enclosure_ids,
                      max_depths = max_depths,
                      parameter_names = parameter_names,
                      contour_breaks_list = contour_breaks_list)

# Example usage:
# Access plots for a specific enclosure id
#print(plots[["E01"]])

# Access plots for a specific parameter in a specific enclosure id
#print(plots[["E01"]][["oxygen.concentration"]])

#png(filename = "path/to/your/plot.png")

