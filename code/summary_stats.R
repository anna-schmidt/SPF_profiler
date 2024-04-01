#------------------------------
# summary stats
#------------------------------

# Summary stats for each variable in master_times

variables <- c("water.temperature","conductivity","pH.value","chlorophyll.a","phycocyanin","oxygen.concentration","photosynthetically.active.radiation.up")

summary_stats <- as.data.frame(sapply(master_times[variables], summary))
summary_stats

#------------------------------

# Removing outliers visually identified from boxplots

## chlorophyll.a > 100 (two outliers)
## phycocyanin > 4 (six outliers)
## conductivity < 200 (one outlier)

#create vectors with values we do not want
no_outliers_chlorophyll.a <- master_times$chlorophyll.a > 100
no_outliers_phycocyanin <- master_times$phycocyanin > 4
no_outliers_conductivity <- master_times$conductivity < 200

#create new dataframe omitting the rows (note- whole rows) where there is a value we do not want

no_boxplot_outliers <- master_times[!no_outliers_chlorophyll.a & !no_outliers_phycocyanin & !no_outliers_conductivity,]

summary_stats2 <- as.data.frame(sapply(no_boxplot_outliers[variables], summary))
summary_stats2

#------------------------------

# Removing outliers based on 1.5*IQR threshold

remove_outliers <- function(column, threshold = 1.5) {
  Q1 <- quantile(column, 0.25) # establish quatiles
  Q3 <- quantile(column, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - threshold * IQR # establish thresholds
  upper_bound <- Q3 + threshold * IQR
  no_IQR_outliers <- ifelse(column < lower_bound | column > upper_bound, NA, column) # remove outliers, replace the missing values with NAs
  return(no_IQR_outliers)
}

# Apply function to defined variables columns
no_IQR_outliers <- lapply(master_times[variables], remove_outliers)
# Turn that into a dataframe
no_IQR_outliers_df <- data.frame(lapply(no_IQR_outliers, as.vector))
# Get rid of rows (again, whole rows) with NAs
no_IQR_outliers_df <- na.omit(no_IQR_outliers_df)

summary_stats3 <- as.data.frame(sapply(no_IQR_outliers_df[variables], summary))
summary_stats3

#------------------------------
# Graph all, see changes visually

## Boxplots for master_times

subset <- master_times %>%
  select(c(water.temperature:photosynthetically.active.radiation.up))

plot.new()
par(mfrow = c(3, 3))  # setting up the plotting layout
plot<- lapply(variables, function(col) {
  boxplot(subset[[col]], main = col, ylab = "Value")
})

## Boxplots for no_boxplot_outliers

subset2 <- no_boxplot_outliers %>%
  select(c(water.temperature:photosynthetically.active.radiation.up))

plot.new()
par(mfrow = c(3, 3))  # setting up the plotting layout
plot2<- lapply(names(subset2), function(col) {
  boxplot(subset2[[col]], main = col, ylab = "Value")
})

## Boxplots for no_IQR_outliers_df

subset3 <- no_IQR_outliers_df %>%
  select(c(water.temperature:photosynthetically.active.radiation.up))

plot.new()
par(mfrow = c(3, 3))  # setting up the plotting layout
plot3<- lapply(names(subset3), function(col) {
  boxplot(subset3[[col]], main = col, ylab = "Value")
})

#------------------------------
### Next:
# decide which outlier metric to use
# create violin plots without outliers?
