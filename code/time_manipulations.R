#------------------------------
# remove dates before 04-25-2023; shorten time format to Hour; remove extra date columns
#------------------------------

# load packages
library(dplyr)
library(lubridate)

# create dataframe for time manipulations
master_times <- master_cleaned_3

# get rid of rows with dates before 04-25-2023
master_times <- master_times %>%
  filter(profile.datetime >= ymd("2023-04-25"))

# keep only hour portion of time column, store in new column "hour_profile.datetime"
master_times$hour_profile.datetime <- hour(master_times$profile.datetime)

# get rid of extra date/time columns
master_times <- master_times %>%
  select(-c(datetime, day_datetime, month_datetime, year_datetime, time_datetime, time_profiledatetime ))