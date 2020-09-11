# Carlos Quesada - Universidad de Deusto
# 2020.07.03
# Source file

#-- LIBRARIES

library(tidyverse)
library(tsfeatures)
library(lubridate)
library(forecast)
library(moments)

# Load config file
source("config.R")

# FEATURE FUNCTIONS
stat_moments <- function(x) {
  list(
    mean     = mean(x),
    variance = var(x),
    skewness = skewness(x),
    kurtosis = kurtosis(x)
  )
}

quantiles <- function(x) {
  q <- quantile(x)
  list(
    minimum        = q[[1]],
    lower_quartile = q[[2]],
    median         = q[[3]],
    upper_quartile = q[[4]],
    maximum        = q[[5]]
  )
}
  
electricity <- function(x) {
  list(
    load_factor = mean(x)/max(x)
  )
}

# PROGRAM FUNCTIONS

#-- Get File List
Get_File_List <- function(dset_key, dset_tseries) {
  # Get folder
  folder <- paste(
    glb_root_folder, 
    glb_subfolders[[dset_key]],
    dset_tseries,
    "/",
    sep = ""
  )
  # Get list of files
  list.files(folder)
}

#-- Load dataset file
Load_Dataset_File <- function(dset_key, dset_tseries, filename) {
  # Get folder
  folder <- paste(
    glb_root_folder, 
    glb_subfolders[[dset_key]],
    dset_tseries,
    "/",
    sep = ""
  )
  # Load data from CSV file
  data <- read.table(
    file = paste(folder, filename, sep = ""),
    header = FALSE,
    sep = ","
  )
  # Times
  times <- ymd_hms(data$V1)
  # Values
  values <- data$V2
  # Create tibble
  tibble(times = times, values = values)
}

#-- Get dataset values inside a time interval
Get_Data_Interval <- function(tm_series, from_date, to_date, step) {
  # Time series ends
  first_ts_date <- tm_series[[1, 1]]
  last_ts_date <- tail(tm_series, 1)[[1, 1]]
  # Interval is included in time series
  if (first_ts_date <= from_date & to_date <= last_ts_date) {
    # Step as difftime
    step_in_secs <- as.difftime(step, units = "secs")
    # Create time sequence
    time_seq <- seq(from_date, to_date, step_in_secs)
    # Find matches in timeseries
    mm <- match(time_seq, tm_series$times)
    # Output vector
    return(tm_series$values[mm])
  # Interval is NOT included in time series
  } else {
    return(NULL)
  }
}
