# Carlos Quesada - Universidad de Deusto
# 2020.07.03
# Source file

#-- LIBRARIES

library(tidyverse)
library(tsfeatures)
library(lubridate)
library(forecast)
library(moments)

#-- GLOBAL VARS (IN CAPITAL LETTERS)

# Path to the dataset folder
DATASET_PATH <- "G:/Mi unidad/WHY/Datasets/lcl/"
# Number of samples in a day for the dataset
SAMPLES_PER_DAY <-
  list(
    lcl = 48
  )

# List of functions that will be passed to tsfeatures.
# -- Some functions were not included because they:
# -- TOOK A LOT OF TIME:
# -- "compengine", "sampenc", "sampen_first", "fluctanal_prop_r1"
# -- DIDN'T WORK:
# -- "binarize_mean", "embed2_incircle" (require param), "hw_parameters"
BASIC_ANALYSIS_LIST <-
  c(
    "stat_moments", "quantiles", "electricity",
    "frequency" , "stl_features", "entropy", "acf_features"
  )
EXTRA_ANALYSIS_LIST <-
  c(
    "stat_moments", "quantiles", "electricity",
    "frequency", "stl_features", "entropy", "acf_features" , "max_kl_shift",
    "outlierinclude_mdrmd", "arch_stat", "max_level_shift", "ac_9",
    "crossing_points", "max_var_shift", "nonlinearity",
    "spreadrandomlocal_meantaul" , "flat_spots", "pacf_features", "firstmin_ac",
    "std1st_der", "heterogeneity", "stability", "firstzero_ac", "trev_num", 
    "holt_parameters", "walker_propcross", "hurst", "unitroot_kpss",
    "histogram_mode", "unitroot_pp", "localsimple_taures", "lumpiness",
    "motiftwo_entro3"
  )

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

################################################################################
#-- Load dataset file
################################################################################

Load_Dataset_File <- function(dataset_path, filename) {
  # Load data from CSV file
  data <- read.table(
    file = paste(dataset_path, filename, sep = ""),
    header = FALSE,
    sep = ","
  )
  # Times
  times <- ymd_hms(data$V1)
  # Values
  values <- data$V2
  # Create tibble
  data.frame(times = times, values = values)
}

################################################################################
#-- Get dataset values inside a time interval
################################################################################
# REMARKS:
# "sampling_period" (measured in seconds) is needed as an input parameter to 
# check non-existing dates, which must be indicated as NaN.
################################################################################

Get_Data_Interval <- function(dset_data,
                              from_date,
                              to_date,
                              sampling_period
                              ) {
  # Time series ends
  first_ts_date <- dset_data[[1, 1]]
  last_ts_date <- tail(dset_data, 1)[[1, 1]]
  # Interval is included in time series
  if (first_ts_date <= from_date & to_date <= last_ts_date) {
    # Step as difftime
    period_in_secs <- as.difftime(sampling_period, units = "secs")
    # Create time sequence
    time_seq <- seq(from_date, to_date, period_in_secs)
    # Find matches in timeseries
    mm <- match(time_seq, dset_data$times)
    # Output 
    output <- data.frame(times = time_seq, values = dset_data$values[mm])
    return(output)
    # Interval is NOT included in time series
  } else {
    return(NULL)
  }
}

################################################################################
#-- Plot a data_info row
################################################################################
# EXAMPLE OF USE:
# Plot_Data_Info_Row(data_info["75220",])
################################################################################

Plot_Data_Info_Row <- function(di_row, dset_key="lcl") {
  # How long a step is in seconds
  step_in_secs <- 86400 / SAMPLES_PER_DAY[[dset_key]] # 30 * 60
  # Load a complete time series from the dataset
  dset_data <- Load_Dataset_File(DATASET_PATH, di_row[[1]])
  # Dates
  from_date <- as.POSIXct(di_row[[2]], format = "%Y-%m-%d %H:%M:%S", tz="GMT")
  to_date   <- as.POSIXct(di_row[[3]], format = "%Y-%m-%d %H:%M:%S", tz="GMT")
  # Interval
  dset_data_intvl <- Get_Data_Interval(
    tm_series = dset_data,
    from_date = from_date,
    to_date   = to_date,
    step      = step_in_secs 
  )
  # Plot
  plot(
    seq(from_date, to_date, as.difftime(step_in_secs, units = "secs")),
    dset_data_intvl,
    type = "l",
    main = di_row[[1]],
    xlab = "Date",
    ylab = "kWh",
    ylim = c(0,5)
  )
}

################################################################################
#-- Function to plot an interval of a time series 
################################################################################

Plot_Data <- function(dset_data, title=NULL) {
  # Create plot
  p <- plot(
    x    = dset_data[[1]],
    y    = dset_data[[2]],
    type = "l",
    main = title,
    xlab = "Date",
    ylab = "kWh",
    ylim = c(0,5)
  )
}

################################################################################
#-- Function to create a library of features
################################################################################

Create_Features_Library <- function(sampling_period,
                                    feats_folder, 
                                    feats_to_plot
                                    ) {
  # Libraries
  library(plotly)
  
  # Load data from CSV files
  feats <- read.table(
    file   = paste(feats_folder, "feats.csv", sep = ""),
    header = TRUE,
    sep    = ","
  )
  data_info <- read.table(
    file   = paste(feats_folder, "data_info.csv", sep = ""),
    header = TRUE,
    sep    = ","
  )
  
  # Indices of series to be plotted
  seq_idx <- seq(1, nrow(feats), length=9)
  
  for (ii in feats_to_plot) {
    print(ii)
    # Feature name
    feat_name <- names(feats)[ii]
    # Get representative filenames of this feature ii
    sorted_col    <- sort(feats[[ii]], index.return = TRUE)
    selected_idx  <- sorted_col$ix[seq_idx]
    repres_fnames <- data_info$filename[selected_idx]
    repres_from_D <- as.POSIXct(data_info$from_date[selected_idx], tz="GMT")
    repres_to_D   <- as.POSIXct(data_info$to_date[selected_idx], tz="GMT")
    repres_values <- sorted_col$x[seq_idx]
    
    # Plot in PDF file
    pdf(
      file   = paste(ii, " - ", feat_name, ".pdf", sep = ""),
      paper  = "special", #"a4r",
      width  = 20,
      height = 15
    )
    par(mfrow = c(3,3))
    
    for (jj in 1:9) {
      # Load dataset file
      dset_data <- Load_Dataset_File(DATASET_PATH, repres_fnames[jj])
      # Get values from dataset file
      dset_data_intvl <- Get_Data_Interval(
        dset_data       = dset_data,
        from_date       = repres_from_D[jj],
        to_date         = repres_to_D[jj],
        sampling_period = sampling_period
      )
      # Plot interval
      Plot_Data(
        dset_data = dset_data_intvl,
        title     = paste(feat_name, "=", repres_values[jj])
      )
    }
    dev.off()
  }
}
