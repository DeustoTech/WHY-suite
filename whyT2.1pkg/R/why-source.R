# Carlos Quesada - Universidad de Deusto
# 2020.07.03
# Source file

#-- LIBRARIES

# library(tidyverse)
# library(tsfeatures)
# library(lubridate)
# library(forecast)
# library(moments)

#-- ESTABLISH A SEED FOR THE RANDOM NUMBERS
set.seed(1981)

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

################################################################################
##  stat_moments()
################################################################################

#' Features of statistical moments
#'
#' @description
#' Compute the mean, variance, skewness and kurtosis of a time series.
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the mean, variance, skewness and kurtosis of the time series.
#'
#' @export

stat_moments <- function(x) {
  list(
    mean     = mean(x),
    variance = var(x),
    skewness = moments::skewness(x),
    kurtosis = moments::kurtosis(x)
  )
}

################################################################################
##  quantiles()
################################################################################

#' Features of quantiles
#'
#' @description
#' Compute the minimum, lower quartile, median, upper quartile and maximum of a time series.
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the minimum, lower quartile, median, upper quartile and maximum of the time series.
#'
#' @export

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

################################################################################
##  electricity()
################################################################################

#' Features of electricity
#'
#' @description
#' Compute the load factor across days of a time series.
#'
#' @param x Time series of class `msts`.
#'
#' @return A list with the mean and variance of the load factors across days in the time series.
#' @export

electricity <- function(x) {
  # Get samples per day
  samples_per_day <- attr(x, "msts")[1]
  # Convert TS to matrix of 28 days x "samples_per_day"
  x_matrix <- t(matrix(x, nrow=samples_per_day))
  # List of load factors (one per day)
  load_factors <- rowMeans(x_matrix)/Rfast::rowMaxs(x_matrix, value=TRUE)
  # Return
  list(
    mean_load_factors = mean(load_factors),
    var_load_factors = var(load_factors)
  )
}

################################################################################
##  dataset_to_raw_dataframe()
##  OLD: Load_Dataset_File()
################################################################################

#' Raw dataframe from dataset
#'
#' @description
#' Get a dataframe from a dataset contained in a CSV file.
#'
#' @param csv_file Path (string) to the CSV file containing the dataset.
#'
#' @return Raw dataframe, i.e. without checking missing samples.
#'
#' @export

dataset_to_raw_dataframe <- function(csv_file) {
  # Load data from CSV file
  data <- data.table::fread(
    file = csv_file,
    header = FALSE,
    sep = ","
  )
  # Times
  times <- as.POSIXct(data$V1, tz="GMT")
  # Values
  values <- data$V2
  # Create dataframe
  data.frame(times = times, values = values)
}

################################################################################
##  raw_to_cooked_dataframe()
##  OLD: Get_Data_Interval()
################################################################################

#' Cooked dataframe from raw dataframe
#'
#' @description
#' Cook a raw dataframe. Cooking consists in completing missing samples with NA values and extracting a time interval out of the raw dataframe.
#'
#' @param dset_data Raw dataframe.
#' @param from_date Initial date and time of the interval of class `POSIXct` and time zone GMT.
#' @param to_date Final date and time of the interval of class `POSIXct` and time zone GMT.
#' @param sampling_period Sampling period (measured in seconds) of the raw dataframe. This value is required to complete missing samples with `NaN`.
#'
#' @return Cooked dataframe.
#'
#' @export

raw_to_cooked_dataframe <- function(dset_data, from_date, to_date, sampling_period) {
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
##  plot_datainfo_row()
##  OLD: Plot_Data_Info_Row()
################################################################################

#' Plotting of a row of `data_info` plotting
#'
#' @description
#' Plot the dataset indicated in a row of the file `data_info.csv`.
#'
#' @param di_row Row of `data_info`.
#' @param dset_key Key of the dataset.
#'
#' @return Plot of the dataset.
#'
#' @example
#' \dontrun{
#' plot_datainfo_row(data_info["75220",])
#' }
#'
#' @export

plot_datainfo_row  <- function(di_row, dset_key="lcl") {
  # How long a step is in seconds
  step_in_secs <- 86400 / SAMPLES_PER_DAY[[dset_key]] # 30 * 60
  # Load a complete time series from the dataset
  csv_path <- paste(DATASET_PATH, di_row[[1]], sep="")
  dset_data <- dataset_to_raw_dataframe(csv_path)
  # Dates
  from_date <- as.POSIXct(di_row[[2]], format = "%Y-%m-%d %H:%M:%S", tz="GMT")
  to_date   <- as.POSIXct(di_row[[3]], format = "%Y-%m-%d %H:%M:%S", tz="GMT")
  # Interval
  dset_data_intvl <- raw_to_cooked_dataframe(
    dset_data       = dset_data,
    from_date       = from_date,
    to_date         = to_date,
    sampling_period = step_in_secs
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
##  plot_dataframe()
##  OLD: Plot_Data()
################################################################################

#' Plotting of a dataframe
#'
#' @description
#' Plot a dataframe (either raw or cooked).
#'
#' @param dset_data The dataframe to be plotted.
#' @param title Optional title (`NULL` by default).
#'
#' @return Plot of the dataset.
#'
#' @export

plot_dataframe <- function(dset_data, title=NULL) {
  # Create plot
  p <- plot(
    x    = dset_data[[1]],
    y    = dset_data[[2]],
    col  = "blue",
    type = "l",
    main = title,
    xlab = "Date",
    ylab = "kWh",
    ylim = c(0,5)
  )
}

################################################################################
##  plot_features_library()
##  OLD: Create_Features_Library()
################################################################################

#' Creation of a PDF library of features
#'
#' @description
#' For each feature, the nine most representative values are plotted, including the minimum, the median and the maximum values.
#'
#' @param sampling_period Sampling period of the dataframe.
#' @param feats_folder String with a path to the folder where the features are contained.
#' @param feats_to_plot Vector of the features to be plotted.
#'
#' @return A PDF file per feature with nine plots each.
#'
#' @export

plot_features_library <- function(sampling_period, feats_folder, feats_to_plot) {
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
      csv_path <- paste(DATASET_PATH, repres_fnames[jj], sep="")
      dset_data <- dataset_to_raw_dataframe(csv_path)
      # Get values from dataset file
      dset_data_intvl <- raw_to_cooked_dataframe(
        dset_data       = dset_data,
        from_date       = repres_from_D[jj],
        to_date         = repres_to_D[jj],
        sampling_period = sampling_period
      )
      # Plot interval
      plot_dataframe(
        dset_data = dset_data_intvl,
        title     = paste(feat_name, "=", repres_values[jj])
      )
    }
    dev.off()
  }
}

################################################################################
##  compute_pca_from_features()
##  OLD: Compute_PCA_From_Features()
################################################################################

#' PCA from features
#'
#' @description
#' Compute and plot PCA from the features of the time series.
#'
#' @param feats_folder String with the path to the folder where the features are contained.
#' @param otp Vector of the observations to plot.
#' @param ftp Vector of the features to plot.
#' @param axis_x Integer indicating the principal component to be plotted as axis x.
#' @param axis_y Integer indicating the principal component to be plotted as axis y.
#' @param color_by_SE_vars Boolean indicating if plotted points must be colored according to the socioeconomical variables.
#' @param SE_data_file String with the path to the file with the socioeconomical variables.
#' @param get_point_identity Boolean indicating if plotted points must be identified by clicking on them.
#'
#' @return A list with class `princomp`.
#'
#' @export

compute_pca_from_features <- function(feats_folder, otp, ftp, axis_x, axis_y, color_by_SE_vars, SE_data_file, get_point_identity) {
  # Load data from CSV files
  feats <- read.table(
    file   = paste(feats_folder, "feats.csv", sep = ""),
    header = TRUE,
    sep    = ","
  )

  if (color_by_SE_vars | get_point_identity) {
    # Load data from CSV files
    data_info <- read.table(
      file   = paste(feats_folder, "data_info.csv", sep = ""),
      header = TRUE,
      sep    = ","
    )
  }

  # Color by socioeconomic variables?
  if (color_by_SE_vars) {
    # Load data from CSV files
    SE_vars <- read.table(
      file   = SE_data_file,
      header = TRUE,
      sep    = ","
    )
    # List of analyzed series
    analyzed_series <- substr(data_info[,1], 1, 9)
    # Find analyzed series in SE_vars
    SE_indices <- match(analyzed_series, SE_vars[,1])
    # Get grouped ACORN
    grouped_ACORN <- SE_vars[SE_indices,4]
    grouped_ACORN[grouped_ACORN == "Affluent"]    = "green"
    grouped_ACORN[grouped_ACORN == "Comfortable"] = "orange"
    grouped_ACORN[grouped_ACORN == "Adversity"]   = "red"
    grouped_ACORN[grouped_ACORN == "ACORN-U"]     = "blue"
    # color
    color <- grouped_ACORN
  } else {
    color <- "blue"
  }

  # PCA
  pca <- prcomp(feats[otp, ftp], scale. = TRUE)

  # Plot PCA
  plot(
    pca[["x"]][, axis_x],
    pca[["x"]][, axis_y],
    col = color,
    pch = "+",
    xlab = paste("PC #", axis_x, sep = ""),
    ylab = paste("PC #", axis_y, sep = "")
  )

  # Get point identification
  if (get_point_identity) {
    # Get points
    ts_ids <- identify(
      x = pca[["x"]][, axis_x],
      y = pca[["x"]][, axis_y],
      plot = FALSE
    )
    # Print points
    for (ts_id in ts_ids) {
      print(ts_id)
    }
  }

  return(pca)
}
