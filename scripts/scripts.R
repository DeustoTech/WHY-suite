#' This is a compilation of simple scripts to execute functions of the `whyT2.1` library. 
#' 
#' KEY OF ABBREVIATIONS:
#' DSET = dataset | EXT = extended | FEAT = feature | LCL = Low Carbon London | PCA = principal component analysis | RAW = raw | TS = time series | W/ = with
#' 
#' LIST OF SCRIPTS:
#' 
#' ** DATASET MANAGEMENT **
#' `1` Create EXT DSETs from folder of RAW DSETs
#' 
#' ** FEATURE EXTRACTION **
#' `4` Get FEATs of (1-month LCL RAW) DSETs from folder
#' `9` Get FEATs of LCL EXT DSET
#' 
#' ** MACHINE LEARNING TOOLS **
#' `6` Compute PCA from CSV file of FEATs (W/ plots)
#' `7` Compute k-means from CSV file of FEATs (W/ plots)
#' `8` Compute k-means of PCA from CSV file of FEATs (W/ plots)
#'  
#' ** PLOTTING DATA **
#' `5` Plot an LCL EXT DSET
#' `3` Create visual PDF library of FEATs
#' 
#' ** TIME SERIES GENERATION **
#' `2` Create TS from FEATs using GRATIS

################################################################################
script_selection <- 10
################################################################################

library(whyT2.1)

scripts <- function(script_selection) {
  # ----------------------------------------------------------------------------
  
  # SCRIPT 1
  if (script_selection == 1) {
    # User parameters
    input_folder  <- "G:/Mi unidad/WHY/Datasets/lcl/"
    output_folder <- "G:/Mi unidad/WHY/Datasets/lcl-ext/"
    
    # Function call
    whyT2.1::extend_datasets(input_folder,output_folder)
    
    return(NULL)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 2
  if (script_selection == 2) {
    # Extra libraries
    library(gratis)
    
    # User parameters
    
    # Function call
    gen_ts <- gratis::generate_ts_with_target(
      n = 1,
      ts.length = length(values$values),
      freq = c(ts_freq, 7 * ts_freq),
      seasonal = 2,
      features = c("frequency", "stl_features", "entropy", "acf_features"),
      selected.features = names(feats)[1:24],
      target = as.vector(t(as.data.frame(feats[1:24]))),
      parallel = FALSE
    )
    
    return(gen_ts)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 3
  if (script_selection == 3) {
    # User parameters
    SAMPLES_PER_DAY <- 48
    sampling_period <- 86400 / SAMPLES_PER_DAY
    feats_folder    <- paste("G:/Mi unidad/WHY/Resultados/lcl/features/", 
                             "2012-2013, 0% NA, scale=FALSE, 70 feats/", 
                             sep = "")
    feats_to_plot   <- c(1:10, 15:70)
    
    # Function call
    whyT2.1::plot_features_library(
      sampling_period = sampling_period,
      feats_folder = feats_folder,
      feats_to_plot = feats_to_plot
    )
    
    return(NULL)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 4
  if (script_selection == 4) {
    # User parameters
    folder_path      <- "G:/Mi unidad/WHY/Datasets/lcl/"
    from_date        <- ISOdate(2013, 2, 1, 0, 0, 0)
    to_date          <- ISOdate(2013, 2, 28, 23, 30, 0)
    dset_key         <- "lcl"
    allowed_na       <- 0
    type_of_analysis <- "extra"
    
    # Function call
    feats <- whyT2.1::get_features_of_datasets_in_folder(
      folder_path, from_date, to_date, dset_key, allowed_na, type_of_analysis)
    
    return(feats)
  }

  # ----------------------------------------------------------------------------
  
  # SCRIPT 5
  if (script_selection == 5) {
    # User parameters
    lcl_ext_folder <- "G:/Mi unidad/WHY/Datasets/lcl-ext/"
    lcl_4_digit_id <- "5005"
    from_time      <- as.POSIXct("2013-02-01", tz="GMT")
    to_time        <- as.POSIXct("2013-03-01", tz="GMT")
    
    # Function call
    path     <- paste(lcl_ext_folder, "MAC00", lcl_4_digit_id, sep="")
    load(path)
    # Select time interval
    ival_idx <- edf$df$times >= from_time & edf$df$times <= to_time
    ext_dset <- edf$df[ival_idx,]
    
    whyT2.1::plot_dataframe(
      dset_data = ext_dset, 
      title     = paste("File", lcl_4_digit_id)
      )
    
    return(NULL)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 6
  if (script_selection == 6) {
    # -- All feats                      <- c(1:10, 15:70)
    # -- Statistical feats              <- 1:10
    # -- STL features                   <- 15:26
    # -- Autocorrelation feats          <- 28:34
    # -- Stats + STL + Acorr + Entropy  <- c(1:10, 15:34)
    # -- Quantiles + seasonal strengths <- c(5:9, 21:22)
    # -- Mean, var, + seas. strengths   <- c(1:2, 21:22)
    
    #### User parameters
    # Folder to features' file
    feats_folder <- paste("G:/Mi unidad/WHY/Resultados/lcl/features/",
                          "2013 Feb, 0% NA, scale=FALSE, 70 feats/", sep="")
    # Features to plot
    ftp <- 1:10
    # Color by socioeconomic variables
    SE_data_file <- paste("G:/Mi unidad/WHY/Datos (raw)/Low Carbon London/",
                          "informations_households.csv", sep="")
    color_by_SE_vars <- T
    # Axes selection
    axis_x <- 1
    axis_y <- 2
    
    #### Function calls
    # Compute PCA
    pca <- whyT2.1::pca_from_features(
      feats_folder = feats_folder, 
      ftp          = ftp
      )
    # Plot PCA scores
    pca <- whyT2.1::plot_pca(
      pca              = as.data.frame(pca$x),
      feats_folder     = feats_folder,
      axis_x           = axis_x,
      axis_y           = axis_y,
      color_by_SE_vars = color_by_SE_vars,
      SE_data_file     = SE_data_file
      )
    
    return(pca)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 7
  if (script_selection == 7) {
    # Folder to features' file
    feats_folder <- paste("G:/Mi unidad/WHY/Resultados/lcl/features/",
                          "2013 Feb, 0% NA, scale=FALSE, 70 feats/", sep="")
    # Features to plot
    ftp <- 1:10
    # Compute k-means
    km <- whyT2.1::kmeans_from_features(
      feats_folder = feats_folder,
      ftp          = ftp,
      centers      = 3:5
      )
    # Plot elbow curve
    whyT2.1::plot_kmeans(
      km            = km$results,
      plot_elbow    = TRUE
    )
    # Plot k-means
    whyT2.1::plot_kmeans(
      km            = km$results[[3]],
      feats_df      = km$feats,
      plot_clusters = TRUE
    )
    
    return(km)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 8
  if (script_selection == 8) {
    # Folder to features' file
    feats_folder <- paste("G:/Mi unidad/WHY/Resultados/lcl/features/",
                          "2013 Feb, 0% NA, scale=FALSE, 70 feats/", sep="")
    ftp          <- c(5:9, 21:22)
    centers      <- 4
    # Compute k-means
    km <- whyT2.1::pca_kmeans_analysis(
      feats_folder = feats_folder,
      ftp          = ftp,
      min_var      = 0.90,
      centers      = centers
    )
    # Plot k-means
    whyT2.1::plot_kmeans(
      km            = km[["results"]],
      feats_df      = km[["feats"]],
      plot_clusters = TRUE
    )
    
    return(km)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 9
  if (script_selection == 9) {
    # Load extended dataframe (edf) from file
    load("G:/Mi unidad/WHY/Datasets/lcl-ext/MAC001001")
    # Get features
    feats <- whyT2.1::get_features_from_cooked_dataframe(edf, "extra")
    
    return(feats)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 10
  if (script_selection == 10) {
    # Load extended dataframe (edf) from file
    load("G:/Mi unidad/WHY/Datasets/lcl-ext/MAC005005")
    # Get ts from edf
    tseries  <- get_timeseries_from_cooked_dataframe(edf)
    # Initial date
    ini_date <- get_extrema_dates_from_timeseries(tseries)
    # Date sequence
    samples_per_day <- attr(tseries, "msts")[1]
    date_by  <- as.difftime(24 / samples_per_day, units = "hours")
    date_seq <- seq(from       = ini_date,
                    length.out = length(tseries),
                    by         = date_by)
    
    # ### Group by periods of 1 hour
    # cut_seq <- cut(date_seq, breaks = "1 hour")
    # # Aggregate data (sum) according to the groups
    # aggr_ts <- stats::aggregate(
    #   x   = as.numeric(tseries),
    #   by  = list(date_time = cut_seq),
    #   FUN = sum )
    # ## Estimate incomplete groups (possibly initial and final bins)
    # # Number of elements per group
    # elems_per_group <- samples_per_day / 24
    # # Number of elements in the first group
    # elems_first_group <- sum(as.numeric(cut_seq) == 1)
    # # Perform estimation in the first group
    # if (elems_per_group != elems_first_group) {
    #   aggr_ts[1,2] <- aggr_ts[1,2] * (elems_per_group / elems_first_group)
    # }
    # # ID of the last group
    # id_last_group <- length(levels(cut_seq))
    # # Number of elements in the last group
    # elems_last_group <- sum(as.numeric(cut_seq) == id_last_group)
    # # Perform estimation in the last group
    # if (elems_per_group != elems_last_group) {
    #   aggr_ts[id_last_group, 2] <-
    #     aggr_ts[id_last_group, 2] * (elems_per_group / elems_last_group)
    # }
    # ## Assign meaningful group values
    # nice_groups <- (lubridate::hour(ini_date) + 0:(id_last_group - 1) ) %% 24
    # # Aggregate data (mean) according to the nice groups
    # hourly_mean <- stats::aggregate(
    #   x   = aggr_ts$x,
    #   by  = list(hour = nice_groups),
    #   FUN = mean )
    # # Aggregate data (variance) according to the nice groups
    # hourly_var <- stats::aggregate(
    #   x   = aggr_ts$x,
    #   by  = list(hour = nice_groups),
    #   FUN = stats::var )
    
    # ### Group by periods of 1 weekday
    # cut_seq <- cut(date_seq, breaks = "1 day")
    # # Aggregate data (sum) according to the groups
    # aggr_ts <- stats::aggregate(
    #   x   = as.numeric(tseries),
    #   by  = list(date_time = cut_seq),
    #   FUN = sum )
    # ## Estimate incomplete groups (possibly initial and final bins)
    # # Number of elements per group
    # elems_per_group <- samples_per_day
    # # Number of elements in the first group
    # elems_first_group <- sum(as.numeric(cut_seq) == 1)
    # # Perform estimation in the first group
    # if (elems_per_group != elems_first_group) {
    #   aggr_ts[1,2] <- aggr_ts[1,2] * (elems_per_group / elems_first_group)
    # }
    # # ID of the last group
    # id_last_group <- length(levels(cut_seq))
    # # Number of elements in the last group
    # elems_last_group <- sum(as.numeric(cut_seq) == id_last_group)
    # # Perform estimation in the last group
    # if (elems_per_group != elems_last_group) {
    #   aggr_ts[id_last_group, 2] <-
    #     aggr_ts[id_last_group, 2] * (elems_per_group / elems_last_group)
    # }
    # ## Assign meaningful group values
    # nice_groups <- 
    #   (lubridate::wday(ini_date) - 1 + 0:(id_last_group - 1) ) %% 7 + 1
    # # Aggregate data (mean) according to the nice groups
    # daily_mean <- stats::aggregate(
    #   x   = aggr_ts$x,
    #   by  = list(wday = nice_groups),
    #   FUN = mean )
    # # Aggregate data (variance) according to the nice groups
    # daily_var <- stats::aggregate(
    #   x   = aggr_ts$x,
    #   by  = list(week_day = nice_groups),
    #   FUN = stats::var )
    
    ### Group by periods of 1 month
    cut_seq <- cut(date_seq, breaks = "1 month")
    browser()
    # Aggregate data (sum) according to the groups
    aggr_ts <- stats::aggregate(
      x   = as.numeric(tseries),
      by  = list(date_time = cut_seq),
      FUN = sum )
    ## Estimate incomplete groups (possibly initial and final bins)
    # Number of elements per group
    elems_per_group <- round(samples_per_day * 30.4375)
    # Number of elements in the first group
    elems_first_group <- sum(as.numeric(cut_seq) == 1)
    # Perform estimation in the first group
    if (elems_per_group != elems_first_group) {
      aggr_ts[1,2] <- aggr_ts[1,2] * (elems_per_group / elems_first_group)
    }
    # ID of the last group
    id_last_group <- length(levels(cut_seq))
    # Number of elements in the last group
    elems_last_group <- sum(as.numeric(cut_seq) == id_last_group)
    # Perform estimation in the last group
    if (elems_per_group != elems_last_group) {
      aggr_ts[id_last_group, 2] <-
        aggr_ts[id_last_group, 2] * (elems_per_group / elems_last_group)
    }
    ## Assign meaningful group values
    nice_groups <- 
      (lubridate::wday(ini_date) - 1 + 0:(id_last_group - 1) ) %% 7 + 1
    # Aggregate data (mean) according to the nice groups
    daily_mean <- stats::aggregate(
      x   = aggr_ts$x,
      by  = list(wday = nice_groups),
      FUN = mean )
    # Aggregate data (variance) according to the nice groups
    daily_var <- stats::aggregate(
      x   = aggr_ts$x,
      by  = list(week_day = nice_groups),
      FUN = stats::var )

    print(daily_mean)
    plot(daily_mean)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 11
  if (script_selection == 11) {
    
  }
  
  # ----------------------------------------------------------------------------
}

# Execute selected script

result <- scripts(script_selection)
