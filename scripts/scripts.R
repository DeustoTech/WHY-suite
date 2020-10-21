#' This is a compilation of simple scripts to execute functions of the `whyT2.1` library. 
#' 
#' KEY OF ABBREVIATIONS:
#' DSET = dataset | EXT = extended | FEAT = feature
#' PCA = principal component analysis | RAW = raw | TS = time series | W/ = with
#' 
#' LIST OF SCRIPTS:
#' 
#' ** DATASET MANAGEMENT **
#' `1` Create EXT DSETs from folder of RAW DSETs
#' 
#' ** FEATURE EXTRACTION **
#' `4` Get FEATs of (1-month LCL RAW) DSETs from folder
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
#' 

################################################################################
script_selection <- 5
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
    o <- whyT2.1::get_features_of_datasets_in_folder(
      folder_path, from_date, to_date, dset_key, allowed_na, type_of_analysis)
    return(o)
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
    
  }
  
  # ----------------------------------------------------------------------------
}

# Execute selected script

scripts(script_selection)
