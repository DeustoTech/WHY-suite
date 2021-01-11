#' This is a compilation of simple scripts to execute functions of the `whyT2.1` library. 
#' 
#' KEY OF ABBREVIATIONS:
#' DSET = dataset | EXT = extended | FEAT = feature | GOI = Goiener | LCL = Low Carbon London | PARPCA = principal component analysis | RAW = raw | TS = time series | W/ = with
#' 
#' LIST OF SCRIPTS:
#' 
#' ** DATASET MANAGEMENT **
#' `1`  Create EXT DSETs from folder of RAW DSETs (LCL)
#' `16` Create EXT DSETs from folder of RAW DSETs (GOI)
#' 
#' ** FEATURE CHECK **
#' `10` Extraction of seasonal bins of TS
#' `11` Extraction of load factors of TS
#' 
#' ** FEATURE EXTRACTION **
#' `4`  Get FEATs of (1-month LCL RAW) DSETs from folder
#' `9`  Get FEATs of 1 LCL EXT DSET file
#' `12` Get FEATs of LCL EXT DSET from folder
#' 
#' ** MACHINE LEARNING TOOLS **
#' `6`  Compute PCA from CSV file of FEATs (W/ plots)
#' `7`  Compute k-means from CSV file of FEATs (W/ plots)
#' `8`  Compute k-means of PCA from CSV file of FEATs (W/ plots)
#' `14` Compute k-means of PCA from CSV file of FEATs & EXT DSETs (W/ plots)
#' `15` Compute DBSCAN
#'  
#' ** PLOTTING DATA **
#' `5`  Plot an LCL EXT DSET
#' `3`  Create visual PDF library of FEATs
#' 
#' ** TIME SERIES GENERATION **
#' `2`  Create TS from FEATs using GRATIS

library(whyT2.1)
library(foreach)

################################################################################
script_selection <- 20
################################################################################

scripts <- function(script_selection) {
  # ----------------------------------------------------------------------------
  
  # SCRIPT 1
  if (script_selection == 1) {
    # User parameters
    input_folder  <- "G:/Mi unidad/WHY/Datasets/lcl/"
    output_folder <- "G:/Mi unidad/WHY/Datasets/test.BORRAR/"
    metadata_file <- "G:/Mi unidad/WHY/Datos (raw)/Low Carbon London/informations_households.csv"
    
    # Function call
    whyT2.1::extend_dataset(
      input_folder, 
      output_folder, 
      wanted_days = 800,
      dset_key = "lcl",
      metadata_files = metadata_file
    )
    
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
    feats <- whyT2.1::get_features_from_raw_datasets(
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
    load("G:/Mi unidad/WHY/Datasets/lcl-ext/MAC002002")
    # Get ts from edf
    tseries <- get_timeseries_from_cooked_dataframe(edf)
    # Compute raw seasonal features
    seas_feats <- whyT2.1::get_seasonal_features_from_timeseries(tseries)
    
    return(seas_feats)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 11
  if (script_selection == 11) {
    # Load extended dataframe (edf) from file
    load("G:/Mi unidad/WHY/Datasets/lcl-ext/MAC002002")
    # Get ts from edf
    tseries  <- get_timeseries_from_cooked_dataframe(edf)
    # Compute load factors
    load_factors <- whyT2.1::load_factors(tseries)
    
    return(load_factors)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 12
  if (script_selection == 12) {
    # User parameters
    input_folder     <- "C:/goiener-ext/" #"G:/Mi unidad/WHY/Datasets/goiener-ext/"
    output_folder    <- "C:/goiener-ext/" #"G:/Mi unidad/WHY/Datasets/test.BORRAR/"
    type_of_analysis <- "extra"
    # Compute features
    feats <- whyT2.1::get_features_from_ext_datasets(
      input_folder, output_folder, type_of_analysis, parallelize=FALSE)
    
    return(feats)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 13 --> GOIENER FILES
  if (script_selection == 13) {
    ### USER DEFINED VARIABLES
    # Goiener data input folder
    g_input <- "C:/GOIENER/"
    # Goiener data output folder
    g_output <- "C:/GOIENER_OUTPUT/"
    # Goiener info file
    g_info_file <- paste(
      "G:/.shortcut-targets-by-id/1g1D2rJfAektwZCB-O_F0EHxDYHxJmhmc/", 
      "20WHY datasets/GOIENER/Contratos_Goiener_20201013-anonymized.csv",
      sep = "")

    ### INITIALIZATIONS
    # List of users and filenames
    user_list <- list()
    # Counter 
    counter <- 0
    
    # Get list of filenames in dataset folder
    filenames <- list.files(g_input)
    total_fnames <- length(filenames)
    # Load Goiener info dataframe
    g_info_df <- data.table::fread(
      file = g_info_file,
      header = TRUE,
      sep = ",",
      na.strings = ""
    )
    
    # File by file
    for (filename in filenames[1:2]) {
      # Counter
      counter <- counter + 1
      # Print file being analyzed
      print(paste(date(),
                  filename,
                  round(100*counter/total_fnames, 4), 
                  sep=" | "))
      # Load Goiener file
      g_df <- data.table::fread(
        file = paste(g_input, filename, sep=""),
        header = FALSE,
        sep = ";",
        na.strings = ""
      )
      # Unique users in file
      unique_users <- unique(g_df$V1)
      for (uu in unique_users) {
        # Save filename in list of users
        user_list[[uu]] <- c(user_list[[uu]], filename)
        # Select all uu entries in df
        uu_entries <- g_df$V1 == uu
        # Create new dataframe
        uu_df <- g_df[uu_entries, ]
        # Save
        data.table::fwrite(
          x = uu_df,
          file = paste(g_output, uu, sep=""),
          append = TRUE,
          quote = FALSE,
          sep = ",",
          row.names = FALSE,
          col.names = FALSE
        )
      }
    }
    # Save list of users
    save(user_list, file=paste(g_output, "user_list.RData", sep=""))
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 14 --> ANALYSIS OF FEATURES FROM EXTENDED TIME SERIES
  if (script_selection == 14) {
    # Folder to features' file
    feats_folder <- "C:/Users/carlos.quesada/Documents/temptative-results/"
    ftp          <- c(5:9, 60:83, 128:134, 168:179)
    centers      <- 3
    
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
    
    # Load features
    feats <- utils::read.table(
      file   = paste(feats_folder, "feats.csv", sep = ""),
      header = TRUE,
      sep    = ","
    )
    
    # Plotting constants
    cols1_block <- c(5, 60, 128, 168)
    cols2_block <- c(9, 83, 134, 179)
    x1_block <- c(1, 0, 1, 1)
    x2_block <- c(5, 23, 7, 12)
    
    # Analysis
    for (cc in 1:centers) {
      cluster_idx <- which(km$results$cluster == cc)
      feat_means  <- colMeans(feats[cluster_idx,])
      feat_vars   <- Rfast::colVars(as.matrix(feats[cluster_idx,]))
      feat_df     <- data.frame(feat_means, feat_vars)
      
      title <- paste(
        "Cluster #", cc, " - Elements: ",
        length(cluster_idx),
        sep = "")
      for (ii in 1:4) {
        p <- 
          # Data to be plotted
          ggplot2::ggplot(
            data    = feat_df[cols1_block[ii]:cols2_block[ii],],
            mapping = ggplot2::aes(x=x1_block[ii]:x2_block[ii], y=feat_means)
          ) +
          # Type of graph (line in this case)
          ggplot2::geom_line() +
          # Title
          ggplot2::ggtitle(title) +
          # Labels
          ggplot2::labs(x = "Features", y = "Value") + 
          # Axis limits
          ggplot2::scale_y_continuous() + 
          # Error bars
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = feat_means - sqrt(feat_vars),
                         ymax = feat_means + sqrt(feat_vars)))
        print(p)
      }
    }
    
    return(km)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 15 --> DBSCAN
  if (script_selection == 15) {
    # Folder to features file
    feats_folder <- "C:/Users/carlos.quesada/Documents/temptative-results/"
    ftp          <- c(5:9, 60:83, 128:134, 168:179)
    # Load features
    feats <- utils::read.table(
      file   = paste(feats_folder, "feats.csv", sep = ""),
      header = TRUE,
      sep    = ","
    )
    
    min_var=0.9
    
    # PCA
    pca <- stats::prcomp(feats[, ftp], scale. = TRUE)
    # Get variance
    variance <- summary(pca)[["importance"]]
    # Number of principal components to select
    pc_number <- sum(variance[3,] < min_var) + 1
    print(paste("Selected", pc_number, "PCs, variance", variance[3,pc_number]))
    # Selection of the reduced set of PCA components
    reduced_pc_set <- pca$x[,1:pc_number]
    
    # DBSCAN
    res <- dbscan::optics(reduced_pc_set) #, eps=5, minPts = 5)
    print(res)
    return(res)
  }
  
  # ----------------------------------------------------------------------------  
  # SCRIPT 16
  if (script_selection == 16) {
    # User parameters
    input_folder  <- "G:/Mi unidad/WHY/Datasets/goiener/"
    output_folder <- "G:/Mi unidad/WHY/Datasets/test.BORRAR/"
    metadata_file <- "G:/Mi unidad/WHY/Datos (raw)/GOIENER/Contratos_Goiener_20201013-anonymized.csv"
    
    # Function call
    whyT2.1::extend_dataset(
      input_folder, 
      output_folder, 
      wanted_days = 800,
      dset_key = "goi",
      metadata_files = metadata_file,
      to_date = as.POSIXct("2020-02-29 23:00:00", tz="GMT"),
      extend_after_end = FALSE
    )
    
    return(NULL)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 17
  if (script_selection == 17) {
    # Folder
    input_folder <- "C:/goiener-ext/"
    # Get list of dir names in dataset folder
    dir_names <- list.files(input_folder)
    # Function of actions to be implemented
    goiener_ext_analyzer <- function(x) {
      # Load the extended file
      load(paste(input_folder, x, sep=""))
      # Data list
      return_list <- list(
        filename             = edf$cups,
        TS_length_in_seconds = length(edf$df$values)*3600,
        TS_length_in_years   = length(edf$df$values)/8766,
        TS_length            = "1 year or more",
        sampling_period_in_seconds = 3600,
        sampling_period      = "60 min or more",
        number_of_samples    = length(edf$df$values),
        missing_samples      = edf$number_of_na,
        list_of_missed_samples = NA,
        spatial_resolution   = "household",
        type_of_submetering  = NA,
        NACE_code            = edf$cnae,
        NACE_subcode         = NA,
        city                 = edf$municipality,
        country              = "Spain",
        units                = "kWh",
        tariff               = edf$tariff,
        social_category      = edf$zip_code,
        weather_info         = "no",
        curation             = NA,
        synthetic            = "no"
      )
      return(return_list)
    }
    
    ext_list <- lapply(dir_names, goiener_ext_analyzer)
    ext_df   <- do.call(rbind.data.frame, ext_list)
    # Save
    data.table::fwrite(
      x = ext_df,
      file = paste(input_folder, "@dataset_info.csv", sep=""),
      quote = FALSE,
      sep = ";",
      row.names = FALSE,
      col.names = TRUE
    )
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 18 --- SEMS
  if (script_selection == 18) {
    # SEMS data folder
    sems_input <- "G:/Mi unidad/WHY/Datos (raw)/SEMS"
    # Get list of dir names in dataset folder
    dir_names <- list.dirs(sems_input)
    browser()
    # Dir loop
    for (dir_name in dir_names) {
      # Get list of filenames in dataset folder
      file_names <- list.dirs(paste(sems_input, dir_name, sep="/"))
      # File loop
      for (file_name in file_names) {
        # Load file
        g_df <- data.table::fread(
          file = paste(sems_input, dir_name, file_name, sep="/"),
          header = TRUE,
          sep = ";",
          na.strings = ""
        )
        
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 19 - SCRIPT TO CORRECT THE REPEATED THIRD SEASONAL PERIODS (GOIENER)
  if (script_selection == 19) {
    # Path to the file of repetitions
    rep_file <- "C:/correct/correct_files.csv"
    # Path to the folder of ext Goiener files
    goi_folder <- "G:/Mi unidad/WHY/Datasets/goiener-ext/"
    # Path to the output folder
    output_folder <- "C:/correct/"
    # Load list of incorrect ext files
    ext_files <- data.table::fread(file = rep_file, header = F)
    # # Setup parallel backend to use many processors
    # cores <- parallel::detectCores() - 1
    # cl <- parallel::makeCluster(cores)
    # doParallel::registerDoParallel(cl)
    # Correction loop
    # foreach::foreach(ii = 1:dim(ext_files)[1]) %dopar% {
    for (ext_file in ext_files$V1) {
      load(paste(goi_folder, ext_file, sep=""))
      lst <- length(edf$seasonal_periods)
      if (edf$seasonal_periods[lst-1] == edf$seasonal_periods[lst]) {
        edf$seasonal_periods <- head(edf$seasonal_periods, -1)
        save(edf, file=paste(output_folder, ext_file, sep=""))
      } else {
        print(ext_file)
      }
    } 
    # # Stop parallelization
    # parallel::stopCluster(cl)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 20
  if (script_selection == 20) {
    # User parameters
    input_folder  <- "G:/Mi unidad/WHY/Datos (raw)/REFIT/out/"
    output_folder <- "G:/Mi unidad/WHY/Datos (raw)/REFIT/ext/"
    #metadata_file <- "G:/Mi unidad/WHY/Datos (raw)/Low Carbon London/informations_households.csv"
    
    # Function call
    whyT2.1::extend_dataset(
      input_folder, 
      output_folder, 
      wanted_days = 800,
      dset_key = "ref" 
      #metadata_files = metadata_file
    )
    
    return(NULL)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 21
  if (script_selection == 21) {
    
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 22
  if (script_selection == 22) {
    
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 23
  if (script_selection == 23) {
    
  }
  
  # ----------------------------------------------------------------------------
}

# Execute selected script

result <- scripts(script_selection)
