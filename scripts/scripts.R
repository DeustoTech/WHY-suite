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
script_selection <- 25
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
    #load("G:/Mi unidad/WHY/Datasets/lcl-ext/MAC001001") MAC003583
    load("G:/Mi unidad/WHY/Datasets/lcl-ext/MAC003597.RData")
    # load("C:/Documents and Settings/carlos.quesada/Documents/goiener_users/2ddd8889c59d39e6460b7c65967d7bb9.RData")
    # Get features
    feats <- whyT2.1::get_features_from_cooked_dataframe(edf, "basic")
    
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
    feats_folder <- "C:/Users/carlos.quesada/Documents/features/lcl/"
    # feats_folder <- "C:/Users/carlos.quesada/Documents/features/goiener/"
    ftp          <- c(6:10, 61:84, 162:168, 209:220)
    centers      <- 6
    is.goiener   <- F
    
    # Load features
    feats <- data.table::fread(
      file   = paste(feats_folder, "feats.csv", sep = ""),
      header = TRUE,
      sep    = ","
    )
    
    otp <- 1:dim(feats)[1]
    if (is.goiener) {
      # # Folder of extended data
      # goi_folder <- "C:/issda-ext/" #"C:/goiener-ext/"
      # # Files in the folder
      # fnames <- list.files(goi_folder)
      # # List of valid filenames (cnae is ok)
      # ok_cnae_fnames <- c()
      # # Loop
      # for (fname in fnames) {
      #   load(paste(goi_folder, fname, sep=""))
      #   # Check CNAE code and fill list of valid filenames
      #   if (!is.na(
      #     edf$id
      #     #edf$cnae
      #     )) {
      # 
      #     # if (edf$cnae == 9810 | edf$cnae == 9811 |
      #     #     edf$cnae == 9820 | edf$cnae == 9821) {
      #     if (edf$id == 1) {
      #       ok_cnae_fnames <- c(ok_cnae_fnames, edf$filename)
      #     }
      #   }
      # }
      # browser()
      load("goi_ok_cnae_idx.Rdata")
      otp <- ok_cnae_idx
    }
    

    
    # Compute k-means
    km <- whyT2.1::pca_kmeans_analysis(
      feats_folder = feats_folder,
      ftp          = ftp,
      otp          = otp,
      min_var      = 0.90,
      centers      = centers
    )
    
    # Plot k-means
    whyT2.1::plot_kmeans(
      km            = km[["results"]],
      feats_df      = km[["feats"]],
      plot_clusters = TRUE
    )
    
    # Plotting constants
    cols1_block <- c(6,61, 162, 209)-1
    cols2_block <- c(10,84, 168, 220)-1
    x1_block <- c(1,0, 1, 1)
    x2_block <- c(5,23, 7, 12)
    
    # Analysis
    for (cc in 1:centers) {
      cluster_idx <- which(km$results$cluster == cc)
      feat_means  <- colMeans(feats[cluster_idx,-1])
      feat_vars   <- Rfast::colVars(as.matrix(feats[cluster_idx,-1]))
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
      print(feat_means)
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
    input_folder  <- "C:/issda/"
    output_folder <- "C:/issda/"
    metadata_file <- "G:/Mi unidad/WHY/Datos (raw)/ISSDA/38_CER Electricity_Gas/CER Electricity Revised March 2012/CER_Electricity_Documentation/allocations.csv"
    
    # Function call
    whyT2.1::extend_dataset(
      input_folder, 
      output_folder, 
      wanted_days = 800,
      dset_key = "iss",
      metadata_files = metadata_file
    )
    
    return(NULL)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 21 - Put feats together
  if (script_selection == 21) {
    library(tibble)
    base_folder <- c(
      "EMPTY",
      "C:/Users/carlos.quesada/Documents/features/issda_21.01.12/",
      "C:/Users/carlos.quesada/Documents/features/lcl_20.12.15/",
      "C:/Users/carlos.quesada/Documents/features/refit_21.01.09/"
    )
    goiener_folder <- c(
      "C:/Users/carlos.quesada/Documents/features/goiener_20.12.15/",
      "C:/Users/carlos.quesada/Documents/features/goiener_20.12.16/",
      "C:/Users/carlos.quesada/Documents/features/goiener_21.01.04/"
    )
    improved_folder <- c(
      "C:/Users/carlos.quesada/Documents/features/goiener_21.02.04/",
      "C:/Users/carlos.quesada/Documents/features/issda_21.02.04/",
      "C:/Users/carlos.quesada/Documents/features/lcl_21.02.04/",
      "C:/Users/carlos.quesada/Documents/features/refit_21.02.04/"
    )
    output_folder <- "C:/Users/carlos.quesada/Documents/features/"
    codes <- c("goi", "iss", "lcl", "ref")
    
    tbb_list <- list()
    
    for (ii in 1:4) {
      print(ii)
      base_tbb <- data.frame()
      # Goiener - there are 3 different folders to load!
      if (ii == 1) {
        for (jj in 1:3) {
          # Get list of dir names in dataset folder
          dir_names_b <- list.files(goiener_folder[jj], pattern = "*.csv")

          ### GET BASE DATAFRAME
          # Get feats from base_folder
          for (csv_file in dir_names_b) {
            # Load features
            feats <- data.table::fread(
              file   = paste(goiener_folder[jj], csv_file, sep = ""),
              header = TRUE,
              sep    = ","
            )
            # Merge
            base_tbb <- rbind(base_tbb, feats)
          }
        }
      # Rest of datasets
      } else {
        # Get list of dir names in dataset folder
        dir_names_b <- list.files(base_folder[ii], pattern = "*.csv")

        ### GET BASE DATAFRAME
        # Get feats from base_folder
        for (csv_file in dir_names_b) {
          # Load features
          feats <- data.table::fread(
            file   = paste(base_folder[ii], csv_file, sep = ""),
            header = TRUE,
            sep    = ","
          )
          # Merge
          base_tbb <- rbind(base_tbb, feats)
        }
      }
      # Remove feats
      rm(feats)
      # Convert to tibble
      base_tbb <- tibble(base_tbb)
      
      ### GET IMPROVED DATAFRAME
      impr_tbb <- data.frame()
      # Get list of dir names in dataset folder
      dir_names_i <- list.files(improved_folder[ii], pattern = "*.csv")
      # Get feats from improved_folder
      for (csv_file in dir_names_i) {
        # Load features
        feats <- data.table::fread(
          file   = paste(improved_folder[ii], csv_file, sep = ""),
          header = TRUE,
          sep    = ","
        )
        # Merge
        impr_tbb <- rbind(impr_tbb, feats)
      }
      # Remove feats
      rm(feats)
      # Convert to tibble
      impr_tbb <- tibble(impr_tbb)
      
      ### OPERATIONS WITH BOTH TIBBLES
      # Columns from base_tbb to be removed (from "mean" to "seas_acf1")
      idx_base <- 2:288
      # Columns from impr_tbb to be extracted (from "mean" to "ac_day_28")
      idx_impr <- 1:606
      
      # Remove columns in base tibble
      base_tbb <- dplyr::select(base_tbb, -all_of(idx_base))
      base_tbb <- dplyr::arrange(base_tbb, file)
      # Extract columns in improved tibble
      impr_tbb <- dplyr::select(impr_tbb, all_of(idx_impr))
      impr_tbb <- dplyr::arrange(impr_tbb, file)
      # Add columns

      if (all(base_tbb[,1] == impr_tbb[,1])) {
        base_tbb <- base_tbb %>% add_column(impr_tbb[,2:606], .after = 1)
      } else {
        print("NO COINCIDENCE!")
      }
      
      # Add a new "data_set" column after "file" column
      base_tbb <- tibble::add_column(
        base_tbb, 
        data_set = codes[ii], 
        .after   = "file"
      )
      # Remove ".RData" extension from "file" column
      base_tbb[,1] <- lapply(
        base_tbb[,1],
        gsub,
        pattern = ".RData",
        replacement = ""
      )
      
      tbb_list[[ii]] <- base_tbb
    }
  
    out <- data.frame()
    for (ii in 1:4) {
      out <- rbind(out, tbb_list[[ii]])
    }
    
    # Save
    data.table::fwrite(
      x = out,
      file = paste(output_folder, "feats.csv", sep=""),
      append    = F,
      quote     = F,
      sep       = ",",
      row.names = F,
      col.names = T
    )
    
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 22 - Add metadata [UNFINISHED - see script 23]
  if (script_selection == 22) {
    library(tibble)
    # Files
    file_feats <- "G:/.shortcut-targets-by-id/1g1D2rJfAektwZCB-O_F0EHxDYHxJmhmc/20WHY datasets/Features/feats_v1.0.csv"
    file_goi_1 <- "G:/.shortcut-targets-by-id/1g1D2rJfAektwZCB-O_F0EHxDYHxJmhmc/20WHY datasets/GOIENER/Contratos_Goiener_20201013_anonymized.csv"
    file_goi_2 <- "G:/.shortcut-targets-by-id/1g1D2rJfAektwZCB-O_F0EHxDYHxJmhmc/20WHY datasets/GOIENER/Contratos_Goiener_20201209_anonymized.csv"
    file_issda <- "G:/Mi unidad/WHY/Datos (raw)/ISSDA/38_CER Electricity_Gas/CER Electricity Revised March 2012/CER_Electricity_Documentation/allocations.csv"
    file_LoCLo <- "G:/Mi unidad/WHY/Datos (raw)/Low Carbon London/informations_households.csv"
    
    # Load feats
    feats <- data.table::fread(
      file   = file_feats,
      header = TRUE,
      sep    = ","
    )
    feats <- tibble(feats)
    # Load Goiener 1 metadata
    goi_1 <- data.table::fread(
      file   = file_goi_1,
      header = TRUE,
      sep    = ","
    )
    # Load Goiener 2 metadata
    goi_2 <- data.table::fread(
      file   = file_goi_2,
      header = TRUE,
      sep    = ","
    )
    # Goiener
    names(goi_1) <- names(goi_2)
    goien <- rbind(goi_1, goi_2)
    goien <- unique(goien)
    goien <- dplyr::arrange(goien, cups_ref)
    # Load ISSDA metadata
    issda <- data.table::fread(
      file   = file_issda,
      header = FALSE,
      sep    = ";"
    )
    # Load LCL metadata
    loclo <- data.table::fread(
      file   = file_LoCLo,
      header = TRUE,
      sep    = ","
    )

    # Relationship feats-goiener
    rel_feats_goien <- sapply(
      goien$cups_ref,
      function(i) which(i == feats$file)
    )
    # Relationship feats-ISSDA
    rel_feats_issda <- sapply(
      as.character(issda$V1),
      function(i) which(i == feats$file)
    )
    # Relationship feats-LCL
    rel_feats_loclo <- sapply(
      loclo$LCLid,
      function(i) which(i == feats$file)
    )
    
    # Add column to table
    feats <- feats %>% add_column(
      administrative_division = NA,
      .after = "data_set"
    )
    
    # Goiener provinces
    goien_prov <- sapply(
      names(rel_feats_goien[feats[[1]]]),
      function(i) {
        goien$cups.direccion_prov.nombre_oficial[which(goien$cups_ref == i)[1]]
      }
    )
    
    browser()
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 23 - ADD ALL METADATA OF VERSIONS 1.01 AND 1.02 [NO TARIFFS]
  if (script_selection == 23) {
    # File path to big CSV
    feats_path <- "G:/Mi unidad/WHY/Datasets/@FEATURES/feats_v1.0.csv"
    # Output path
    outpt_path <- "G:/Mi unidad/WHY/Datasets/@FEATURES/metadata.csv"
    # File path to Goiener 
    goien_path <- "G:/Mi unidad/WHY/Datasets/goiener-ext/"
    # File path to ISSDA 
    issda_path <- "G:/Mi unidad/WHY/Datasets/issda-ext/"
    # File path to Low Carbon London 
    loclo_path <- "G:/Mi unidad/WHY/Datasets/lcl-ext/"
    # File path to REFIT 
    refit_path <- "G:/Mi unidad/WHY/Datasets/refit-ext/"
    
    # Load feats
    feats <- data.table::fread(
      file   = feats_path,
      header = TRUE,
      sep    = ","
    )
    
    # FUNCTION TO LOAD METADATA FROM RDATA FILES
    load_metadata <- function(input_df) {
      ## INPUTS
      file_name <- input_df[[1]]
      data_set  <- input_df[[2]]
      ## OUTPUT
      o <- list()
      
      ## LOAD FILE
      # Goiener data set
      if (data_set == "goi") {
        load(paste(goien_path, file_name, ".RData", sep=""))
      }
      # ISSDA data set
      if (data_set == "iss") {
        load(paste(issda_path, file_name, ".RData", sep=""))
      }
      # Low Carbon London data set
      if (data_set == "lcl") {
        load(paste(loclo_path, file_name, ".RData", sep=""))
      }
      # REFIT data set
      if (data_set == "ref") {
        load(paste(refit_path, file_name, ".RData", sep=""))
      }
      
      ## COMMON METADATA
      # file
      o$file <- file_name
      # overall_start_date
      o$overall_start_date <- edf$df$times[1]
      # overall_end_date
      o$overall_end_date <- dplyr::last(edf$df$times)
      # overall_days
      o$overall_days <- as.numeric(
        difftime(
          o$overall_end_date,
          o$overall_start_date,
          units = "days")
      )
      ## Get vector of imputed dates
      imputed_dates <- which(edf$df$imputed == 2)
      # imputed_start_date
      o$imputed_start_date <- edf$df$times[imputed_dates[1]]
      # imputed_end_date
      o$imputed_end_date <- edf$df$times[dplyr::last(imputed_dates)]
      # imputed_days
      o$imputed_days <- as.numeric(
        difftime(
          o$imputed_end_date,
          o$imputed_start_date,
          units = "days")
      )
      if (is.na(o$imputed_days)) o$imputed_days <- 0
      # imputed_days_pct
      o$imputed_days_pct <- o$imputed_days / o$overall_days
      # imputed_na
      o$imputed_na <- edf$number_of_na
      # imputed_na_pct
      o$imputed_na_pct <- edf$number_of_na / length(edf$df$times)
      # total_imputed_pct
      o$total_imputed_pct <- o$imputed_days_pct + o$imputed_na_pct

      ## GOIENER-SPECIFIC METADATA
      if (data_set == "goi") {
        # country
        o$country <- "es"
        # administrative_division
        o$administrative_division <-
          stringr::str_replace(edf$province, ",", ";")
        # municipality
        o$municipality <-
          stringr::str_replace(edf$municipality, ",", ";")
        # zip_code
        o$zip_code <- edf$zip_code
        ## Spatial resolution
        if (is.na(edf$cnae)) {
          o$is_household <- NA
        } else {
          if (floor(edf$cnae/100) == 98) {
            o$is_household <- 1
          } else {
            o$is_household <- 0
          }
        }
        # cnae
        o$cnae <- edf$cnae
        # acorn
        o$acorn <- NA
        # acorn_grouped
        o$acorn_grouped <- NA
      }
      
      ## ISSDA-SPECIFIC METADATA
      if (data_set == "iss") {
        # country
        o$country <- "ie"
        # administrative_division
        o$administrative_division <- NA
        # municipality
        o$municipality <- NA
        # zip_code
        o$zip_code <- NA
        ## Spatial resolution
        if (edf$id == 1) {
          o$is_household <- 1
        } else {
          o$is_household <- 0
        }
        # cnae
        o$cnae <- NA
        # acorn
        o$acorn <- NA
        # acorn_grouped
        o$acorn_grouped <- NA
      }
      
      ## LOW CARBON LONDON-SPECIFIC METADATA
      if (data_set == "lcl") {
        # country
        o$country <- "gb"
        # administrative_division
        o$administrative_division <- "Greater London"
        # municipality
        o$municipality <- NA
        # zip_code
        o$zip_code <- NA
        ## Spatial resolution
        o$is_household <- 1
        # cnae
        o$cnae <- NA
        # acorn
        o$acorn <- edf$acorn
        # acorn_grouped
        o$acorn_grouped <- edf$acorn_grouped
      }
      
      ## REFIT-SPECIFIC METADATA
      if (data_set == "ref") {
        # country
        o$country <- "gb"
        # administrative_division
        o$administrative_division <- NA
        # municipality
        o$municipality <- "Loughborough"
        # zip_code
        o$zip_code <- NA
        ## Spatial resolution
        o$is_household <- 1
        # cnae
        o$cnae <- NA
        # acorn
        o$acorn <- NA
        # acorn_grouped
        o$acorn_grouped <- NA
      }
      
      return(o)
    }
    
    # Generate table
    feats <- feats[1:14,]
    library(pbapply)    
    x <- do.call(dplyr::bind_rows, pbapply(feats[,1:2], 1, load_metadata))
    # Save
    data.table::fwrite(
      x         = x,
      file      = outpt_path,
      append    = F,
      quote     = F,
      sep       = ",",
      row.names = F,
      col.names = T,
      dateTimeAs = "write.csv"
    )
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 24 - ADD GOIENER TARIFFS
  if (script_selection == 24) {
    library(lubridate)
    # File path to big CSV
    feats_path <- "G:/Mi unidad/WHY/Datasets/@FEATURES/feats_v1.02.csv"
    # GOIENER data
    goien_fold <- "G:/Mi unidad/WHY/Datos (raw)/GOIENER/"
    # GOIENER extended data
    goien_ext_fold <- "G:/Mi unidad/WHY/Datasets/goiener-ext/"
    # GOIENER file 1
    goi_1_file <- "Contratos_Goiener_20201013_anonymized.csv"
    # GOIENER file 2
    goi_2_file <- "Contratos_Goiener_20201209_anonymized.csv"
    # GOIENER file paths
    goi_1_path <- paste(goien_fold, goi_1_file, sep="")
    goi_2_path <- paste(goien_fold, goi_2_file, sep="")
    # Output folder
    output_folder <- goien_fold
    browser()
    
    # Load feats
    feats <- data.table::fread(
      file   = feats_path,
      header = TRUE,
      sep    = ","
    )
    # List of GOIENER filenames
    goi_cups <- feats[feats$data_set == "goi",]$file
    
    # Load Goiener 1 metadata
    goi_1 <- data.table::fread(
      file   = goi_1_path,
      header = TRUE,
      sep    = ","
    )
    # Load Goiener 2 metadata
    goi_2 <- data.table::fread(
      file   = goi_2_path,
      header = TRUE,
      sep    = ","
    )
    # Goiener
    names(goi_1) <- names(goi_2)
    goien <- rbind(goi_1, goi_2)
    goien <- unique(goien)
    goien <- dplyr::arrange(
      goien,
      cups_ref,
      desc(fecha_alta),
      desc(fecha_baja)
    )
    
    # Compute data
    o <- data.frame()
    for (gg in goi_cups) {
      # Indices of CUPS 
      idx <- which(goien$cups_ref == gg)
      # If CUPS has metadata
      if (length(idx) != 0) {
        print(gg)
        # Tariff type
        tariff <- goien$tarifa.tarifa_atr_ref[min(idx)]
        # Load extended time series file
        load(paste(goien_ext_fold, gg, ".RData", sep = ""))
        # Get start period
        # as.POSIXct(feats[feats$file == gg,]$overall_end_date, tz = "GMT")
        end_period <- edf$df$times[length(edf$df$times)]
        start_period <- end_period %m-% years(2)
        # Get just two years of the dataframe
        t <- edf$df[edf$df$times > start_period,1:2]
        t[,1] <- as.POSIXct(t[,1], tz="GMT")
        # Get the bins of the hours
        h_factor <- as.factor(lubridate::hour(t[,1]))
        # Aggregate data (sum) according to the bins
        aggr <- stats::aggregate(
          x   = t[,2],
          by  = list(bin = h_factor),
          FUN = sum
        )
        ### OLD TARIFFS ########################################################
        kwh_2y_total <- sum(aggr$x)
        # 00:00-23:59 -> PEAK
        if (tariff == "2.0A" | tariff == "2.1A") {
          kwh_2y_peak_old   <- sum(aggr$x)
          kwh_2y_flat_old   <- NA
          kwh_2y_valley_old <- NA
          pct_2y_peak_old   <- 1.0 
          pct_2y_flat_old   <- NA
          pct_2y_valley_old <- NA
        }
        # 12:00-21:59 -> PEAK
        # 22:00-11:59 -> VALLEY
        if (tariff == "2.0DHA" | tariff == "2.1DHA") {
          kwh_2y_peak_old   <- sum(aggr$x[13:22])
          kwh_2y_flat_old   <- NA
          kwh_2y_valley_old <- sum(aggr$x[23:24]) + sum(aggr$x[1:12])
          pct_2y_peak_old   <- kwh_2y_peak_old / kwh_2y_total
          pct_2y_flat_old   <- NA
          pct_2y_valley_old <- kwh_2y_valley_old / kwh_2y_total
        }
        # 13:00-22:59 -> PEAK
        # 23:00-00:59 & 07:00-12:59 -> FLAT
        # 01:00-06:59 -> VALLEY
        if (tariff == "2.0DHS" | tariff == "2.1DHS") {
          kwh_2y_peak_old   <- sum(aggr$x[14:23])
          kwh_2y_flat_old   <- aggr$x[24] + aggr$x[1] + sum(aggr$x[8:13])
          kwh_2y_valley_old <- sum(aggr$x[2:7])
          pct_2y_peak_old   <- kwh_2y_peak_old / kwh_2y_total
          pct_2y_flat_old   <- kwh_2y_flat_old / kwh_2y_total
          pct_2y_valley_old <- kwh_2y_valley_old / kwh_2y_total
        }
        ### NEW TARIFFS (2.0TD) ################################################
        # Aggregate by workday/weekend
        d_f <- cut(t[,1], breaks = "1 day")
        # Convert to vector of dates
        d_v <- as.POSIXct(as.vector(d_f), tz="GMT")
        # Numbers indicating 0-4 weekdays, 5-6 weekends
        d_factor <- (wday(d_v) - 2) %% 7
        # Get TD bins
        td_factor <- as.numeric(h_factor)
        td_factor[d_factor == 5 | d_factor == 6] <- 0
        td_factor <- as.factor(td_factor)
        # 10:00-13:59 & 18:00-21:59 -> PEAK
        # 08:00-09:59 & 14:00-17:59 & 22:00-23:59 -> FLAT
        # 00:00-07:59 & WEEKENDS -> VALLEY
        kwh_2y_peak_new   <- sum(aggr$x[11:14]) + sum(aggr$x[19:22])
        kwh_2y_flat_new   <- sum(aggr$x[9:10]) + sum(aggr$x[15:18]) +
                             sum(aggr$x[23:24])
        kwh_2y_valley_new <- sum(aggr$x[0:8])
        pct_2y_peak_new   <- kwh_2y_peak_new / kwh_2y_total
        pct_2y_flat_new   <- kwh_2y_flat_new / kwh_2y_total
        pct_2y_valley_new <- kwh_2y_valley_new / kwh_2y_total
        
        ### Put all data together into a dataframe #############################
        df <- data.frame(
          cups              = gg, 
          tariff            = tariff,
          kwh_2y_peak_old   = kwh_2y_peak_old,
          kwh_2y_flat_old   = kwh_2y_flat_old,
          kwh_2y_valley_old = kwh_2y_valley_old,
          pct_2y_peak_old   = pct_2y_peak_old,
          pct_2y_flat_old   = pct_2y_flat_old,
          pct_2y_valley_old = pct_2y_valley_old,
          kwh_2y_peak_new   = kwh_2y_peak_new,
          kwh_2y_flat_new   = kwh_2y_flat_new,
          kwh_2y_valley_new = kwh_2y_valley_new,
          pct_2y_peak_new   = pct_2y_peak_new,
          pct_2y_flat_new   = pct_2y_flat_new,
          pct_2y_valley_new = pct_2y_valley_new
        )
      # If CUPS does NOT have metadata
      } else {
        df <- data.frame(
          cups              = gg,   tariff            = NA,
          kwh_2y_peak_old   = NA,   kwh_2y_flat_old   = NA,
          kwh_2y_valley_old = NA,   pct_2y_peak_old   = NA,
          pct_2y_flat_old   = NA,   pct_2y_valley_old = NA,
          kwh_2y_peak_new   = NA,   kwh_2y_flat_new   = NA,
          kwh_2y_valley_new = NA,   pct_2y_peak_new   = NA,
          pct_2y_flat_new   = NA,   pct_2y_valley_new = NA
        )
      }
      # Add to final dataframe
      o <- rbind(o, df)
    }

    ### ADD SHORT DATAFRAME TO BIG FEATURES DATAFRAME
    # Save o
    data.table::fwrite(
      x         = o,
      file      = paste(output_folder, "goi_tariffs.csv", sep=""),
      append    = F,
      quote     = F,
      sep       = ",",
      row.names = F,
      col.names = T,
      dateTimeAs = "write.csv"
    )
    # Get dimensions of both dataframes
    dim_feats <- dim(feats)
    dim_o <- dim(o)
    # Check that CUPS are properly sorted
    if (all(feats$file[1:dim_o[1]] == o$file)) {
      # Pad short dataframe with NAs
      pad_o <- data.frame(
        matrix(
          NA,
          nrow = dim_feats[1] - dim_o[1],
          ncol = dim_o[2]
        )
      )
      names(pad_o) <- names(o)
      pad_o <- rbind(o, pad_o)
      # Incorporate columns
      feats <- feats %>% add_column(pad_o[,2:dim_o[2]], .before = "file")
      browser()
    } else {
      print("ERROR")
    }
    # Save feats
    data.table::fwrite(
      x         = o,
      file      = paste(output_folder, "new_feats.csv", sep=""),
      append    = F,
      quote     = F,
      sep       = ",",
      row.names = F,
      col.names = T,
      dateTimeAs = "write.csv"
    )
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 25 - SAVE feats.csv WITH goi_tariffs.csv DATA
  if (script_selection == 25) {
    
    # Load feats
    feats <- data.table::fread(
      file   = "G:/Mi unidad/WHY/Datasets/@FEATURES/feats_v1.02.csv",
      header = TRUE,
      sep    = ","
    )
    
    # Load o
    o <- data.table::fread(
      file   = "G:/Mi unidad/WHY/Datasets/@FEATURES/goi_tariffs.csv",
      header = TRUE,
      sep    = ","
    )
    
    # Get dimensions of both dataframes
    dim_feats <- dim(feats)
    dim_o <- dim(o)
    # Check that CUPS are properly sorted
    if (all(feats$file[1:dim_o[1]] == o$file)) {
      # Pad short dataframe with NAs
      pad_o <- data.frame(
        matrix(
          NA,
          nrow = dim_feats[1] - dim_o[1],
          ncol = dim_o[2]
        )
      )
      names(pad_o) <- names(o)
      pad_o <- rbind(o, pad_o)
      # Incorporate columns
      feats <- feats %>% add_column(pad_o[,2:dim_o[2]], .before = "file")
      browser()
    } else {
      print("ERROR")
    }
    # Save feats
    data.table::fwrite(
      x         = feats,
      file      = "G:/Mi unidad/WHY/Datasets/@FEATURES/feats_v1.03.csv",
      append    = F,
      quote     = F,
      sep       = ",",
      row.names = F,
      col.names = T,
      dateTimeAs = "write.csv"
    )

    return()
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 26
  if (script_selection == 26) {
    
    return()
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 27
  if (script_selection == 27) {
    
    return()
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 28
  if (script_selection == 28) {
    
    return()
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 29
  if (script_selection == 29) {
    
    return()
  }
  
  # ----------------------------------------------------------------------------
  
}

# Execute selected script

result <- scripts(script_selection)
