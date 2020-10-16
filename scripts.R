#' This is a compilation of simple scripts to execute functions of the `whyT2.1` library. 
#' 
#' KEY OF ABBREVIATIONS:
#' DSET = dataset | EXT = extended | FEAT = feature | RAW = raw | TS = time series
#' 
#' LIST OF SCRIPTS:
#' 
#' ** DATASET MANAGEMENT **
#' `1` Create EXT DSETs from folder of RAW DSETs
#' 
#' ** FEATURE EXTRACTION **
#' `4` Get FEATs of (1-month LCL RAW) DSETs from folder 
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
  }

  # ----------------------------------------------------------------------------
  
  # SCRIPT 5
  if (script_selection == 5) {
    # User parameters
    folder_path   <- "G:/Mi unidad/WHY/Datasets/lcl-ext/"
    four_digit_id <- "0002"
    
    # Function call
    path <- paste(folder_path, "MAC00", four_digit_id, sep="")
    load(path)
    whyT2.1::plot_dataframe(dset_data = edf$df, title = four_digit_id)
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 6
  if (script_selection == 6) {
    
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 7
  if (script_selection == 7) {
    
  }
  
  # ----------------------------------------------------------------------------
  
  # SCRIPT 8
  if (script_selection == 8) {
    
  }
  
  # ----------------------------------------------------------------------------
}

# Execute selected script
scripts(script_selection)
