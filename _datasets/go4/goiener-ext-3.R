library(foreach)
library(tidyr)
library(whyT2.1)

#Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")

folder <- "D:\\Quesada\\Documents\\__ACTIVIDADES\\GitHub\\why-T2.1\\R\\"
# source(paste0(folder, "dataset-load-and-processing.R"))
# source(paste0(folder, "dataset-metadata.R"))

extend_dataset_v2 <- function(input_folder, output_folder, wanted_days, dset_key, 
                           metadata_files=NULL, from_date="first", to_date="last", 
                           extend_after_end=TRUE, working_with_generation=FALSE,
						   min_years = 1) {
  
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(input_folder)
  # Extract relevant data from metadata files (if any!)
  if (!is.null(metadata_files)) {
    # Load metadata dataframes into a big list
    metadata_dataframes <- lapply(
      metadata_files,
      data.table::fread,
      header     = TRUE,
      sep        = ",",
      na.strings = "",
      encoding   = "UTF-8"
    )
  }
  
  # Setup parallel backend to use many processors
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)
  
  # Progress bar
  pb <- txtProgressBar(style=3)
  # fnames length
  length_fnames <- length(dset_filenames)
  #print(length_fnames)
  
  # Analysis loop
  pkg <- c("tidyr")
  #out <- foreach::foreach (x = 1:length_fnames, .packages = pkg) %dopar% {
  for(x in 1:length_fnames) {    
    # Set progress bar
    setTxtProgressBar(pb, x/length_fnames)
    
    # File name selection
    dset_filename <- dset_filenames[x]
    # Extract metadata
    if (!is.null(metadata_files)) {
      metadata_list <- whyT2.1::extract_metadata(
        dfs      = metadata_dataframes,
        dset_key = dset_key,
        filename = dset_filename
      )
    }
    # Load raw dataframe from dataset and impute
    file_path <- paste0(input_folder, dset_filename)
    rdf <- whyT2.1::get_raw_dataframe_from_dataset(file_path)
    # GOIENER DATASETS ONLY
    if (!working_with_generation) {
      rdf <- rdf[,1:2]
    } else {
      rdf <- rdf[,c(1,3)]
      names(rdf) <- c("times", "values")
    }
    cdf <- whyT2.1::cook_raw_dataframe(
      raw_df    = rdf,
      from_date = from_date, 
      to_date   = to_date, 
      dset_key  = dset_key, 
      filename  = dset_filename, 
      metadata  = metadata_list
    )
    # If cdf is NULL, skip
    if (!is.null(cdf)) {
      # Get length
      initial_date   <- cdf$df[1,1]
      final_date     <- cdf$df[nrow(cdf$df),1]
      # length_in_days <- as.numeric(final_date - initial_date)
      length_in_years <- 
        lubridate::interval(initial_date,final_date)/lubridate::years(1)
      # If TS is longer than min_years, impute; ELSE discard
      if (length_in_years >= min_years) {
        edf <- whyT2.1::impute_cooked_dataframe(
          cdf       = cdf, 
          season    = cdf$seasonal_periods[1] * 7, 
          short_gap = cdf$seasonal_periods[1] / 3
        )
        if (!is.null(edf)) {
          # # Expand if needed
          # edf <- extend_imputed_dataframe(
          #   idf              = idf,
          #   wanted_days      = wanted_days,
          #   extend_after_end = extend_after_end
          # )
          # if (!is.null(edf)) {
            # Save dataframe in output folder
            path <- paste0(
              output_folder, strsplit(dset_filename, ".csv")[[1]], ".RData"
            )
            save(edf, file=path)
          # }
        }
      }
    }
  }
  # Stop parallelization
  parallel::stopCluster(cl)
  
  cat("\n")
}

### USER DEFINED VARIABLES
#if (.Platform$OS.type == "windows") {
#  raw_folder <- "D:/Quesada/Documents/__TRABAJO/why/raw/"
#  ext_folder <- "D:/Quesada/Documents/__TRABAJO/why/ext/"
#  mdata_file <- "D:/Quesada/Documents/__TRABAJO/why/Contratos_Goiener_20211103_anonymized.csv"
#}
#if (.Platform$OS.type == "unix") {
#  raw_folder <- "/home/ubuntu/carlos.quesada/disk/go3/raw/"
#  ext_folder <- "/home/ubuntu/carlos.quesada/disk/go3_pre/ext/"
#  mdata_file <- "/home/ubuntu/carlos.quesada/R_scripts/Contratos_Goiener_20211103_anonymized.csv"
#}

# # Function call for "go4_pre" (consumption)
# extend_dataset_v2(
  # "/home/ubuntu/carlos.quesada/disk/go4/raw/", 
  # "/home/ubuntu/carlos.quesada/disk/go4_pre/imp/", 
  # wanted_days = NULL,
  # dset_key = "goi",
  # metadata_files = "/home/ubuntu/carlos.quesada/R_scripts/Contratos_Goiener_20211103_anonymized.csv",
  # to_date = as.POSIXct("2020-02-29 23:00:00", tz="UTC"),
  # extend_after_end = NULL,
  # working_with_generation = FALSE,
  # min_years = 1
# )

# Function call for "go4_pst" (consumption)
extend_dataset_v2(
 "/home/ubuntu/carlos.quesada/disk/go4/raw/", 
 "/home/ubuntu/carlos.quesada/disk/go4_pst/imp/", 
 wanted_days = NULL,
 dset_key = "goi",
 metadata_files = "/home/ubuntu/carlos.quesada/R_scripts/Contratos_Goiener_20211103_anonymized.csv",
 from_date = as.POSIXct("2020-03-01 00:00:00", tz="UTC"),
 extend_after_end = NULL,
 working_with_generation = FALSE,
 min_years = 1
)

# Function call for "go3_p20" (consumption)
#extend_dataset_v2(
#  "/home/ubuntu/carlos.quesada/disk/go3/raw/", 
#  "/home/ubuntu/carlos.quesada/disk/go3_p20/imp/", 
#  wanted_days = NULL,
#  dset_key = "go3_p20",
#  metadata_files = "/home/ubuntu/carlos.quesada/R_scripts/Contratos_Goiener_20211103_anonymized.csv",
#  from_date = as.POSIXct("2020-06-01 00:00:00", tz="UTC"),
#  to_date = as.POSIXct("2020-12-31 23:00:00", tz="UTC"),
#  extend_after_end = NULL,
#  working_with_generation = FALSE,
#  min_years = lubridate::interval(
#	as.POSIXct("2020-06-01 00:00:00", tz="UTC"),
#	as.POSIXct("2020-12-31 23:00:00", tz="UTC"))/lubridate::years(1)
#)