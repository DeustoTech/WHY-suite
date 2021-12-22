# INPUT FOLDER
# ifold <- "C:/Users/carlos.quesada/Documents/GO4/"
ifold <- "/mnt/disk2/go4_pst/imp/"

# OUTPUT FILE
# ofold <- "C:/Users/carlos.quesada/Documents/GO4_out/"
ofold <- "/mnt/disk2/features/go4_pst_21.12.12/"

# SOURCE FILE
# sfile <- "C:\\Users\\carlos.quesada\\Documents\\GitHub\\why-T2.1\\_datasets\\go4\\new_feats_src.R"
sfile <- "/home/ubuntu/carlos.quesada/R_scripts/new_feats_src.R"

library(tsfeatures)
library(foreach)

# Input parameters
input_folder <- c(ifold)

# Output parameters
output_path <- c(ofold)

# Type of analysis
type_of_analysis <- "extra"

# List of file paths
fpaths <- c()
for (ii in 1:length(input_folder)) {
  # Get list of file paths
  fpaths <- c(
    fpaths,
    list.files(input_folder[ii], pattern="*.RData", full.names = T)
  )
}

# SEQUENCING
length_fpaths <- length(fpaths)
SS_seq <- c(seq(0, length_fpaths, 6000), length_fpaths)
for (SS in 1:(length(SS_seq)-1)) {
  
  # Setup parallel backend to use many processors
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)
  
  # Progress bar
  pb <- txtProgressBar(style=3)
  
  o <- foreach::foreach(
    x              = (SS_seq[SS]+1):SS_seq[SS+1],
    .combine       = rbind,
    .inorder       = TRUE,
    # .errorhandling = "stop"
    .packages      = c(
      "tsfeatures", "moments", "forecast", "stats", "lubridate", "stringr",
      "utils"
    )
  ) %dopar% {
    
    # for(x in 1:length(fpaths)) {
    
    # Set progress bar
    setTxtProgressBar(pb, x/length_fpaths)
    
    # SOURCE FILE OF FUNCTIONS
    source(sfile)
    
    # Select file name
    fpath <- fpaths[x]
    fname <- strsplit(basename(fpath), split=".RData")[[1]]
    # print(fname)
    # Load extended dataframe
    load(fpath)
    # Set exceptions
    if (!edf$is_0) {
      # REMOVE 3RD SEASONAL PERIOD
      edf$seasonal_periods <- edf$seasonal_periods[1:2]
      # METADATA
      ff_file <- data.frame(
        file = fname, 
        data_set = edf$dset_key,
        # EXCLUSIVE GOIENER
        num_of_samples = length(edf$df$values),
        mdata_file_idx = edf$mdata_file_idx,
        ts_start_date = edf$df$times[1],
        ts_end_date = edf$df$times[nrow(edf$df)],
        ts_days = as.numeric(edf$df$times[nrow(edf$df)] - edf$df$times[1]),
        contract_start_date = edf$start_date,
        contract_end_date = edf$end_date,
        abs_imputed_na = edf$number_of_na,
        rel_imputed_na = edf$number_of_na / length(edf$df$values),
        country = "es",
        administrative_division = edf$province,
        municipality = edf$municipality,
        zip_code = edf$zip_code,
        cnae = edf$cnae,
        is_household = ifelse(edf$cnae %/% 100 == 98, 1, 0),
        ref_tariff = edf$ref_tariff,
        ref_atr_tariff = edf$ref_atr_tariff,
        ref_atr_proc = edf$ref_atr_proc,
        billing_type = edf$billing_type,
        indexed_margin = edf$indexed_margin,
        self_consump = edf$self_consump,
        p1_kw = edf$p1_kw,
        p2_kw = edf$p2_kw,
        p3_kw = edf$p3_kw,
        p4_kw = edf$p4_kw,
        p5_kw = edf$p5_kw,
        p6_kw = edf$p6_kw
      )
      # GET FEATURES
      ff_feats <- get_features_from_cooked_dataframe(
        cdf              = edf,
        type_of_analysis = type_of_analysis
      )
      # Incorporate filename as a column
      o <- cbind(ff_file, ff_feats)
    } else {
      NULL
    }
  }
  
  # Stop parallelization
  parallel::stopCluster(cl)
  
  
  # Save results to the CSV file
  data.table::fwrite(
    x         = o,
    file      = paste0(output_path, "o_", SS_seq[SS+1], ".csv"),
    sep       = ",",
    na        = "",
    quote     = FALSE,
    append    = FALSE,
    col.names = TRUE,
    row.names = FALSE
  )
  
}