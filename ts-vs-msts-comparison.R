# Carlos Quesada - Universidad de Deusto
# 2020.07.11
# This file is just to compare if "ts" behaves the same way as "msts" with one 
# seasonal period equal to the frequency of "ts"

# Load source file and libraries
source("why-source.R")

dset_key <- "lcl"

# Initialize outputs
op_feats_ts <- NULL
op_feats_msts <- NULL
op_dset_info <- NULL

# Get list of files in folder 
dset_filenames <- Get_File_List(dset_key)

# Analysis loop
for (dset_filename in dset_filenames[1:6]) {
  # Load dataset file
  print(dset_filename)
  dset_data <- Load_Dataset_File(dset_key, dset_filename)
  
  # Set values
  ts_freq <- 48
  date_1st <- dset_data[[1,1]]
  date_last <- tail(dset_data, 1)[[1,1]]
  
  # Get values from dataset file
  dset_data_intvl <- Get_Data_Interval(
    tm_series = dset_data,
    from_date = date_1st,
    to_date = date_last,
    step = 30 * 60
  )
  
  # Get seasonal periods
  seas_periods <- NULL
  len_data_intvl <- length(dset_data_intvl)
  if (len_data_intvl > 2*ts_freq) {
    seas_periods <- c(seas_periods, ts_freq)
  }
  if (len_data_intvl > 2*7*ts_freq) {
    seas_periods <- c(seas_periods, 7*ts_freq)
  }
  if (len_data_intvl > 2*365*ts_freq) {
    seas_periods <- c(seas_periods, 365*ts_freq)
  }
  
  # Not analyze features if TS too short
  if (!is.null(seas_periods)) {
    # Convert to time series
    vals_ts <- ts(
      data = dset_data_intvl,
      start = c(1, 1),
      frequency = ts_freq
    )
    vals_msts <- msts(
      data = dset_data_intvl,
      start = c(1, 1),
      ts.frequency = ts_freq,
      seasonal.periods = ts_freq
    )
    
    # Analyze time series
    feats_ts <- tsfeatures(
      list(vals_ts),
      features = glb_all_analysis_list,
      na.action = na.interp
    )
    feats_msts <- tsfeatures(
      list(vals_msts),
      features = glb_all_analysis_list,
      na.action = na.interp
    )
  
    # Create tibble of dataset info
    dset_info <- tibble(
      filename = dset_filename,
      from_date = format(date_1st, "%Y-%m-%d %H:%M:%S"),
      to_date = format(date_last, "%Y-%m-%d %H:%M:%S"),
      total_samples = len_data_intvl,
      total_NAs = sum(is.na(dset_data_intvl))
    )
    
    # Bind tibble rows
    op_feats_ts <- bind_rows(op_feats_ts, feats_ts)
    op_feats_msts <- bind_rows(op_feats_msts, feats_msts)
    op_dset_info <- bind_rows(op_dset_info, dset_info)
  }
}

# Save tibbles as CSV
write.table(
  op_feats_ts,
  file = "feats-ts.csv",
  row.names = FALSE,
  sep = ",",
  na = "",
  quote = FALSE
)
write.table(
  op_feats_msts,
  file = "feats-msts.csv",
  row.names = FALSE,
  sep = ",",
  na = "",
  quote = FALSE
) 
write.table(
  op_dset_info,
  file = "data_info.csv",
  row.names = FALSE,
  sep = ",",
  na = "",
  quote = FALSE
) 
