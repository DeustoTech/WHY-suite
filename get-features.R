# Carlos Quesada - Universidad de Deusto
# 2020.07.11
# Compute features for a particular dataset
# Analysis for just February 2013

# Load source file and libraries
source("why-source.R")

# User parameters
dset_key <- "lcl"
# Date can be an ISOdate or either "first" to indicate the first timestamp in
# the dataset or "last" to indicate the last one
from_date <- ISOdate(2013, 2, 1, 0, 0, 0)  #"first"
to_date <- ISOdate(2013, 2, 28, 23, 30, 0) #"last"
# Accepted percentage of NaN values between the start and end dates provided.
# If the time series exceeds that percentage, it will be discarded
accepted_na_rate <- 0.0

# Initialize outputs
op_feats <- NULL
op_dset_info <- NULL
op_rejected <- NULL

# Get list of filenames in dataset folder 
dset_filenames <- list.files(DATASET_PATH)

# Randomize list of filenames
set.seed(2411)

# Analysis loop
for (dset_filename in dset_filenames[1:5]) {
  # Load dataset file
  print(dset_filename)
  dset_data <- Load_Dataset_File(DATASET_PATH, dset_filename)
  
  # Set values
  ts_freq <- SAMPLES_PER_DAY[[dset_key]]
  
  # Check interval left end
  if (any(class(from_date) == "character")) {
    if (from_date == "first") {
      from_date <- dset_data[[1, 1]]
    }
  }
  # Check interval right end
  if (any(class(to_date) == "character")) {
    if (to_date == "last") {
      to_date <- tail(dset_data, 1)[[1, 1]]
    }
  }
  
  # Get values from dataset file
  dset_data_intvl <- Get_Data_Interval(
    tm_series = dset_data,
    from_date = from_date,
    to_date = to_date,
    step = 86400 / samples_per_day[[dset_key]] # 30 * 60
  )
  
  # Check correctness of "dset_data_intvl"
  seas_periods <- NULL
  ts_is_0 <- FALSE
  if (!is.null(dset_data_intvl)) {
    # Get seasonal periods
    len_data_intvl <- length(dset_data_intvl)
    if (len_data_intvl > 2 * ts_freq) {
      seas_periods <- c(seas_periods, ts_freq)
    }
    if (len_data_intvl > 2 * 7 * ts_freq) {
      seas_periods <- c(seas_periods, 7 * ts_freq)
    }
    if (len_data_intvl > 2 * 365 * ts_freq) {
      seas_periods <- c(seas_periods, 365 * ts_freq)
    }
    # Check if all values are 0 (it happens with LCL4067)
    ts_is_0 <- all(dset_data_intvl[!is.na(dset_data_intvl)] == 0.0)
    # Check NAs
    many_na <- sum(is.na(dset_data_intvl))/len_data_intvl > accepted_na_rate
  }
  
  # Not analyze features
  if (is.null(dset_data_intvl) | is.null(seas_periods) | ts_is_0 | many_na) {
    # Select rejection reason
    rejected <- tibble(
      filename = dset_filename,
      null_dataset = is.null(dset_data_intvl),
      null_seas_periods = is.null(seas_periods),
      ts_is_0 = ts_is_0,
      many_na = many_na
    )
    # Bind tibble rows
    op_rejected <- bind_rows(op_rejected, rejected)
  # Do analyze features
  } else {
    # Convert to time series
    vals_msts <- msts(
      data = dset_data_intvl,
      start = c(1, 1),
      ts.frequency = ts_freq,
      seasonal.periods = seas_periods
    )
    
    # Analyze time series
    feats <- tsfeatures(
      list(vals_msts),
      features = BASIC_ANALYSIS_LIST,
      scale = FALSE,
      na.action = na.interp
    )
    print(feats)
  
    # Create tibble of dataset info
    dset_info <- tibble(
      filename = dset_filename,
      from_date = format(from_date, "%Y-%m-%d %H:%M:%S"),
      to_date = format(to_date, "%Y-%m-%d %H:%M:%S"),
      total_samples = len_data_intvl,
      total_NAs = sum(is.na(dset_data_intvl))
    )
    
    # Bind tibble rows
    op_feats <- bind_rows(op_feats, feats)
    op_dset_info <- bind_rows(op_dset_info, dset_info)
  }
}

# Save tibbles as CSV
write.table(
  op_feats,
  file = "feats.csv",
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
write.table(
  op_rejected,
  file = "rejected.csv",
  row.names = FALSE,
  sep = ",",
  na = "",
  quote = FALSE
) 
