library(tsfeatures)
library(foreach)

# Input parameters
input_folder <- c("D:/Quesada/Documents/__TRABAJO/why/GO4/")

# Output parameters
output_path <- c("C:/GO4/out.csv")

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

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

o <- foreach::foreach(
  x              = 1:length(fpaths),
  .combine       = rbind,
  .inorder       = TRUE,
  # .errorhandling = "stop"
  .packages      = c(
    "tsfeatures", "moments", "forecast", "stats", "lubridate", "stringr",
    "utils"
  )
) %dopar% {

# for(x in 1:length(fpaths)) {
  
  # SOURCE FILE OF FUNCTIONS
  source("D:\\Quesada\\Documents\\__ACTIVIDADES\\GitHub\\why-T2.1\\_datasets\\goi\\new_feats_src.R")
  
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
    # Filename and dataset key
    ff_file <- data.frame(file = fname, data_set = edf$dset_key)
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
  file      = output_path,
  sep       = ",",
  na        = "",
  quote     = FALSE,
  append    = FALSE,
  col.names = TRUE,
  row.names = FALSE
)

