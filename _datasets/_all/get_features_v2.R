library(whyT2.1)
library(foreach)

# Input parameters
input_folder <- c(
  "/home/ubuntu/carlos.quesada/disk/go2/ext/",
  "/home/ubuntu/carlos.quesada/disk/iss/ext/",
  "/home/ubuntu/carlos.quesada/disk/lcl/ext/",
  "/home/ubuntu/carlos.quesada/disk/meg/ext/",
  "/home/ubuntu/carlos.quesada/disk/por/ext/"
)

# Output parameters
output_path <-
  "/home/ubuntu/carlos.quesada/disk/features/feats_21.05.26/feats.csv"

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
  .errorhandling = "stop",
  .packages      = c("whyT2.1")

) %dopar% {

  # Select file name
  fpath <- fpaths[x]
  fname <- strsplit(basename(fpath), split=".RData")[[1]]
  print(fname)
  # Load extended dataframe
  load(fpath)
  # Set exceptions
  if (!edf$is_0) {
    ff_file <- data.frame(file = fname, data_set = edf$dset_key)
    # GET FEATURES
    ff_feats <- get_features_from_cooked_dataframe(
      cdf              = edf,
      type_of_analysis = type_of_analysis
    )
    # Incorporate filename as a column
    all_features <- cbind(ff_file, ff_feats)
    return(all_features)
  } else {
    return(NULL)
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