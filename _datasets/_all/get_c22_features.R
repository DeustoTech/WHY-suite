library(whyT2.1)
library(foreach)

# Accept arguments from command line
args <- commandArgs(trailingOnly = TRUE)
args <- as.numeric(args)

# Size of step
step_size <- 499

# Input parameters
input_folder <- c(
  "/home/ubuntu/carlos.quesada/disk/go2/ext/",
  "/home/ubuntu/carlos.quesada/disk/iss/ext/",
  "/home/ubuntu/carlos.quesada/disk/lcl/ext/",
  "/home/ubuntu/carlos.quesada/disk/meg/ext/",
  "/home/ubuntu/carlos.quesada/disk/por/ext/"
)
# Output parameters
output_path <- paste0(
  "/home/ubuntu/carlos.quesada/disk/features/feats_21.05.26/c22_feats_",
  args[1],
  ".csv"
)
# Type of analysis
type_of_analysis <- "custom"
# List of functions
list_of_functions <- c("catch22_features")
# Scale (it's irrelevant, catch22 scales anyway)
.scale <- TRUE

# List of file paths
fpaths <- c()
for (ii in 1:length(input_folder)) {
  # Get list of file paths
  fpaths <- c(
    fpaths,
    sort(list.files(input_folder[ii], pattern="*.RData", full.names = T))
  )
}

fpaths_len <- length(fpaths)
min_val <- min(fpaths_len, args[1]+step_size)

if (args[1] > fpaths_len) {
  stop(paste0("NO MORE FILES! max = ", fpaths_len, ", curr = ", args[1]))
}

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

o <- foreach::foreach(
  x              = args[1]:min_val,
  .combine       = rbind,
  .inorder       = TRUE,
  .errorhandling = "stop",
  .packages      = c("whyT2.1")
) %dopar% {

  # Select file name
  fpath <- fpaths[x]
  fname <- strsplit(basename(fpath), split=".RData")[[1]]
  print(paste0(x, fpath))
  # Load extended dataframe
  load(fpath)
  # Set exceptions
  if (!edf$is_0) {
    ff_file <- data.frame(file = fname, data_set = edf$dset_key)
    # GET FEATURES
    ff_feats <- get_features_from_cooked_dataframe(
      cdf               = edf,
      type_of_analysis  = type_of_analysis,
      list_of_functions = list_of_functions,
      .scale            = .scale
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

print("AQUI LLEGO")

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
