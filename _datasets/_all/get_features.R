#library(whyT2.1)
library(foreach)
# Input parameters
input_folder      <- c(
	"/home/ubuntu/carlos.quesada/disk/go2/ext/",
	"/home/ubuntu/carlos.quesada/disk/iss/ext/",
	"/home/ubuntu/carlos.quesada/disk/lcl/ext/",
	"/home/ubuntu/carlos.quesada/disk/meg/ext/",
	"/home/ubuntu/carlos.quesada/disk/por/ext/"
)
output_folder     <- c(
	"/home/ubuntu/carlos.quesada/disk/features/go2_21.05.27/",
	"/home/ubuntu/carlos.quesada/disk/features/iss_21.05.27/",
	"/home/ubuntu/carlos.quesada/disk/features/lcl_21.05.27/",
	"/home/ubuntu/carlos.quesada/disk/features/meg_21.05.27/",
	"/home/ubuntu/carlos.quesada/disk/features/por_21.05.27/"
)
type_of_analysis   <- "extra" #"basic"


# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

# Analysis loop
for(i in 1:length(input_folder))
{
  # Get list of filenames in dataset folder
  dset_filenames <- list.files(input_folder[i], pattern="*.RData")
  
  foreach::foreach(x = 1:length(dset_filenames),.packages=c("whyT2.1"),.errorhandling='remove') %dopar% {
    
  # Select file name
  dset_filename <- dset_filenames[x]
  #print(dset_filename)
  # Load extended dataframe
  load(paste0(input_folder, dset_filename))
  
  # Set exceptions
  if (!edf$is_0) {
    ff_file <- data.frame(
      file     = gsub(".RData", "", dset_filename),
      data_set = edf$dset_key
    )
    # GET FEATURES
    ff_feats <- get_features_from_cooked_dataframe(edf, type_of_analysis,
                                                   list_of_functions, .scale)
    # Incorporate filename as a column
    all_features <- cbind(ff_file, ff_feats)
    # Output file name
    o_file <- paste0(output_folder, "feats-", Sys.getpid(), ".csv")
    # Save results to the CSV file
    data.table::fwrite(
      x         = all_features,
      file      = o_file,
      sep       = ",",
      na        = "",
      quote     = FALSE,
      append    = TRUE,
      col.names = x <= cores,
      row.names = FALSE
    )
  }
  rm(c(edf,ff_feats,ff_file))
  return(NULL)
  }
}

# Stop parallelization
parallel::stopCluster(cl)





#list_of_functions <- c("stat_data_aggregates", "load_factors")
#.scale            <- FALSE

# # Compute features
# for (ii in 1:1) {
# 	feats <- whyT2.1::get_features_from_ext_datasets(input_folder[ii], output_folder[ii], type_of_analysis)
# 	#feats <- whyT2.1::get_features_from_ext_datasets(input_folder[ii], output_folder[ii], type_of_analysis, 
# 	#list_of_functions=list_of_functions, .scale=.scale, parallelize=TRUE)
# }
