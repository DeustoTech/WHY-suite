library(whyT2.1)
library(foreach)

# User parameters
input_folder  <- "/home/ubuntu/carlos.quesada/disk/por/raw/"
output_folder <- "/home/ubuntu/carlos.quesada/disk/por/ext/"
metadata_file <- NULL

# Function call
whyT2.1::extend_dataset(
  input_folder, 
  output_folder, 
  wanted_days = 800,
  dset_key = "por"
  #metadata_files = metadata_file,
  #to_date = as.POSIXct("2020-02-29 23:00:00", tz="GMT"),
  #extend_after_end = FALSE
)
