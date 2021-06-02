library(whyT2.1)
library(foreach)

# User parameters
input_folder  <- "/home/ubuntu/carlos.quesada/disk/nee/raw/"
output_folder <- "/home/ubuntu/carlos.quesada/disk/nee/ext/"
metadata_file <- NULL

# Function call
whyT2.1::extend_dataset(
  input_folder, 
  output_folder, 
  wanted_days = 800,
  dset_key = "nee",
  metadata_files = "/home/ubuntu/carlos.quesada/disk/nee/pre-raw/sites.csv"
  #to_date = as.POSIXct("2020-02-29 23:00:00", tz="GMT"),
  #extend_after_end = FALSE
)
