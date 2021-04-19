library(whyT2.1)
library(foreach)

### USER DEFINED VARIABLES
if (.Platform$OS.type == "windows") {
  raw_folder <- "C:/go2_raw/"
  ext_folder <- "C:/go2_ext/"
  mdata_file <- "G:/Mi unidad/WHY/Datos (raw)/GOIENER/Contratos_Goiener_20210301_anonymized.csv"
}
if (.Platform$OS.type == "unix") {
  raw_folder <- "/home/ubuntu/carlos.quesada/disk/go2/raw/"
  ext_folder <- "/home/ubuntu/carlos.quesada/disk/go2/ext/"
  mdata_file <- "/home/ubuntu/carlos.quesada/R_scripts/Contratos_Goiener_20210301_anonymized.csv"
}



# Function call
whyT2.1::extend_dataset(
  raw_folder, 
  ext_folder, 
  wanted_days = 800,
  dset_key = "go2",
  metadata_files = mdata_file,
  to_date = as.POSIXct("2020-02-29 23:00:00", tz="GMT"),
  extend_after_end = FALSE
)
