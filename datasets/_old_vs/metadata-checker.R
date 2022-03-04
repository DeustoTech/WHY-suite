### FILE TO CHECK HOW MANY USER FILES HAVE METADATA INFO

### USER DEFINED VARIABLES
if (.Platform$OS.type == "windows") {
  goiener_users_folder  <- "C:/go2_users/"
  goiener_output_folder <- "C:/go2_raw/"
  goiener_report_folder <- "C:/go2_raw/"
  goiener_metadata_file <- "G:/Mi unidad/WHY/Datos (raw)/GOIENER/Contratos_Goiener_20210301_anonymized.csv"
}
if (.Platform$OS.type == "unix") {
  goiener_users_folder  <- "/home/ubuntu/carlos.quesada/disk/go2/users/"
  goiener_output_folder <- "/home/ubuntu/carlos.quesada/disk/go2/raw/"
  goiener_report_folder <- "/home/ubuntu/carlos.quesada/disk/go2/"
  goiener_metadata_file <- "/home/ubuntu/carlos.quesada/R_scripts/Contratos_Goiener_20210301_anonymized.csv"
}

# Read metadata file
df <- data.table::fread(
  file       = goiener_metadata_file,
  header     = TRUE,
  sep        = ",",
  na.strings = NULL,
  select     = 1
)
df <- as.vector(df$cups_ref)

# Read files in folder
u_fnames <- list.files(goiener_users_folder, pattern="^[[:xdigit:]]")
u_fnames <- substr(u_fnames, 1,32)

# Metadated files
mf <- u_fnames %in% df
print(
  paste0(sum(mf), " out of ", length(mf), " files (", round(100*sum(mf)/length(mf),2), "%) have metadata.")
)
