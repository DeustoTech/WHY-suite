#######################################################################
###  INTEGRATION OF FEAT FILES INTO THE BIG FILE (feats_v1.XX.csv)  ###
#######################################################################

# User defined variables
if (.Platform$OS.type == "windows") {
  # Folder of the big file
  curr_bigfile_dir <- "G:/Mi unidad/WHY/Features/"
  # Last version of the big file in the folder
  last_bigfile_vs <- "1.10"
  # New wanted version of the big file after the incorporation of data
  new_bigfile_vs <- "1.11"
  # Folder of the new features
  new_feats_dir <- "G:/Mi unidad/WHY/Features/meg_21.04.15/"
  # Key of the new features
  key <- "meg"
  # Path to metadata file
  metadata_file <- "G:/Mi unidad/WHY/Datos (raw)/MEGARA/metadata-meg.csv"
}
if (.Platform$OS.type == "unix") {
  # TO BE FILLED IF REQUIRED
}

# Path to the last version of feats
last_bigfile_path <- paste0(curr_bigfile_dir, "feats_v", last_bigfile_vs, ".csv")
# Path to the new wanted version of feats
new_bigfile_path <- paste0(curr_bigfile_dir, "feats_v", new_bigfile_vs, ".csv")

# Get the big file
curr_bigfile_df <- data.table::fread(
  file   = last_bigfile_path,
  header = TRUE,
  sep    = ","
)

# Get the files in the folder of features
new_feats_filenames <- list.files(new_feats_dir)

# Get the dataframe with the new features
new_feats_df <- data.frame()
for (new_feats_filename in new_feats_filenames) {
  new_feats_df_ii <- data.table::fread(
    file   = paste0(new_feats_dir, new_feats_filename),
    header = TRUE,
    sep    = ","
  )
  new_feats_df <- rbind(new_feats_df, new_feats_df_ii)
}
new_feats_df$file <- sub('\\.RData$', '', new_feats_df$file)

# Get the metadata file
metadata_df <- data.table::fread(
  file   = metadata_file,
  header = TRUE,
  sep    = ","
)

# Bind columns (includes all rows in x)
aux_bigfile_df <- dplyr::left_join(
  x  = new_feats_df,
  y  = metadata_df,
  by = c("file" = "file")
)

# Bind rows
new_bigfile_df <- dplyr::bind_rows(curr_bigfile_df, aux_bigfile_df)

# Write file
data.table::fwrite(
  new_bigfile_df,
  file       = paste0(curr_bigfile_dir, "feats_v", new_bigfile_vs, ".csv"),
  row.names  = FALSE,
  col.names  = TRUE,
  sep        = ",",
  na         = "",
  dateTimeAs = "write.csv",
  quote      = FALSE
)
