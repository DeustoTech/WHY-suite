out_path <- "G:/Mi unidad/WHY/Features/feats_21.06.01/new_feats.csv"

new_feats <- data.table::fread(
  file   = "G:/Mi unidad/WHY/Features/feats_21.06.01/feats.csv",
  header = TRUE
)

new_mdata <- data.table::fread(
  file   = "G:/Mi unidad/WHY/Features/feats_21.06.01/2021.06.03_mdata.csv",
  header = TRUE
)

# CONCATENATE
feats <- merge(new_mdata, new_feats, by=c("file", "data_set"))

# Save
data.table::fwrite(
  x         = feats,
  file      = out_path,
  append    = F,
  quote     = F,
  sep       = ",",
  row.names = F,
  col.names = T,
  dateTimeAs = "write.csv"
)