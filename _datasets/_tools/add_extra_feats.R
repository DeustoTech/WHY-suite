# ADD sum_per_day

file_in  <- "G:/Mi unidad/WHY/Features/feats_v1.16.csv"
file_out <- "G:/Mi unidad/WHY/Features/feats_v1.17.csv"

feats <- data.table::fread(
  file   = file_in,
  header = TRUE,
  sep    = ","
)

feats$sum_per_day <- feats$sum / feats$overall_days

# Save feats to the CSV file
data.table::fwrite(
  x          = feats,
  file       = file_out,
  sep        = ",",
  na         = "",
  quote      = FALSE,
  append     = FALSE,
  col.names  = TRUE,
  row.names  = FALSE,
  dateTimeAs = "write.csv"
)