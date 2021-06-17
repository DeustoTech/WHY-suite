# ADD sum_per_day

file_in  <- "G:/Mi unidad/WHY/Features/feats_v1.18.csv"
file_out <- "G:/Mi unidad/WHY/Features/feats_v1.19.csv"

feats <- data.table::fread(
  file   = file_in,
  header = TRUE,
  sep    = ","
)

acorn_groups <- list(
  A=1, B=1, C=1,
  D=2, E=2,
  F=3, G=3, H=3, I=3, J=3,
  K=4, L=4, M=4, N=4,
  O=5, P=5, Q=5,
  U=""
)

feats$sum_per_day <- feats$sum / feats$overall_days
feats$acorn <- substr(feats$acorn, 7, 7)
feats$acorn_grouped <- acorn_groups[replace(feats$acorn, feats$acorn=="", "U")]

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