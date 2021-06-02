################################################################
##  Put conventional features and catch-22 features together  ##
################################################################

# Conventional features file path
ft_path <- "G:/Mi unidad/WHY/Features/feats_21.05.26/feats.csv"
# Catch-22 folder
c22_dir <- "G:/Mi unidad/WHY/Features/feats_21.05.26/"
# Catch-22 file vector
c22_v <- paste0(c22_dir, "c22_feats_", seq(1,23501,500), ".csv")
# Output path
ft_out <- "G:/Mi unidad/WHY/Features/feats_21.05.26/feats-all.csv"

# Read conventional feats file
conv_feats <- data.table::fread(
  file   = ft_path,
  header = TRUE,
  sep    = ","
)

# Read Catch-22 feats file
c22_feats <- data.frame()
for (c22_path in c22_v) {
  c22_df <- data.table::fread(
    file   = c22_path,
    header = TRUE,
    sep    = ","
  )
  c22_feats <- rbind(c22_feats, c22_df)
}

# Put feats together
feats <- merge(conv_feats, c22_feats, by=c("file", "data_set"))
feats_2 <- plyr::join(conv_feats, c22_feats, by=c("file", "data_set"))

# # Save feats to the CSV file
# data.table::fwrite(
#   x          = feats,
#   file       = ft_out,
#   sep        = ",",
#   na         = "",
#   quote      = FALSE,
#   append     = FALSE,
#   col.names  = TRUE,
#   row.names  = FALSE,
#   dateTimeAs = "write.csv"
# )
  
browser()