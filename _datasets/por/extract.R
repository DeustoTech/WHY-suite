# FILE TO EXTRACT TIME SERIES FROM "PORTUGAL" DATASET

por_path <- "G:/Mi unidad/WHY/Datos (raw)/Portugal/LD2011_2014.txt"

# por <- data.table::fread(
#   file   = por_path,
#   header = TRUE,
#   sep    = ";"
# )

# DATES
date_list <- as.POSIXct(
  x      = por$V1,
  tz     = "GMT",
  format = "%Y-%m-%d %H:%M:%OS"
)

# VALUES
for (ii in 2:ncol(por)) {
  print(ii)
  value_list <- as.numeric(gsub(",", ".", por[[ii]]))
  non_zero_values <- which(value_list != 0)
  idx <- min(non_zero_values):max(non_zero_values)
  if (length(idx) != 0) {
    # Conversion from kW to kWh
    value_list <- value_list / 4
    # Get dataframe
    df <- data.frame(date_list[idx], value_list[idx])
    # Filename
    fname <- as.character(names(por)[[ii]])
  }
  
  # Save
  data.table::fwrite(
    x         = df,
    file      = paste0("G:/Mi unidad/WHY/Datasets/por/raw/", fname, ".csv"),
    append    = F,
    quote     = F,
    sep       = ",",
    row.names = F,
    col.names = F,
    dateTimeAs = "write.csv"
  )
}