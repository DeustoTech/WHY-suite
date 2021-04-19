input <- "D:/Quesada/Documents/__TRABAJO/Deusto/WHY/Datos (raw)/EnerNOC/csv/"
output <- "D:/Quesada/Documents/__TRABAJO/Deusto/WHY/Datasets/enernoc/"

# Get list of files
data_files <- list.files(input)
# Remove last file in list
data_files <- data_files[-101]

for (ii_file in data_files) {
  print(ii_file)
  # Read csv
  df <- read.table(
    file = paste(input, ii_file, sep = ""),
    header = TRUE,
    sep = ","
  )
  # Create new data.frame
  new_df <- data.frame(df$dttm_utc, df$value)
  # Transform new data.frame into csv
  new_filename <- paste(output, ii_file, sep="")
  write.table(
    new_df,
    file = new_filename,
    quote = F,
    sep = ",",
    col.names = F,
    row.names = F
  )
}
