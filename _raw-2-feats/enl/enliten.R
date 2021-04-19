input <- "D:/Quesada/Documents/__TRABAJO/Deusto/WHY/Datos (raw)/ENLITEN/by_sensor/"
output <- "D:/enliten/"

# Get filenames in folder
filenames <- list.files(input)

for (ii_file in filenames) {
  print(ii_file)
  # Read csv
  df <- read.table(
    file = paste(input, ii_file, sep = ""),
    header = F,
    sep = "\t"
  )
  # Sort dates
  sorted_df <- sort(df$V5, index.return = T)
  # Write csv
  write.table(
    data.frame(df$V5[sorted_df$ix], df$V4[sorted_df$ix]),
    file = paste(output, gsub(".tsv", ".csv", ii_file), sep=""),
    quote = F,
    sep = ",",
    col.names = F,
    row.names = F
  )
}

