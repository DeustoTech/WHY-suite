input <- "D:/Quesada/Documents/__TRABAJO/Deusto/WHY/Datos (raw)/ISSDA/38_CER Electricity_Gas/CER Electricity Revised March 2012/output/"
output <- "D:/Quesada/Documents/__TRABAJO/Deusto/WHY/Datos (raw)/ISSDA/38_CER Electricity_Gas/CER Electricity Revised March 2012/output_2/"

for(ii_file in list.files(input)[1:6435]) {
  # Transform file into data.frame
  filename <- paste(input, ii_file, sep="")
  print(ii_file)
  tseries_dframe <- read.table(
    file = filename,
    header = F,
    sep = ";"
  )
  # Transform columns into dates
  dates <- substr(
    ISOdate(
      tseries_dframe[,1],
      tseries_dframe[,2],
      tseries_dframe[,3],
      tseries_dframe[,4],
      tseries_dframe[,5],
      tseries_dframe[,6]
    ), 1, 19)
  # Create new data.frame
  new_df <- data.frame(dates, tseries_dframe[,7])
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
