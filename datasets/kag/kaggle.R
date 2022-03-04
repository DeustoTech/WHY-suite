input <- "D:/Quesada/Documents/__TRABAJO/Deusto/WHY/Datos (raw)/Kaggle/train.csv"
output <- "D:/kaggle/"

# Read csv
df <- read.table(
  file = input,
  header = F,
  sep = ","
)

# Write csv
for (building_id in 0:1448) {
  print(building_id)
  write.table(
    data.frame(df[df$V1 == building_id & df$V2 == 0, c(3, 4)]),
    file = paste(output, building_id, ".csv", sep=""),
    quote = F,
    sep = ",",
    col.names = F,
    row.names = F
  )
}
