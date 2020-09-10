# Carlos Quesada - Universidad de Deusto
# 2020.07.15
# Plot 2013 Feb from dataset Low Carbon London

# Load source file and libraries
source("why-source_v03.R")

# User parameters
dset_key <- "lcl"
from_date <- ISOdate(2013, 2, 1, 0, 0, 0) #"first"
to_date <- ISOdate(2013, 2, 28, 23, 30, 0) #"last"
accepted_na_rate <- 0.0

samples_per_day <- list(lcl = 48)
step_in_secs <- 86400 / samples_per_day[[dset_key]]

# Get list of filenames in dataset folder 
dset_filenames <- Get_File_List(dset_key)

for (dset_filename in dset_filenames) {
  # Load dataset file
  print(dset_filename)
  dset_data <- Load_Dataset_File(dset_key, dset_filename)
  
  # Get values from dataset file
  dset_data_intvl <- Get_Data_Interval(
    tm_series = dset_data,
    from_date = from_date,
    to_date = to_date,
    step = step_in_secs # 30 * 60
  )
  
  pdf(file=paste(substring(dset_filename,1,9),".pdf",sep=""))
  plot(
    seq(from_date, to_date, as.difftime(step_in_secs, units = "secs")),
    dset_data_intvl,
    type = "l",
    main = dset_filename,
    xlab = "Date",
    ylab = "kWh",
    ylim = c(0,5)
  )
  dev.off()
}
