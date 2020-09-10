# Carlos Quesada - Universidad de Deusto
# 2020.07.21
# Create a library of features so they can be visually understood

# Load source file and libraries
source("why-source_v03.R")
library(plotly)

# User parameters
dset_key <- "lcl"
from_date <- ISOdate(2013, 2, 1, 0, 0, 0) #"first"
to_date <- ISOdate(2013, 2, 28, 23, 30, 0) #"last"
samples_per_day <- list(lcl = 48)
step_in_secs <- 86400 / samples_per_day[[dset_key]]

# Function to generate plots
generate_plot <- function(feature, filename, value) {
  # Load dataset file
  dset_data <- Load_Dataset_File(dset_key, filename)
  # Get values from dataset file
  dset_data_intvl <- Get_Data_Interval(
    tm_series = dset_data,
    from_date = from_date,
    to_date = to_date,
    step = step_in_secs # 30 * 60
  )
  # Create plot
  p <- plot(
    seq(from_date, to_date, as.difftime(step_in_secs, units = "secs")),
    dset_data_intvl,
    type = "l",
    main = paste(feature, "=", value),
    xlab = "Date",
    ylab = "kWh",
    #ylim = c(0,5)
  )
}

# Root subfolder
root_subfolder <- paste(
  glb_root_folder,
  "Low Carbon London/features/2013 Feb, 0% NA, scale=FALSE/",
  # "Low Carbon London/features/whole file/",
  sep = ""
)

# Load data from CSV file
feats <- read.table(
  file = paste(root_subfolder, "feats.csv", sep = ""),
  header = TRUE,
  sep = ","
)
data_info <- read.table(
  file = paste(root_subfolder, "data_info.csv", sep = ""),
  header = TRUE,
  sep = ","
)

# Indices of series to be plotted
seq_idx <- seq(1, nrow(feats), length=9)

for (ii in 5:60) {
  # Feature name
  feat_name <- names(feats)[ii]
  # Get representative filenames of this feature ii
  sorted_col <- sort(feats[[ii]], index.return = TRUE)
  selected_idx <- sorted_col$ix[seq_idx]
  repres_fnames <- data_info$filename[selected_idx]
  repres_values <- sorted_col$x[seq_idx]
  # Plot
  pdf(
    file = paste(ii, " - ", feat_name, ".pdf", sep = ""),
    paper = "special", #"a4r",
    width = 20,
    height = 15
  )
  par(mfrow = c(3,3))
  for (jj in 1:9) {
    generate_plot(feat_name, repres_fnames[jj], repres_values[jj])
  }
  dev.off()
}

