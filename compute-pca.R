# Carlos Quesada - Universidad de Deusto
# 2020.07.13

# Compute PCA from a CSV file of features

# Load source file and libraries
source("why-source.R")

################################################################################
# -- User parameters
################################################################################

# Dataset key
res_folder <- "G:/Mi unidad/WHY/Resultados/lcl/features/"
#feats_subfolder <- "2013 Feb, 0% NA, scale=FALSE, 70 feats/"
feats_subfolder <- "2012-2013, 0% NA, scale=FALSE, 70 feats/"
# Observations to plot
#otp <- 1:4605
otp <- 1:84129

# Features to plot
# -- All features
#ftp <- c(1:10, 15:70)
# -- Statistical features
#ftp <- 1:10
# -- STL features
#ftp <- 15:26
# -- Autocorrelation features
#ftp <- 28:34
# -- Stats + STL + Acorr + Entropy features
#ftp <- c(1:10, 15:34)
# -- Quantiles + seasonal strengths
# <- c(5:9, 21:22)
# -- Mean, variance + seasonal strengths
ftp <- c(1:2, 21:22)

# Axes selection
axis_x <- 1
axis_y <- 2

# Get the identification of a point on a plot window
get_point_identity <- TRUE

################################################################################

# Path to features and data info
feats_folder <- paste(res_folder, feats_subfolder, "feats.csv", sep="")

# Load features from CSV file
feats <- read.table(
  file = feats_folder,
  header = TRUE,
  sep = ","
)

# PCA
pca <- prcomp(feats[otp, ftp], scale. = TRUE)

# Plot PCA
plot(
  pca[["x"]][, axis_x],
  pca[["x"]][, axis_y],
  col = "blue",
  pch = "+",
  xlab = axis_x,
  ylab = axis_y
)

# Get point identification

if (get_point_identity == TRUE) {
  # Folder
  data_info_folder <- paste(
    res_folder, 
    feats_subfolder, 
    "data_info.csv", 
    sep=""
  )
  
  # Load data info from CSV file
  data_info <- read.table(
    file = data_info_folder,
    header = TRUE,
    sep = ","
  )
  
  # Get points
  ts_ids <- identify(
    x = pca[["x"]][, axis_x], 
    y = pca[["x"]][, axis_y],
    plot = FALSE
  )
  # Print points
  for (ts_id in ts_ids) {
      print(ts_id)
  }
}
