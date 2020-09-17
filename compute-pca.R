# Carlos Quesada - Universidad de Deusto
# 2020.07.13
# Compute PCA from a CSV file of features

# Load source file and libraries
source("why-source.R")

################################################################################
# -- User parameters
################################################################################

# Dataset key
dset_key <- "lcl"
root_folder <- "G:/Mi unidad/WHY/Resultados/lcl/features/"
feats_subfolder <- "2013 Feb, 0% NA, scale=FALSE, 70 feats/feats.csv"
# Observations to plot
otp <- 1:4605

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
ftp <- c(1:10, 15:34)

# Axes selection
axis_x <- 1
axis_y <- 2

################################################################################

# Path to features
feats_folder <- paste(results_folder, feats_subfolder, sep="")

# Load data from CSV file
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
  col = "red",
  xlab = axis_x,
  ylab = axis_y
)
