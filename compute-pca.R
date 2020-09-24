# Carlos Quesada - Universidad de Deusto
# 2020.07.13

# Compute PCA from a CSV file of features

# Load source file and libraries
source("why-source.R", local=TRUE)

# Dataset key
feats_folder <- "G:/Mi unidad/WHY/Resultados/lcl/features/2012-2013, 0% NA, scale=FALSE, 70 feats/"

# Observations to plot
#otp <- 1:4605
otp <- 1:1000 #1:84129

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

# Function call
Compute_Features_PCA(
  observations  = otp,
  features      = ftp,
  axes          = c(axis_x, axis_y),
  point_identif = get_point_identity
)
