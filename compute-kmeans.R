# Carlos Quesada - Universidad de Deusto
# 2020.09.02
# Compute k-means from a CSV file of features

# Load source file and libraries
source("why-source_v04.R")
# Cluster-PCA visualization
library(factoextra)

# Dataset key
dset_key <- "lcl"
feats_folder <- "2013 Feb, 0% NA, scale=FALSE, 70 feats/"

# Elbow method or standard k-means?
elbow_method <- F
max_clusters <- 20
# In case elbow_method is FALSE
number_of_centers <- 16

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
# -- Stats + Acorr features
ftp <- c(1:10, 28:34)
# -- Stats + STL + Acorr + Entropy features
#ftp <- c(1:10, 15:34)


# Path to features
feats_subfolder <- paste(
  "G:/Mi unidad/WHY/Resultados/", 
  dset_key,
  "/features/",
  feats_folder,
  sep = ""
)

# Load data from CSV file
feats <- read.table(
  file = paste(feats_subfolder, "feats.csv", sep = ""),
  header = TRUE,
  sep = ","
)

# Load data info
data_info <- read.table(
  file = paste(feats_subfolder, "data_info.csv", sep = ""),
  header = TRUE,
  sep = ","
)


x <- feats[otp, ftp]

# K-means elbow method
if (elbow_method == T) {
  sum_of_squares <- vector()
  
  for(ii in 1:max_clusters) {
    print(ii)
    km <- kmeans(
      x = x,
      centers = ii,
      iter.max = 500,
      nstart = 500
      )
    sum_of_squares <- c(sum_of_squares, km$tot.withinss)
    print(km$tot.withinss)
  }
  
  # Show results
  plot(1:max_clusters, sum_of_squares)
  
# Standard k-means method
} else {
  km <- kmeans(
    x = x,
    centers = number_of_centers,
    iter.max = 500,
    nstart = 500
  )
  
  # Visualize clusters with 
  plot(fviz_cluster(km, data = x))
}
