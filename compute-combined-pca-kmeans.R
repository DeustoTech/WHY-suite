# Carlos Quesada - Universidad de Deusto
# 2020.09.03
# Compute PCA from a CSV file of features

# Load source file and libraries
source("why-source_v04.R")
# Cluster-PCA visualization
library(factoextra)

# Dataset key
dset_key <- "lcl"
feats_folder <- "2013 Feb, 0% NA, scale=FALSE, 70 feats/"

# Compute only PCA?
only_pca <- F
# Number of PCA components to compute k-means
pca_components <- 4

# Elbow method or standard k-means?
elbow_method <- F
max_clusters <- 20
# In case elbow_method is FALSE
number_of_centers <- 8

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
# -- Stats + STL + ACF + Entropy features
#ftp <- c(1:10, 15:34)
# -- Quantiles + ACFs
#ftp <- c(5:9, 28:34, 54:55, 19:20)
# -- Quantiles + seasonal strengths
ftp <- c(5:9, 21:22)

# Axes selection
axis_x <- 1
axis_y <- 2

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

################################################################################

if (only_pca == F) {
  # Selection of some PCA components
  x <- pca$x[,1:pca_components]
  
  # K-means elbow method
  if (elbow_method == T) {
    sum_of_squares <- vector()
  
    for(ii in 1:max_clusters) {
      print(ii)
      km <- kmeans(
        x = x,
        centers = ii,
        iter.max = 500,
        nstart = 10000
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
      nstart = 10000
    )
  
    # Visualize clusters with
    plot(fviz_cluster(km, data = x))
  }
}

# Plot clusters content ########################################################
for (center in 1:number_of_centers) {
  # Initialization
  total_mean <- numeric(48)
  # Load data
  ts_ids <- as.numeric(names(km$cluster[km$cluster == center]))
  ts_names <- data_info[ts_ids, 1]
  length_ts_names <- length(ts_names)
  
  for (ts_name in ts_names) {
    ts <- Load_Dataset_File(dset_key, "", ts_name)
    print(ts_name)
    for (day in 1:27) {
      # Take just a day
      ts_ival <- Get_Data_Interval(
        ts,
        ISOdate(2013, 2, day, 0, 0, 0),
        ISOdate(2013, 2, day + 1, 0, 0, 0), 
        86400 / 48
      )
      total_mean <- total_mean + (1/length_ts_names/28) * ts_ival
    }
  }
  plot(seq(0,24,0.5),
       ts_ival, 
       type = "l", 
       ylim=c(0,5), 
       xlab="Hours", 
       ylab="kWh",
       main=paste("Cluster #", center, sep="")
  )
}
