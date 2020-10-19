# Carlos Quesada - Universidad de Deusto
# 2020.09.03
# Compute a combination of PCA and k-means from a CSV file of features

# Load source file and libraries
library(whyT2.1)
# Cluster-PCA visualization
library(factoextra)
# Set a seed for random numbers
set.seed(1981)

################################################################################
# -- User parameters
################################################################################

# Code of the dataset
dset_key <- "lcl"
# Dataset key
root_folder <- "G:/Mi unidad/WHY/Resultados/lcl/features/"
feats_subfolder <- "2013 Feb, 0% NA, scale=FALSE, 70 feats/"

# Compute only PCA?
only_pca <- F
# Number of PCA components to compute k-means
pca_components <- 4

# Elbow method or standard k-means?
elbow_method <- F
max_clusters <- 20
# In case elbow_method is FALSE
number_of_centers <- 4

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

# Axes selection in PCA
axis_x <- 1
axis_y <- 2

# Plot accumulated months of each cluster?
month_accumul <- FALSE

# Plot mean and variance of all TS in cluster
cluster_accumul <- TRUE

################################################################################

# Path to features
feats_folder <- paste(root_folder, feats_subfolder, "feats.csv", sep="")
datainfo_folder <- paste(root_folder, feats_subfolder, "data_info.csv", sep="")

# Load data from CSV file
feats <- read.table(
  file = feats_folder,
  header = TRUE,
  sep = ","
)

# Load data info
data_info <- read.table(
  file = datainfo_folder,
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

################################################################################
# Plot cluster accumulated 
################################################################################

# library(matrixStats)

if (cluster_accumul == TRUE) {
  for (center in 1:number_of_centers) {
    # Initialization
    all_ts <- data.frame()
    # Load data
    ts_ids   <- as.numeric(names(km$cluster[km$cluster == center]))
    ts_names <- data_info[ts_ids, 1]
    
    for (ts_name in ts_names) {
      print(ts_name)
      ts <- Load_Dataset_File(DATASET_PATH, ts_name)
      dset_data_intvl <- Get_Data_Interval(
        dset_data = ts,
        from_date = as.POSIXct("2013-02-01 00:00:00", tz="GMT"),
        to_date   = as.POSIXct("2013-02-28 23:30:00", tz="GMT"),
        sampling_period = 86400 / SAMPLES_PER_DAY[["lcl"]] # 30 * 60
      )
      all_ts <- rbind(all_ts, dset_data_intvl$values)
    }
    
    all_means <- colMeans(all_ts)
    # all_sds   <- colSds(data.matrix(all_ts))
    
    plot(dset_data_intvl$times, 
         all_means,
         type = "l", 
         xlab = "February 2013", 
         ylab = "kWh",
         main = paste("Cluster #", 
                      center, 
                      " (", 
                      length(ts_names), 
                      " time series)", 
                      sep=""),
         col  = "blue",
         ylim = c(0,2)
    )
    # lines(dset_data_intvl$times,
    #       all_means + all_sds,
    #       col  = "red"
    # )
    # lines(dset_data_intvl$times,
    #       all_means - all_sds,
    #       col  = "red"
    # )

  }
}

################################################################################
# Plot month accumulated consumption of each cluster
################################################################################

if (month_accumul == TRUE) {
  for (center in 1:number_of_centers) {
    # Initialization
    total_mean <- numeric(48)
    # Load data
    ts_ids <- as.numeric(names(km$cluster[km$cluster == center]))
    ts_names <- data_info[ts_ids, 1]
    length_ts_names <- length(ts_names)
    
    for (ts_name in ts_names) {
      ts <- Load_Dataset_File(DATASET_PATH, ts_name)
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
}
