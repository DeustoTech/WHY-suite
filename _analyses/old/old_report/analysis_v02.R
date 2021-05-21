################################################################################
##  ANALYSIS v2
##  R file with the analysis functions of the Rmd file
################################################################################

library(here)
library(foreach)
library(whyT2.1)

################################################################################
##  Constants
################################################################################

# Version of "feats" file
vers <- "1.08"
# Set random seed
seed_value <- 1981
set.seed(seed_value)
# k-means iter.max
kmeans_iter.max <- 200
# k-means nstart
kmeans_nstart <- 50
# Interval of centers to analyze with k-means
km_cc_ival <- 2:20

################################################################################
##  Paths to extended files of time series
################################################################################

get_folders <- function() {
  o <- list()
  
  if (.Platform$OS.type == "windows") {
    # Paths to datasets
    if (dset == "lcl")
      o$ext_path <- "G:/Mi unidad/WHY/Datasets/lcl/ext/"
    if (dset == "goi")
      o$ext_path <- "G:/Mi unidad/WHY/Datasets/goi/ext/"
    if (dset == "iss")
      o$ext_path <- "G:/Mi unidad/WHY/Datasets/iss/ext/"
    # Path to "feats" file
    o$feats_path <- "G:/Mi unidad/WHY/Features/"
  }
  if (.Platform$OS.type == "unix") {
    # Paths to datasets
    if (dset == "lcl")
      o$ext_path <- "/home/ubuntu/carlos.quesada/disk/lcl/ext/"
    if (dset == "goi")
      o$ext_path <- "/home/ubuntu/carlos.quesada/disk/goi/ext/"
    if (dset == "iss")
      o$ext_path <- "/home/ubuntu/carlos.quesada/disk/iss/ext/"
    # Path to "feats" file
    o$feats_path <- "/home/ubuntu/carlos.quesada/disk/features/"
  }
  
  return(o)
}

################################################################################
##  FUNCTION to choose a set of features
################################################################################

get_feats_set <- function(idx) {
  if (idx == 1) {
    x <- c(
      "rel_mean_00h04hspr", "rel_mean_04h08hspr", "rel_mean_08h12hspr",
      "rel_mean_12h16hspr", "rel_mean_16h20hspr", "rel_mean_20h00hspr",
      "rel_mean_00h04hsum", "rel_mean_04h08hsum", "rel_mean_08h12hsum",
      "rel_mean_12h16hsum", "rel_mean_16h20hsum", "rel_mean_20h00hsum",
      "rel_mean_00h04haut", "rel_mean_04h08haut", "rel_mean_08h12haut",
      "rel_mean_12h16haut", "rel_mean_16h20haut", "rel_mean_20h00haut",
      "rel_mean_00h04hwin", "rel_mean_04h08hwin", "rel_mean_08h12hwin",
      "rel_mean_12h16hwin", "rel_mean_16h20hwin", "rel_mean_20h00hwin",
      "rel_mean_weekday_pday", "rel_mean_weekend_pday")
  }
  if (idx == 2) {
    x <- c(
      "peak_hour_1", "off_peak_hour_1", "peak_month", "off_peak_month",
      "peak_weekday_pday")
  }
  if (idx == 3) {
    x <- c() # TO BE FILLED
  }
  if (idx == 4) {
    x <- c() # TO BE FILLED
  }
  return(x)
}

################################################################################
##  File existence checker
################################################################################

check_existence_of <- function(folder, file) {
  # Check if folder not exists
  if (!file.exists(here::here(dset, folder))) {
    # Create folder if not exist
    dir.create(here::here(dset, folder))
    # File does not exist
    return(FALSE)
  } else {
    # Check if file exists
    return(file.exists(here::here(dset, folder, file)))
  }
}

################################################################################
##  k-means
################################################################################

# FUNCTION to compute k-means
kmeans_fnct <- function() {
  # Name of the km file to open or create
  km_file <- paste0("km_", dset, "_s", ss, "_c", cc, ".RData")
  
  # Does this computation already exist?
  if (!check_existence_of("km", km_file)) {
    # Compute k-means
    km <- stats::kmeans(
      w_feats,
      cc,
      iter.max  = kmeans_iter.max,
      nstart    = kmeans_nstart,
      algorithm = "MacQueen"
    )
    # Save file
    save(km, file = here::here(dset, "km", km_file))
    
  } else {
    # Load file
    load(here::here(dset, "km", km_file))
  }
  
  return(km)
}

################################################################################
##  Heatmap matrix
################################################################################

# FUNCTION to compute the heatmap matrix

heatmap_mat_fnct <- function() {
  # Name of the hmm file to open or create
  hmm_file <- paste0("hmm_", dset, "_s", ss, "_c", cc, "-", cc_idx, ".RData")
  
  # Does this computation already exist?
  if (!check_existence_of("hmm", hmm_file)) {
    ### km is already loaded!!!
    # # Name of the km file to open
    # km_file <- paste0("km_", dset, "_s", ss, "_c", cc, ".RData")
    # # Load the corresponding km file
    # load(here::here(dset, "km", km_file))
    
    # Retrieve filenames
    file_names <- subset(
      x      = feats,
      subset = rows,
      select = "file"
    ) 
    # Get cluster list
    cluster_list <- file_names[km$cluster == cc_idx]
    # Set vector of paths
    paths_vector <- paste0(o$ext_path, cluster_list$file, ".RData")
    # Get heatmap matrix
    hmm <- whyT2.1::get_heatmap_matrix(data.frame(paths_vector))
    # Save heatmap matrix
    save(hmm, file = here::here(dset, "hmm", hmm_file))
    
  } else {
    # Load file
    load(here::here(dset, "hmm", hmm_file))
  }
  
  return(hmm)
}

################################################################################
##  MAIN ANALYSIS LOOP
################################################################################

# List of datasets to analyze
dset_list <- c("goi", "iss", "lcl")

# Loop of datasets
for (dset in dset_list) {
  # Folder list
  o <- get_folders()

  # Load feats
  if (!exists("feats")) {
    feats <- data.table::fread(
      file   = paste0(o$feats_path, "feats_v", vers, ".csv"),
      header = TRUE,
      sep    = ","
    )
  }
  
  # Create folder if not exist
  if (!file.exists(here::here(dset))) {
    dir.create(here::here(dset))
  }
  
  # Selection of rows from feats
  rows <-
    feats$data_set == dset  &       
    feats$is_household == 1 &       
    feats$total_imputed_pct < 2/3
  
  # Loop of sets of features
  for (ss in 1:2) {
    # Load the set of features
    feats_set <- get_feats_set(ss)
    # Load the proper subset of features (working features)
    w_feats <- subset(
      x      = feats,
      subset = rows,
      select = feats_set
    )
    
    # Loop of clustering methods
    for (mm in 1) {
      
      # K-MEANS
      if (mm == 1) {
        # Loop for k-means centers
        for (cc in km_cc_ival) {
          # Compute k-means
          km <- kmeans_fnct()

          # Loop for each of the centers
          for (cc_idx in 1:cc) {
            hmm <- heatmap_mat_fnct()
            # PLOT hmm
          }
        }
      }
      
      # OTHER CLUSTERING METHODS TO COME
      if (mm == 2) {
        
      }
    }
  }
}
