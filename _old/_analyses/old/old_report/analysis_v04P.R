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
km_cc_ival <- 8:12

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
  if (!file.exists(here::here(key, folder))) {
    # Create folder if not exist
    dir.create(here::here(key, folder))
    # File does not exist
    return(FALSE)
  } else {
    # Check if file exists
    return(file.exists(here::here(key, folder, file)))
  }
}

################################################################################
##  k-means
################################################################################

# FUNCTION to compute k-means
kmeans_fnct <- function() {
  # Name of the km file to open or create
  km_file <- paste0("km_", key, "_s", ss, "_c", cc, ".RData")
  
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
    save(km, file = here::here(key, "km", km_file))
    
  } else {
    # Load file
    load(here::here(key, "km", km_file))
  }
  
  return(km)
}

################################################################################
##  Heatmap matrix
################################################################################

# FUNCTION to compute the heatmap matrix
heatmap_mat_fnct <- function() {
  # Name of the hmm file to open or create
  hmm_file <- paste0("hmm_", key, "_s", ss, "_c", cc, "-", ii, ".RData")
  
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
    cluster_list <- file_names[km$cluster == ii]
    # Set vector of paths
    paths_vector <- paste0(o$ext_path, cluster_list$file, ".RData")
    # Get heatmap matrix
    hmm <- whyT2.1::get_heatmap_matrix(data.frame(paths_vector))
    # Save heatmap matrix
    save(hmm, file = here::here(key, "hmm", hmm_file))
    
  } else {
    # Load file
    load(here::here(key, "hmm", hmm_file))
  }
  
  return(hmm)
}

################################################################################
##  Heatmap plot
################################################################################

heatmap_plot_fnct <- function() {
  # Name of the hmm file to open or create
  hmpdf_file <- paste0("hmpdf_", key, "_s", ss, "_c", cc, "-", ii, ".pdf")
  
  # Does this computation already exist?
  if (!check_existence_of("hmpdf", hmpdf_file)) {
    whyT2.1::plot_heatmap_matrix(
      hmm,
      format_file = "pdf",
      file_path = here::here("hmpdf", hmpdf_file)
    )
  }
  
  # Name of the hmm file to open or create
  hmpng_file <- paste0("hmpng_", key, "_s", ss, "_c", cc, "-", ii, ".png")
  
  # Does this computation already exist?
  if (!check_existence_of("hmpng", hmpng_file)) {
    whyT2.1::plot_heatmap_matrix(
      hmm,
      format_file = "png",
      file_path = here::here("hmpng", hmpng_file)
    )
  }
}

################################################################################
##  MAIN ANALYSIS LOOP
################################################################################

# List of keys (xxxY: xxx <- dataset, Y <- H for households, A for all)
key_list <- c("goiH", "issH", "lclH", "goiA")
# Packages to be passed in parallelization
packs <- c("whyT2.1", "here", "stats", "lubridate", "data.table")

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores)
doParallel::registerDoParallel(cl)

# Loop of keys
foreach::foreach (kk = 1:length(key_list)) %do% {
  # Get key
  key <- key_list[kk]
  # Convert key into dataset and options
  if (key == "goiH") {
    dset <- "goi"
    only_households <- T
  }
  if (key == "goiA") {
    dset <- "goi"
    only_households <- F
  }
  if (key == "issH") {
    dset <- "iss"
    only_households <- T
  }
  if (key == "lclH") {
    dset <- "lcl"
    only_households <- T
  }
  
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
  if (!file.exists(here::here(key))) {
    dir.create(here::here(key))
  }
  
  # Selection of rows from feats
  if (only_households) {
    rows <-
      feats$data_set == dset  &       
      feats$is_household == 1 &       
      feats$total_imputed_pct < 2/3
  } else {
    rows <-
      feats$data_set == dset  &       
      (feats$is_household == 1 | feats$is_household == 0)  &       
      feats$total_imputed_pct < 2/3
  }
  
  # Loop of sets of features
  foreach::foreach (ss = 1:2) %do% {
  # for (ss in 1:2) {
    # Load the set of features
    feats_set <- get_feats_set(ss)
    # Load the proper subset of features (working features)
    w_feats <- subset(
      x      = feats,
      subset = rows,
      select = feats_set
    )
    
    # Loop of clustering methods
    foreach::foreach (mm = 1:1) %do% {
      # K-MEANS
      if (mm == 1) {
        # Loop for k-means centers
        
        foreach::foreach (cc = km_cc_ival) %do% {
          # Compute k-means
          km <- kmeans_fnct()

          # Loop for each of the centers
          foreach::foreach (ii = 1:cc, .packages = packs, .inorder = F) %dopar% {
            # Compute the heatmap matrix
            hmm <- heatmap_mat_fnct()
            # PLOT hmm as PDF and PNG
            heatmap_plot_fnct()
          }
        }
      }
      
      # OTHER CLUSTERING METHODS TO COME
      if (mm == 2) {
        
      }
    }
  }
}

# Stop parallelization
parallel::stopCluster(cl)
