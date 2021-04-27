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

# Loop of keys (data sets)
kk_loop <- 4:4
# Loop of feature sets
ss_loop <- 1:1
# Loop of clustering methods
mm_loop <- 1:1
# Loop of k-means centers
cc_loop <- 12:25

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
    x <- c(
      "mean", "entropy", "seasonal_strength1", "seasonal_strength2",
      "seasonal_strength3", "ac_day_1", "ac_day_7", "ac_day_28")
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
##  Cluster measures
################################################################################

# FUNCTION to compute the cluster measures
get_cluster_measures <- function() {
  
  # FUNCTION THAT COMPUTES SS (SQUARED SUM)
  sqsm <- function(x) sum(scale(x, scale = FALSE)^2)
  
  # Name of the cme file to open or create
  cme_file <- paste0("cme_", key, "_s", ss, "_c", cc, ".RData")
  # Does this computation already exist?
  if (!check_existence_of("cme", cme_file)) {
    ### Compute tot.withinss
    withinss <- sapply(split(w_feats, km$cluster), sqsm)
    tot_withinss <- sum(withinss)
    ### Compute silhouette function
    sil <- cluster::silhouette(x = km$cluster, dist = dist(w_feats))
    ### Davies-Bouldin's Index
    db_index <- clusterSim::index.DB(x = w_feats, cl = km$cluster)
    # Save file
    cme <- list(elbow = tot_withinss, db_index = db_index, silhouette = sil)
    save(cme, file = here::here(key, "cme", cme_file))
  } else {
    # Load file
    load(here::here(key, "cme", cme_file))
  }
  
  # Name of the cme file to open or create
  cmepng_file <- paste0("cmepng_", key, "_s", ss, "_c", cc, ".png")
  # Does this computation already exist?
  if (!check_existence_of("cmepng", cmepng_file)) {
    png(
      here::here(key, "cmepng", cmepng_file),
      width = 900,
      height = 1200
    )
    # Plot cluster contents
    par(fig=c(0,1,0,1), cex=1.0)
    plot(cme$silhouette, col=1:cc, border=NA)
    # Save file
    dev.off()
  }
  
  return(cme)
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
  # # Name of the hmm file to open or create
  # hmpdf_file <- paste0("hmpdf_", key, "_s", ss, "_c", cc, "-", ii, ".pdf")
  # 
  # # Does this computation already exist?
  # if (!check_existence_of("hmpdf", hmpdf_file)) {
  #   whyT2.1::plot_heatmap_matrix(
  #     hmm,
  #     format_file = "pdf",
  #     file_path = here::here(key, "hmpdf", hmpdf_file)
  #   )
  # }
  
  # Name of the hmm file to open or create
  hmpng_file <- paste0("hmpng_", key, "_s", ss, "_c", cc, "-", ii, ".png")
  
  # Does this computation already exist?
  if (!check_existence_of("hmpng", hmpng_file)) {
    whyT2.1::plot_heatmap_matrix(
      hmm,
      format_file = "png",
      file_path = here::here(key, "hmpng", hmpng_file),
      plot_width = 1200,
      plot_height = 900
    )
  }
}

################################################################################
##  Get cluster statistics
################################################################################

get_cluster_statistics <- function() {
  # Name of the stats file to open or create
  sta_file <- paste0("sta_", key, "_s", ss, "_c", cc, "-", ii, ".png")
  # Does this computation already exist?
  if (!check_existence_of("sta", sta_file)) {
    png(
      here::here(key, "sta", sta_file),
      width = 1200,
      height = 900
    )
    # FEATURE SET 1
    if (ss == 1) {
      # Plot cluster contents (features 1 to 24)
      par(fig=c(0,0.88,0,1))
      par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
      cluster_list <- w_feats[km$cluster == ii, 1:24]
      short_labels <- substr(names(cluster_list), 10, 18)
      boxplot(cluster_list, las=2, names=short_labels)
      # Plot cluster contents (features 25 & 26)
      par(fig=c(0.88,1,0,1), new=TRUE)
      par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
      cluster_list <- w_feats[km$cluster == ii, 25:26]
      short_labels <- substr(names(cluster_list), 10, 16)
      boxplot(cluster_list, las=2, names=short_labels)
    }
    # REST OF FEATURE SETS
    if (ss != 1) {
      # Plot cluster contents (features 1 to 24)
      par(fig=c(0,1,0,1))
      par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
      cluster_list <- w_feats[km$cluster == ii,]
      # short_labels <- substr(names(cluster_list), 10, 18)
      boxplot(cluster_list, las=2)
    }
    # Save file
    dev.off()
  }
}

################################################################################
##  Get socioeconomic indicators
################################################################################

get_soc_ec_indicators <- function() {
  if (dset == "lcl") {
    # Name of the stats file to open or create
    sei_file <- paste0("sei_", key, "_s", ss, "_c", cc, "-", ii, ".png")
    # Does this computation already exist?
    if (!check_existence_of("sei", sei_file)) {
      # Retrieve ACORN values
      acorn <- subset(
        x      = feats,
        subset = rows,
        select = "acorn"
      ) 
      
      # General ACORN table
      if (!exists("acorn_table")) {
        acorn_table <- table(acorn)
      }
      
      # Get cluster list
      cluster_table <- table(acorn[km$cluster == ii])
      # Bind
      cluster_acorn <- rbind(acorn_table, cluster_table, fill=TRUE)
      cluster_acorn[is.na(cluster_acorn)] <- 0
      # Get percentages of acorn representation
      pc_acorn <- cluster_acorn[2,] / cluster_acorn[1,]
      
      # Open png file
      png(
        here::here(key, "sei", sei_file),
        width = 1200,
        height = 900
      )
      # Plot cluster contents (features 1 to 24)
      par(fig=c(0,1,0,1))
      par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
      barplot(pc_acorn)
  
      # Save file
      dev.off()
    }
  }
}

################################################################################
##  Get elbow curve
################################################################################

get_elbow <- function() {
  # Name of the elb file to open or create
  elb_file <- paste0("elb_", key, "_s", ss, ".png")
  # Does this computation already exist?
  if (!check_existence_of("elb", elb_file)) {
    # Get all elbows
    elbow <- c()
    for (cc in cc_loop) {
      # Name of the cme file to load
      cme_file <- paste0("cme_", key, "_s", ss, "_c", cc, ".RData")
      # Load file
      load(here::here(key, "cme", cme_file))
      # Get elbows
      elbow <- c(elbow, cme$elbow)
    }
    ### Create elbow plot
    # Open png file
    png(
      here::here(key, "elb", elb_file),
      width = 1200,
      height = 900
    )
    # Plot cluster contents (features 1 to 24)
    par(fig=c(0,1,0,1))
    par(cex=1.0, mai=c(0.8,0.5,0.05,0.05))
    plot(cc_loop, elbow)
    # Save file
    dev.off()
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
foreach::foreach (kk = kk_loop) %do% {
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
  foreach::foreach (ss = ss_loop) %do% {
    # Load the set of features
    feats_set <- get_feats_set(ss)
    # Load the proper subset of features (working features)
    w_feats <- subset(
      x      = feats,
      subset = rows,
      select = feats_set
    )
    
    # Loop of clustering methods
    foreach::foreach (mm = mm_loop) %do% {
      # K-MEANS
      if (mm == 1) {
        
        # Loop for k-means centers
        foreach::foreach (cc = cc_loop) %do% {
          # Compute k-means
          km <- kmeans_fnct()
          # Compute cluster measures
          cme <- get_cluster_measures()
          
          # Loop for each of the centers
          foreach::foreach (ii = 1:cc, .packages = packs, .inorder = F) %dopar% {
            # Compute the heatmap matrix
            hmm <- heatmap_mat_fnct()
            # PLOT hmm as PDF and PNG
            heatmap_plot_fnct()
            # Compute statistics of each cluster
            get_cluster_statistics()
            # Compute socioeconomic indicators of each cluster
            get_soc_ec_indicators()
          }
        }
        
        # Compute elbow curve
        elb <- get_elbow()
      }
      
      # OTHER CLUSTERING METHODS TO COME
      if (mm == 2) {
        
      }
    }
  }
}

# Stop parallelization
parallel::stopCluster(cl)
