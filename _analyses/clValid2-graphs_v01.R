library(foreach)

##############################################################################
##  get_feature_graphs()
##############################################################################

get_feature_graphs <- function() {
  # Create dir if it does not exist
  dir.create(paste0(root_dir, "graph/"))
  # Name of the file to create
  ft_fname <- paste0("graph_", n1, n2, n3, "_", cc, "-", cluster_number, ".png")
  # Save as PNG
  png(
    paste0(root_dir, "graph/", ft_fname),
    width = 1200,
    height = 900
  )
  # FEATURE SET 1
  if (n1 == 1) {
    # Plot cluster contents (features 1 to 24)
    par(fig=c(0,0.9,0,1))
    par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
    column_names <- feats_set[[n1]][1:24]
    short_labels <- substr(column_names, 10, 18)
    cluster_elems <- w_feats[cluster_idx, ..column_names]
    boxplot(cluster_elems, las=2, names=short_labels)
    # Plot cluster contents (feature 25)
    par(fig=c(0.9,1,0,1), new=TRUE)
    par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
    column_names <- feats_set[[n1]][25]
    short_labels <- substr(column_names, 10, 16)
    cluster_elems <- w_feats[cluster_idx, ..column_names]
    boxplot(cluster_elems, las=2, names=short_labels)
  }
  # REST OF FEATURE SETS
  if (n1 != 1) {
    # Plot cluster contents (features 1 to 7)
    par(fig=c(0,1,0,1))
    par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
    cluster_elems <- w_feats[km$cluster == ii,]
    # short_labels <- substr(names(cluster_elems), 10, 18)
    boxplot(cluster_elems, las=2)
  }
  # Save file
  dev.off()
}

##############################################################################
##  PATHS
##############################################################################

# User defined variables
if (.Platform$OS.type == "windows") {
  feats_path <- "G:/Mi unidad/WHY/Features/feats_v1.12.csv"
  root_dir   <- "G:/Mi unidad/WHY/Analyses/clValid2/data/"
}
if (.Platform$OS.type == "unix") {
  feats_path <- "/home/ubuntu/carlos.quesada/disk/features/feats_v1.12.csv"
  root_dir   <- "/home/ubuntu/carlos.quesada/analyses/clValid2/"
}

################################################################################
##  SELECTABLE VARIABLES
################################################################################

# 1st DIGIT: SETS OF FEATURES --------------------------------------------------
feats_set <- list(
  # TYPE 1
  c(
    "rel_mean_00h04hspr", "rel_mean_04h08hspr", "rel_mean_08h12hspr",
    "rel_mean_12h16hspr", "rel_mean_16h20hspr", "rel_mean_20h00hspr",
    "rel_mean_00h04hsum", "rel_mean_04h08hsum", "rel_mean_08h12hsum",
    "rel_mean_12h16hsum", "rel_mean_16h20hsum", "rel_mean_20h00hsum",
    "rel_mean_00h04haut", "rel_mean_04h08haut", "rel_mean_08h12haut",
    "rel_mean_12h16haut", "rel_mean_16h20haut", "rel_mean_20h00haut",
    "rel_mean_00h04hwin", "rel_mean_04h08hwin", "rel_mean_08h12hwin",
    "rel_mean_12h16hwin", "rel_mean_16h20hwin", "rel_mean_20h00hwin",
    "rel_mean_weekday_pday"
  ),
  # TYPE 2
  c(
    "peak_hour_1", "off_peak_hour_1", "peak_month", "off_peak_month",
    "peak_weekday_pday"
  ),
  # TYPE 3
  c(
    "mean", "entropy", "seasonal_strength1", "seasonal_strength2",
    "seasonal_strength3", "ac_day_1", "ac_day_7", "ac_day_28"
  ),
  # TYPE 4
  c(
    # CATCH22 - TO BE FILLED
  )
)

# 2nd DIGIT: DATASETS ----------------------------------------------------------
dset_keys <- list(
  # TYPE 1
  list(keys = c("go2", "meg"), is_hhold = 1,   imp_na_pct = 0.1, sum_pday = 0.1),
  # TYPE 2
  list(keys = c("lcl"),        is_hhold = 1,   imp_na_pct = 0.1, sum_pday = 0.1),
  # TYPE 3
  list(keys = c("iss"),        is_hhold = 1,   imp_na_pct = 0.1, sum_pday = 0.1),
  # TYPE 4
  list(keys = c("go2", "meg"), is_hhold = 0:1, imp_na_pct = 0.1, sum_pday = 0.1)
)

# 3rd DIGIT: CLUSTERING METHOD -------------------------------------------------
cluster_methods <- c(
  # TYPE 1
  "hierarchical",
  # TYPE 2
  "kmeans",
  # TYPE 3
  "diana",
  # TYPE 4
  "fanny",
  # TYPE 5
  "som",
  # TYPE 6
  "pam",
  # TYPE 7
  "sota",
  # TYPE 8
  "clara",
  # TYPE 9
  "model"
)

# 4th DIGIT: VALIDATION --------------------------------------------------------
validation <- list(
  # TYPE 1
  i = "internal", 
  # TYPE 2
  s = "stability", 
  # TYPE 3
  b = "biological"
)

# 5th DIGIT: NUMBER OF CLUSTERS ------------------------------------------------
cluster_set <- list(
  # TYPE 1
  c(5, 10, 15, 20, seq(22, 40, 2), 50, 60, 70),
  # TYPE 2
  24
)

################################################################################
##  LOAD FEATURES
################################################################################

# Load feats
feats <- data.table::fread(
  file   = feats_path,
  header = TRUE,
  sep    = ","
)

feats_set_idx      <- 1
# dataset
# cluster method
validation_set_idx <- 1
cluster_set_idx    <- 2
cluster_number     <- 24

################################################################################
##  LOOP
################################################################################

fnames <- list.files(path = root_dir, pattern = "*.clValid2")

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

# Dataset loop
foreach::foreach(ff = 1:length(fnames)) %:%
  foreach::foreach(cc = 1:cluster_number, .inorder = FALSE, errorhandling = "remove") %do% {
  
    w_fname <- fnames[ff]
    
    print(paste0(w_fname, " - ", cc))
    
    # Extract data from filename
    n1 <- as.numeric(substr(w_fname, 3, 3)) # features
    n2 <- as.numeric(substr(w_fname, 4, 4)) # datasets
    n3 <- as.numeric(substr(w_fname, 5, 5)) # cluster method
    n4 <- as.numeric(substr(w_fname, 6, 6)) # validation method
    n5 <- as.numeric(substr(w_fname, 7, 7)) # cluster sequence
    
    # Rows
    row_conditions <- 
      feats$data_set %in% dset_keys[[n2]]$keys &
      feats$imputed_na_pct < dset_keys[[n2]]$imp_na_pct &
      feats$is_household %in% dset_keys[[n2]]$is_hhold &
      feats$sum_per_day > dset_keys[[n2]]$sum_pday
    
    browser()
    # Working features
    w_feats <- feats[row_conditions,]
    w_fpath <- paste0(root_dir, w_fname)
    
    if (file.exists(w_fpath)) {
      load(w_fpath)
      ### Get the clustering 
      # HIERARCHICAL
      if (n3 == 1) {
        # CLASIFICACION MUY MALA: LO METE CASI TODO EN EL CLUSTER #1
        cluster_list <- cutree(o@clusterObjs[["hierarchical"]], k=cluster_number)
      }
      # K-MEANS
      if (n3 == 2) {
        # CLASIFICACION OK
        cluster_list <- o@clusterObjs[["kmeans"]][[as.character(cluster_number)]][["cluster"]]
      }
      # DIANA
      if (n3 == 3) {
        # CLASIFICACION MUY MALA: LO METE CASI TODO EN EL CLUSTER #1
        cluster_list <- cutree(o@clusterObjs[["diana"]], k=cluster_number)
      }
      # FANNY
      if (n3 == 4) {
        # NO HA FUNCIONADO, SOLO HACE 3 CLUSTERS, Y CASI TODO EN CLUSTER #1
        cluster_list <- o@clusterObjs[["fanny"]][[as.character(cluster_number)]]$clustering
      }
      # SOM
      if (n3 == 5) {
        # CLASIFICACION OK
        cluster_list <- o@clusterObjs[["som"]][[as.character(cluster_number)]]$unit.classif
      }
      # PAM
      if (n3 == 6) {
        # CLASIFICACION OK
        cluster_list <- o@clusterObjs[["pam"]][[as.character(cluster_number)]]$clustering
      }
      # SOTA
      if (n3 == 7) {
        # CLASIFICACION MUY MALA: LO METE CASI TODO EN LOS CLUSTERS #1-#4
        cluster_list <- o@clusterObjs[["sota"]][[as.character(cluster_number)]]$clust
      }
      # CLARA
      if (n3 == 8) {
        # CLASIFICACION OK
        cluster_list <- o@clusterObjs[["clara"]][[as.character(cluster_number)]]$clustering
      }
      # MODEL-BASED
      if (n3 == 9) {
        # CLASIFICACION OK
        cluster_list <- o@clusterObjs[["model"]][[as.character(cluster_number)]]$classification
      }
      ###########################
      ##  Get cluster indices  ##
      ###########################
      cluster_idx <- cluster_list == cc
      
      ##################
      ##  Get graphs  ##
      ##################
      get_feature_graphs()
      
      # # GOI & MEG
      # if (dd == 1 | dd == 4) {
      # }
      # # LCL
      # if (dd == 2) {
      # }
      # # ISS
      # if (dd == 3) {
      # }
      
    }
}

# Stop parallelization
parallel::stopCluster(cl)
