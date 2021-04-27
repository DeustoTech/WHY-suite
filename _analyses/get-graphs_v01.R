library(foreach)

get_socioeconomic_graphs <- function() {
  
  ##############################################################################
  ##  get_lcl_se_graphs()
  ##############################################################################
  
  get_lcl_ft_graphs <- function() {
    # Name of the file to create
    lcl_fname <- paste0("graph_", feats_set_idx, dd, mm, "_", cc, "_1.png")
    # Save as PNG
    png(
      paste0(root_dir, "graph/", lcl_fname),
      width = 1200,
      height = 900
    )
    # FEATURE SET 1
    if (feats_set_idx == 1) {
      # Plot cluster contents (features 1 to 24)
      par(fig=c(0,0.88,0,1))
      par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
      short_labels <- substr(feats_set[[feats_set_idx]][1:24], 10, 18)
      browser()
      cluster_elems <- w_feats[cluster_idx, feats_set[[feats_set_idx]][1:24]]
      boxplot(cluster_elems, las=2, names=short_labels)
      # Plot cluster contents (feature 25)
      par(fig=c(0.88,1,0,1), new=TRUE)
      par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
      cluster_elems <- w_feats[cluster_idx, feats_set[[feats_set_idx]][25]]
      short_labels <- substr(names(cluster_elems), 10, 16)
      boxplot(cluster_elems, las=2, names=short_labels)
    }
    # REST OF FEATURE SETS
    if (feats_set_idx != 1) {
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
    feats_path <- "G:/Mi unidad/WHY/Features/feats_v1.11.csv"
    root_dir   <- "G:/Mi unidad/WHY/Analyses/clValid/"
  }
  if (.Platform$OS.type == "unix") {
    feats_path <- "/home/ubuntu/carlos.quesada/disk/features/feats_v1.11.csv"
    root_dir   <- "/home/ubuntu/carlos.quesada/analyses/clValid/"
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
    list(keys = c("go2", "meg"), is_hhold = 1,   imp_na_pct = 0.1),
    # TYPE 2
    list(keys = c("lcl"),        is_hhold = 1,   imp_na_pct = 0.1),
    # TYPE 3
    list(keys = c("iss"),        is_hhold = 1,   imp_na_pct = 0.1),
    # TYPE 4
    list(keys = c("go2", "meg"), is_hhold = 0:1, imp_na_pct = 0.1)
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
    c(5, 10, 15, 20, seq(22, 40, 2), 50, 60, 70)
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
  
  feats_set_idx <- 1
  cluster_set_idx <- 1
  validation_set_idx <- 1
  cluster_number <- 24
  
  ################################################################################
  ##  LOOP
  ################################################################################
  
  # Setup parallel backend to use many processors
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)
  
  # Dataset loop
  # foreach::foreach(dd = 2:4) %:%
  #   foreach::foreach(mm = c(1:3, 5:9)) %:%
  #     foreach::foreach (cc = cluster_number:1, .inorder = FALSE) %dopar% {
        
  for (dd in 2:4) {
    for (mm in c(1:3, 5:9)) {
      for (cc in cluster_number:1) {
        
        # Cluster loop
        print(paste(dd, mm, cc))
        
        # Rows
        row_conditions <- 
          feats$data_set %in% dset_keys[[dd]]$keys &
          feats$imputed_na_pct < dset_keys[[dd]]$imp_na_pct &
          feats$is_household %in% dset_keys[[dd]]$is_hhold
        
        # Working features
        w_feats <- feats[row_conditions,]
        
        # Filename 
        w_fname <- paste0(
          "o_", feats_set_idx, dd, mm, validation_set_idx, cluster_set_idx, ".clValid"
        )
        w_fpath <- paste0(root_dir, "data/", w_fname)

        if (file.exists(w_fpath)) {
          load(w_fpath)
          ### Get the clustering 
          # HIERARCHICAL
          if (mm == 1) {
            # CLASIFICACION MUY MALA: LO METE CASI TODO EN EL CLUSTER #1
            cluster_list <- cutree(o@clusterObjs[["hierarchical"]], k=cluster_number)
          }
          # K-MEANS
          if (mm == 2) {
            # CLASIFICACION OK
            cluster_list <- o@clusterObjs[["kmeans"]][[as.character(cluster_number)]][["cluster"]]
          }
          # DIANA
          if (mm == 3) {
            # CLASIFICACION MUY MALA: LO METE CASI TODO EN EL CLUSTER #1
            cluster_list <- cutree(o@clusterObjs[["diana"]], k=cluster_number)
          }
          # FANNY
          if (mm == 4) {
            # NO HA FUNCIONADO, SOLO HACE 3 CLUSTERS, Y CASI TODO EN CLUSTER #1
          }
          # SOM
          if (mm == 5) {
            # CLASIFICACION OK
            cluster_list <- o@clusterObjs[["som"]][[as.character(cluster_number)]]$unit.classif
          }
          # PAM
          if (mm == 6) {
            # CLASIFICACION OK
            cluster_list <- o@clusterObjs[["pam"]][[as.character(cluster_number)]]$clustering
          }
          # SOTA
          if (mm == 7) {
            # CLASIFICACION MUY MALA: LO METE CASI TODO EN LOS CLUSTERS #1-#4
            cluster_list <- o@clusterObjs[["sota"]][[as.character(cluster_number)]]$clust
          }
          # CLARA
          if (mm == 8) {
            # CLASIFICACION OK
            cluster_list <- o@clusterObjs[["clara"]][[as.character(cluster_number)]]$clustering
          }
          # MODEL-BASED
          if (mm == 9) {
            # CLASIFICACION OK
            cluster_list <- o@clusterObjs[["model"]][[as.character(cluster_number)]]$classification
          }
          ###########################
          ##  Get cluster indices  ##
          ###########################
          cluster_idx <- cluster_list == cc
          
          # GOI & MEG
          if (dd == 1 | dd == 4) {
            
          }
          # LCL
          if (dd == 2) {
            get_lcl_ft_graphs()
          }
          # ISS
          if (dd == 3) {
            
          }
          
          # # Set vector of paths
          # paths_vector <- paste0(
          #   dataset_dir,
          #   w_feats$data_set[idx],
          #   "/ext/",
          #   w_feats$file[idx],
          #   ".RData"
          # )
          # # Get heatmap matrix
          # m <- whyT2.1::get_heatmap_matrix(data.frame(paths_vector))
          # # Save heatmap matrix
          # o_fname <- paste0(
          #   output_dir, "hmm_", feats_set_idx, dd, mm, "_", cc, ".RData"
          # )
          # save(m, file = o_fname)
        }
      }
    }
  }
  
  # Stop parallelization
  parallel::stopCluster(cl)
}

get_socioeconomic_graphs()
