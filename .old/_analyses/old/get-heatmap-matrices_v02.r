library(foreach)

  ##############################################################################
  ##  PATHS
  ##############################################################################
  
  # User defined variables
  if (.Platform$OS.type == "windows") {
    feats_path  <- "G:/Mi unidad/WHY/Features/feats_v1.11.csv"
    clValid_dir <- "G:/Mi unidad/WHY/Analyses/clValid/data/"
    dataset_dir <- "G:/Mi unidad/WHY/Datasets/"
    output_dir  <- "G:/Mi unidad/WHY/Analyses/clValid/heatmap_mat/"
  }
  if (.Platform$OS.type == "unix") {
    feats_path  <- "/home/ubuntu/carlos.quesada/disk/features/feats_v1.11.csv"
    clValid_dir <- "/home/ubuntu/carlos.quesada/analyses/clValid/clValid_files/"
    dataset_dir <- "/home/ubuntu/carlos.quesada/disk/"
    output_dir  <- "/home/ubuntu/carlos.quesada/analyses/clValid/heatmap_mat/"
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
    sep    = ",",
    select = c("data_set", "file", "imputed_na_pct", "is_household")
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
  foreach::foreach(dd = 1:4) %dopar% {
    # Rows
    row_conditions <- 
      feats$data_set %in% dset_keys[[dd]]$keys &
      feats$imputed_na_pct < dset_keys[[dd]]$imp_na_pct &
      feats$is_household %in% dset_keys[[dd]]$is_hhold
    
    w_feats <- feats[row_conditions,]
    # Columns
    # w_feats <- subset(w_feats, select = feats_set[[1]])
    
    # Method loop
    foreach::foreach(mm = c(2,5:9,1,3)) %dopar% {
      # Filename 
      w_fname <- paste0(
        "o_", feats_set_idx, dd, mm, validation_set_idx, cluster_set_idx, ".clValid"
      )
      w_fpath <- paste0(clValid_dir, w_fname)
      
      if (file.exists(w_fpath)) {
        load(w_fpath)
        ### Get the clustering 
        # HIERARCHICAL
        if (mm == 1) {
          # CLASIFICACIÓN MUY MALA: LO METE CASI TODO EN EL CLUSTER #1
          cluster_list <- cutree(o@clusterObjs[["hierarchical"]], k=cluster_number)
        }
        # K-MEANS
        if (mm == 2) {
          # CLASIFICACIÓN OK
          cluster_list <- o@clusterObjs[["kmeans"]][[cluster_number]][["cluster"]]
        }
        # DIANA
        if (mm == 3) {
          # CLASIFICACIÓN MUY MALA: LO METE CASI TODO EN EL CLUSTER #1
          cluster_list <- cutree(o@clusterObjs[["diana"]], k=cluster_number)
        }
        # FANNY
        if (mm == 4) {
          # NO HA FUNCIONADO, SOLO HACE 3 CLUSTERS, Y CASI TODO EN CLUSTER #1
        }
        # SOM
        if (mm == 5) {
          # CLASIFICACIÓN OK
          cluster_list <- o@clusterObjs[["som"]][[cluster_number]]$unit.classif
        }
        # PAM
        if (mm == 6) {
          # CLASIFICACIÓN OK
          cluster_list <- o@clusterObjs[["pam"]][[cluster_number]]$clustering
        }
        # SOTA
        if (mm == 7) {
          # CLASIFICACIÓN MUY MALA: LO METE CASI TODO EN LOS CLUSTERS #1-#4
          cluster_list <- o@clusterObjs[["sota"]][[cluster_number]]$clust
        }
        # CLARA
        if (mm == 8) {
          # CLASIFICACIÓN OK
          cluster_list <- o@clusterObjs[["clara"]][[cluster_number]]$clustering
        }
        # MODEL-BASED
        if (mm == 9) {
          # CLASIFICACIÓN OK
          cluster_list <- o@clusterObjs[["model"]][[cluster_number]]$classification
        }
      }
      
      # Cluster loop
      foreach::foreach (cc = cluster_number:1, .inorder = FALSE) %dopar% {
        print(paste(dd, mm, cc))
        # Get cluster indices
        idx <- cluster_list == cc
        # Set vector of paths
        paths_vector <- paste0(
          dataset_dir,
          w_feats$data_set[idx],
          "/ext/",
          w_feats$file[idx],
          ".RData"
        )
        # Get heatmap matrix
        m <- whyT2.1::get_heatmap_matrix(data.frame(paths_vector))
        # Save heatmap matrix
        o_fname <- paste0(
          output_dir, "hmm_", feats_set_idx, dd, mm, "_", cc, ".RData"
        )
        save(m, file = o_fname)
      }
    }
  }
  
  # Stop parallelization
  parallel::stopCluster(cl)
