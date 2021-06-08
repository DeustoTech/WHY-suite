library(foreach)

clValid2_heatmaps <- function() {
  ##############################################################################
  ##  PATHS
  ##############################################################################
  
  # User defined variables
  if (.Platform$OS.type == "windows") {
    feats_path  <- "G:/Mi unidad/WHY/Features/feats_v1.12.csv"
    clValid_dir <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.05.05_3-cl-methods/data/"
    dataset_dir <- "G:/Mi unidad/WHY/Datasets/"
    hmm_dir     <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.05.05_3-cl-methods/hmm/"
    hmp_dir     <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.05.05_3-cl-methods/hmp/"
    source("G:/Mi unidad/WHY/Github/why-T2.1/_analyses/selectable_variables.R", local=T)
  }
  if (.Platform$OS.type == "unix") {
    feats_path  <- "/home/ubuntu/carlos.quesada/disk/features/feats_v1.17.csv"
    clValid_dir <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2021.06.08_km-som-var-cl/data/"
    dataset_dir <- "/home/ubuntu/carlos.quesada/disk/"
    hmm_dir     <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2021.06.08_km-som-var-cl/hmm/"
    hmp_dir     <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2021.06.08_km-som-var-cl/hmp/"
    source("/home/ubuntu/carlos.quesada/analyses/selectable_variables.R", local=T)
  }
  
  cluster_map <- c(NA, 16, 16, 30, 6, NA, 40)
  skip_xxx2x <- TRUE
  .scale_hmm <- TRUE
  
  ################################################################################
  ##  LOAD FEATURES
  ################################################################################
  
  # Load feats
  feats <- data.table::fread(
    file   = feats_path,
    header = TRUE,
    sep    = ",",
    select = c("data_set", "file", "imputed_na_pct", "is_household", "sum_per_day", "minimum")
  )
  
  ################################################################################
  ##  LOOP
  ################################################################################
  fnames <- list.files(path = clValid_dir, pattern = "*.clValid2")
  
  # Setup parallel backend to use many processors
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)
  
  o <- foreach::foreach(ff = 1:length(fnames)) %:%
    foreach::foreach(cc = 1:max(cluster_map, na.rm = T), .inorder = FALSE) %dopar% {
	
	  number_of_clusters <- cluster_map[[as.numeric(substr(fnames[[ff]],4,4))]]
	  # print(paste("File", fnames[[ff]], "has", number_of_clusters, "clusters"))
	  if (cc > number_of_clusters) return(NA)
      
      w_fname <- fnames[ff]
      
      # Extract data from filename
      n1 <- as.numeric(substr(w_fname, 3, 3)) # features
      n2 <- as.numeric(substr(w_fname, 4, 4)) # datasets
      n3 <- as.numeric(substr(w_fname, 5, 5)) # cluster method
      n4 <- as.numeric(substr(w_fname, 6, 6)) # validation method
      n5 <- as.numeric(substr(w_fname, 7, 7)) # cluster sequence
      
      # Skip by validation flag
      if (skip_xxx2x & n4 == 2) next
      
      # Rows
      row_conditions <- 
        feats$data_set %in% dset_keys[[n2]]$keys &
        feats$imputed_na_pct < dset_keys[[n2]]$imp_na_pct &
        feats$is_household %in% dset_keys[[n2]]$is_hhold &
        feats$sum_per_day > dset_keys[[n2]]$sum_pday &
		feats$minimum >= 0
      
      w_feats <- feats[row_conditions,]
      w_fpath <- paste0(clValid_dir, w_fname)
      
      load(w_fpath)
      
      ### Get the clustering 
      # HIERARCHICAL
      if (n3 == 1) {
        cluster_list <- cutree(o@clusterObjs[["hierarchical"]], k=number_of_clusters)
      }
      # K-MEANS
      if (n3 == 2) {
        cluster_list <- o@clusterObjs[["kmeans"]][[as.character(number_of_clusters)]][["cluster"]]
      }
      # DIANA
      if (n3 == 3) {
        cluster_list <- cutree(o@clusterObjs[["diana"]], k=number_of_clusters)
      }
      # FANNY
      if (n3 == 4) {
        cluster_list <- o@clusterObjs[["fanny"]][[as.character(number_of_clusters)]]$clustering
      }
      # SOM
      if (n3 == 5) {
        cluster_list <- o@clusterObjs[["som"]][[as.character(number_of_clusters)]]$unit.classif
      }
      # PAM
      if (n3 == 6) {
        cluster_list <- o@clusterObjs[["pam"]][[as.character(number_of_clusters)]]$clustering
      }
      # SOTA
      if (n3 == 7) {
        cluster_list <- o@clusterObjs[["sota"]][[as.character(number_of_clusters)]]$clust
      }
      # CLARA
      if (n3 == 8) {
        cluster_list <- o@clusterObjs[["clara"]][[as.character(number_of_clusters)]]$clustering
      }
      # MODEL-BASED
      if (n3 == 9) {
        cluster_list <- o@clusterObjs[["model"]][[as.character(number_of_clusters)]]$classification
      }
      
      # Cluster loop
      print(paste0(w_fname, " - ", cc))
      
      # Get cluster indices
      idx <- cluster_list == cc
      # Set vector of paths
      paths_vector <- paste0(dataset_dir, w_feats$data_set[idx], "/ext/", w_feats$file[idx], ".RData")
      
      # File paths
      hmm_path <- paste0(hmm_dir, "hmm_", n1, n2, n3, "_", cc, "-", number_of_clusters, ".RData")
      hmp_path <- paste0(hmp_dir, "hmp_", n1, n2, n3, "_", cc, "-", number_of_clusters, ".png")
      
      # Get heatmap matrix
      m <- whyT2.1::get_heatmap_matrix(data.frame(paths_vector), .scale = .scale_hmm)
      # Save heatmap matrix
      save(m, file = hmm_path)
  
      # Generate heatmaps
      whyT2.1::plot_heatmap_matrix(
        m           = m,
        format_file = "png",
        file_path   = hmp_path,
        plot_width  = 1200,
        plot_height = 900
      )
  }
  
  # Stop parallelization
  parallel::stopCluster(cl)
}

clValid2_heatmaps()