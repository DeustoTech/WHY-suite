##############################################################################
##  PATHS
##############################################################################

# User defined variables
if (.Platform$OS.type == "windows") {
  feats_path <- "G:/Mi unidad/WHY/Features/feats_v1.12.csv"
  root_dir   <- "G:/Mi unidad/WHY/Analyses/clValid2/"
  source("G:/Mi unidad/WHY/Github/why-T2.1/_analyses/selectable_variables.R")
}
if (.Platform$OS.type == "unix") {
  feats_path <- "/home/ubuntu/carlos.quesada/disk/features/feats_v1.12.csv"
  root_dir   <- "/home/ubuntu/carlos.quesada/analyses/clValid2/"
  source("/home/ubuntu/carlos.quesada/analyses/selectable_variables.R")
}

##############################################################################
##  get_feature_graphs()
##############################################################################

get_feature_graphs <- function() {
  # Create dir if it does not exist
  if (!dir.exists(paste0(root_dir, "graph/"))) {
    dir.create(paste0(root_dir, "graph/"))
  }
  # Name of the file to create
  ft_fname <- paste0("graph_", n1, n2, n3, "_", cc, "-", number_of_clusters, ".png")
  # Save as PNG
  png(
    paste0(root_dir, "graph/", ft_fname),
    width = 1200,
    height = 900
  )
  # FEATURE SET 1
  if (n1 == 1) {
    # Colors
    boxplots_colors <- c(
      rep("darkolivegreen1", 6),
      rep("darkgoldenrod1" , 6),
      rep("burlywood"      , 6),
      rep("cadetblue1"     , 6)
    )
    # Plot cluster contents (features 1 to 24)
    par(fig=c(0,0.9,0,1))
    par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
    column_names <- feats_set[[n1]][1:24]
    short_labels <- substr(column_names, 10, 18)
    cluster_elems <- w_feats[cluster_idx,]
    cluster_elems <- subset(cluster_elems, select = column_names)
    boxplot(cluster_elems, las=2, names=short_labels, col=boxplots_colors)
    # Plot cluster contents (feature 25)
    par(fig=c(0.9,1,0,1), new=TRUE)
    par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
    column_names <- feats_set[[n1]][25]
    short_labels <- substr(column_names, 10, 16)
    cluster_elems <- w_feats[cluster_idx,]
    cluster_elems <- subset(cluster_elems, select = column_names)
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

################################################################################
##  LOAD FEATURES
################################################################################

# Load feats
feats <- data.table::fread(
  file   = feats_path,
  header = TRUE,
  sep    = ","
)

number_of_clusters <- 24

################################################################################
##  LOOP
################################################################################
fnames <- list.files(path = paste0(root_dir, "data/"), pattern = "*.clValid2")

# # Setup parallel backend to use many processors
# cores <- parallel::detectCores() - 1
# cl <- parallel::makeCluster(cores, outfile = "")
# doParallel::registerDoParallel(cl)

# Dataset loop
# Da muchos problemas si se paraleliza porque los png() y los dev.off() se 
# entremezclan!
for (ff in 1:length(fnames)) {
  for (cc in 1:number_of_clusters) {
  
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
    
    # Working features
    w_feats <- feats[row_conditions,]
    w_fpath <- paste0(root_dir, "data/", w_fname)
    
    if (file.exists(w_fpath)) {
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
}

# # Stop parallelization
# parallel::stopCluster(cl)
