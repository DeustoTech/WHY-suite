library(foreach)
library(stringr)

##############################################################################
##  PATHS
##############################################################################

# User defined variables
if (.Platform$OS.type == "windows") {
  feats_path <- "G:/Mi unidad/WHY/Features/feats_v1.12.csv"
  root_dir   <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.05.05_3-cl-methods/"
  source("G:/Mi unidad/WHY/Github/why-T2.1/_analyses/selectable_variables.R")
}
if (.Platform$OS.type == "unix") {
  feats_path <- "/home/ubuntu/carlos.quesada/disk/features/feats_v1.12.csv"
  root_dir   <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2021.05.05_3-cl-methods/"
  source("/home/ubuntu/carlos.quesada/analyses/selectable_variables.R")
}

number_of_clusters <- 24
skip_xxx2x <- TRUE

##############################################################################
##  COLORS
##############################################################################

boxplot_colors <- c(
  rep("darkolivegreen1", 6),
  rep("darkgoldenrod1" , 6),
  rep("burlywood"      , 6),
  rep("cadetblue1"     , 6)
)

acorn_colors <- c(
  rep("cornflowerblue", 3),
  rep("darkorchid1", 2),
  rep("darkolivegreen3", 5),
  rep("darkgoldenrod1", 4),
  rep("aquamarine", 3)
)

##############################################################################
##  get_lcl_graphs()
##############################################################################
get_lcl_graphs <- function() {
  # Create dir if it does not exist
  if (!dir.exists(paste0(root_dir, "lcl/"))) {
    dir.create(paste0(root_dir, "lcl/"))
  }
  
  # Retrieve ACORN values
  acorn <- subset(
    x      = feats,
    subset = row_conditions,
    select = "acorn"
  ) 
  
  # Count letters
  acorn_codes <- LETTERS[1:17]
  acorn_all <- c()
  acorn_cluster <- c()
  for (ii in 1:length(acorn_codes)) {
    acorn_all[ii] <- sum(acorn == acorn_codes[ii])
    acorn_cluster[ii] <- sum(acorn[cluster_list == cc] == acorn_codes[ii])
  }
  
  # Get percentages of acorn representation
  pc_acorn <- acorn_cluster / acorn_all
  names(pc_acorn) <- acorn_codes
  
  # Name of the file to create
  ft_fname <- paste0("lcl_", n1, n2, n3, "_", cc, "-", number_of_clusters, ".png")
  # Open png file
  png(
    paste0(root_dir, "lcl/", ft_fname),
    width = 1200,
    height = 900
  )
  # Plot cluster contents (features 1 to 24)
  par(fig=c(0,1,0,1))
  par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
  barplot(pc_acorn, col=acorn_colors, ylim=c(0,1))
  
  # Save file
  dev.off()
}

##############################################################################
##  get_go2_graphs()
##############################################################################
get_go2_graphs <- function() {
  # Create dir if it does not exist
  if (!dir.exists(paste0(root_dir, "goi/"))) {
    dir.create(paste0(root_dir, "goi/"))
  }
  
  # Retrieve ACORN values
  go2_feats <- subset(
    x      = feats,
    subset = row_conditions,
    select = c("administrative_division", "municipality", "zip_code", "cnae")
  ) 
  
  #################
  ##  PROVINCES  ##
  #################
  
  # Count provinces
  provinces <- sort(unique(go2_feats$administrative_division))
  # prov_all <- c()
  prov_cluster <- c()
  for (ii in 1:length(provinces)) {
    # prov_all[ii] <- sum(go2_feats$administrative_division == provinces[ii])
    prov_cluster[ii] <- sum(go2_feats$administrative_division[cluster_list == cc] == provinces[ii])
  }
  
  # Get percentages of acorn representation
  pc_prov <- prov_cluster #/ prov_all
  names(pc_prov) <- provinces
  
  # Name of the file to create
  prov_fname <- paste0("goi_", n1, n2, n3, "_", cc, "-", number_of_clusters, "_1.png")
  
  # Open png file
  png(
    paste0(root_dir, "goi/", prov_fname),
    width = 1200,
    height = 900
  )
  # Plot cluster contents (features 1 to 24)
  par(fig=c(0,1,0,1))
  par(cex=1.0, mai=c(0.8,0.5,0.05,0.05))
  barplot(sort(pc_prov[pc_prov != 0], decreasing = TRUE)[1:50], las=2)
  
  # Save file
  dev.off()
  
  ############
  ##  CNAE  ##
  ############
  
  # Count cnae's
  cnae <- sort(unique(go2_feats$cnae))
  # cnae_all <- c()
  cnae_cluster <- c()
  for (ii in 1:length(cnae)) {
    # cnae_all[ii] <- sum(go2_feats$cnae == cnae[ii])
    cnae_cluster[ii] <- sum(go2_feats$cnae[cluster_list == cc] == cnae[ii])
  }
  
  # Get percentages of acorn representation
  pc_cnae <- cnae_cluster #/ cnae_all
  names(pc_cnae) <- stringr::str_pad(cnae, 4, pad="0")
  
  # Name of the file to create
  cnae_fname <- paste0("goi_", n1, n2, n3, "_", cc, "-", number_of_clusters, "_2.png")
  
  # Open png file
  png(
    paste0(root_dir, "goi/", cnae_fname),
    width = 1200,
    height = 900
  )
  # Plot cluster contents (features 1 to 24)
  par(fig=c(0,1,0,1))
  par(cex=1.0, mai= c(0.8,0.5,0.05,0.05))
  barplot(sort(pc_cnae[pc_cnae != 0], decreasing = TRUE)[1:50], las=2)
  
  # Save file
  dev.off()
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
  # FEATURE SET 1
  if (n1 == 1) {
    # Save as PNG
    png(
      paste0(root_dir, "graph/", ft_fname),
      width = 1200,
      height = 900
    )
    # Plot cluster contents (features 1 to 24)
    par(fig=c(0,0.9,0,1))
    par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
    column_names <- feats_set[[n1]][1:24]
    short_labels <- substr(column_names, 10, 18)
    cluster_elems <- w_feats[cluster_idx,]
    cluster_elems <- subset(cluster_elems, select = column_names)
    boxplot(cluster_elems, las=2, names=short_labels, col=boxplot_colors)
    # Plot cluster contents (feature 25)
    par(fig=c(0.9,1,0,1), new=TRUE)
    par(cex=0.7, mai=c(0.8,0.5,0.05,0.05))
    column_names <- feats_set[[n1]][25]
    short_labels <- substr(column_names, 10, 16)
    cluster_elems <- w_feats[cluster_idx,]
    cluster_elems <- subset(cluster_elems, select = column_names)
    boxplot(cluster_elems, las=2, names=short_labels)
    # Save file
    dev.off()
  }
  # REST OF FEATURE SETS
  if (n1 != 1) {
    # Save as PNG
    png(
      paste0(root_dir, "graph/", ft_fname),
      width = 1200,
      height = 900
    )
    # Plot cluster contents (features 1 to 7)
    par(fig=c(0,1,0,1))
    par(cex=0.9, mai=c(0.8,0.5,0.05,0.05))
    cluster_elems <- w_feats[km$cluster == ii,]
    # short_labels <- substr(names(cluster_elems), 10, 18)
    boxplot(cluster_elems, las=2)
    # Save file
    dev.off()
  }
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

################################################################################
##  LOOP
################################################################################
fnames <- list.files(path = paste0(root_dir, "data/"), pattern = "*.clValid2")

# Setup parallel backend to use many processors
cores <- parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

# Dataset loop
# Da muchos problemas si se paraleliza porque los png() y los dev.off() se 
# entremezclan!
o <- foreach::foreach (ff = 1:length(fnames)) %:%
  foreach::foreach (cc = 1:number_of_clusters) %dopar% {
  
    w_fname <- fnames[ff]
    print(paste0(w_fname, " - ", cc))
    
    # Extract data from filename
    n1 <- as.numeric(substr(w_fname, 3, 3)) # features
    n2 <- as.numeric(substr(w_fname, 4, 4)) # datasets
    n3 <- as.numeric(substr(w_fname, 5, 5)) # cluster method
    n4 <- as.numeric(substr(w_fname, 6, 6)) # validation method
    n5 <- as.numeric(substr(w_fname, 7, 7)) # cluster sequence
    
    ### REMOVE THIS
    if (n2 != 4) next
    
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
      # get_feature_graphs()
      
      # GOI & MEG
      if (n2 == 1 | n2 == 4) {
        get_go2_graphs()
      }
      # LCL
      if (n2 == 2) {
        get_lcl_graphs()
      }
      # ISS
      if (n2 == 3) {
      }
      
    }
}


# Stop parallelization
parallel::stopCluster(cl)
