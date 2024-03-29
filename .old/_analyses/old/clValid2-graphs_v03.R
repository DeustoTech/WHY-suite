library(foreach)
library(stringr)

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
  par(fig=c(0,1,0,1), cex=1)
  barplot(pc_acorn, col=acorn_colors, ylim=c(0,1))
  
  # Save file
  dev.off()
}

##############################################################################
##  get_iss_graphs()
##############################################################################
get_iss_graphs <- function() {
  # Create dir if it does not exist
  if (!dir.exists(paste0(root_dir, "iss/"))) {
    dir.create(paste0(root_dir, "iss/"))
  }
  
  # QUESTION 6103 - FLOOR AREA
  floor_area <- feats$q_6103
  floor_area[floor_area > 1E6] <- NA
  idx <- feats$q_61031 == 2
  idx[is.na(idx)] <- FALSE
  floor_area[idx] <- floor_area[idx] * 0.09290304
  
  # QUESTION 453 - YEAR HOUSE BUILT
  w_year <- as.numeric(feats$q_453)
  idx <- w_year > 9000 | w_year < 100
  idx[is.na(idx)] <- FALSE
  w_year[idx] <- NA
  
  ### GRAPH 1 ###
  iss_name <- paste0("iss_", n1, n2, n3, "_", cc, "-", number_of_clusters, "_1.png")
  # Open png file
  png(
    paste0(root_dir, "iss/", iss_name),
    width = 1200,
    height = 900
  )
  yes_no_col <- c("darkgreen", "darkred")
  yes_no_mar <- c(4.0,4.1,2.0,2.1)
  # Plot 1.1
  par(fig=c(0,8/23,5/6,1), cex=1.0, mar=yes_no_mar)
  res_200 <- c(sum(feats$q_200[cluster_list == cc] == 1, na.rm = T),
               sum(feats$q_200[cluster_list == cc] == 2, na.rm = T))
  try(barplot(res_200, names= c("m", "f"), xlab=200, horiz=T, las=2))
  # Plot 1.2
  par(fig=c(0,8/23,4/6,5/6), cex=1.0, mar=yes_no_mar, new=TRUE)
  res_47001 <- c(sum(feats$q_47001[cluster_list == cc] == 1, na.rm = T),
                 sum(feats$q_47001[cluster_list == cc] == 2, na.rm = T))
  try(barplot(res_47001, names= c("y", "n"), xlab=47001, horiz=T, las=2, col=yes_no_col))
  # Plot 1.3
  par(fig=c(0,8/23,3/6,4/6), cex=1.0, mar=yes_no_mar, new=TRUE)
  res_47011 <- c(sum(feats$q_47011[cluster_list == cc] == 1, na.rm = T),
                 sum(feats$q_47011[cluster_list == cc] == 2, na.rm = T))
  try(barplot(res_47011, names= c("y", "n"), xlab=47011, horiz=T, las=2, col=yes_no_col))
  # Plot 1.4
  par(fig=c(0,8/23,2/6,3/6), cex=1.0, mar=yes_no_mar, new=TRUE)
  res_4801 <- c(sum(feats$q_4801[cluster_list == cc] == 1, na.rm = T),
                sum(feats$q_4801[cluster_list == cc] == 2, na.rm = T))
  try(barplot(res_4801, names= c("y", "n"), xlab=4801, horiz=T, las=2, col=yes_no_col))
  # Plot 1.5
  par(fig=c(0,8/23,1/6,2/6), cex=1.0, mar=yes_no_mar, new=TRUE)
  res_471 <- c(sum(feats$q_471[cluster_list == cc] == 1, na.rm = T),
               sum(feats$q_471[cluster_list == cc] == 2, na.rm = T))
  try(barplot(res_471, names= c("y", "n"), xlab=471, horiz=T, las=2, col=yes_no_col))
  # Plot 1.6
  par(fig=c(0,8/23,0,1/6), cex=1.0, mar=yes_no_mar, new=TRUE)
  res_473 <- c(sum(feats$q_473[cluster_list == cc] == 1, na.rm = T),
               sum(feats$q_473[cluster_list == cc] == 2, na.rm = T))
  try(barplot(res_473, names= c("y", "n"), xlab=473, horiz=T, las=2, col=yes_no_col))
  
  # Plot 2
  par(fig=c(1/23*8,1/23*13,0,1), cex=1.0, new=TRUE)
  try(boxplot(feats$q_420[cluster_list == cc], replace(feats$q_430[cluster_list == cc], feats$q_430[cluster_list == cc] == 8, 0), feats$q_4551[cluster_list == cc], las=2, names= c(420, 430, 4551)))
  # Plot 3
  par(fig=c(1/23*13,1/23*17,0,1), cex=1.0, new=TRUE)
  try(boxplot(feats$q_43521[cluster_list == cc], feats$q_4531[cluster_list == cc], las=2, names= c(43521, 4531)))
  # Plot 4
  par(fig=c(1/23*17,1/23*20,0,1), cex=1.0, new=TRUE)
  try(boxplot(w_year[cluster_list == cc], las=2, xlab= c(453)))
  # Plot 5
  par(fig=c(1/23*20,1/23*23,0,1), cex=1.0, new=TRUE)
  try(boxplot(floor_area[cluster_list == cc], las=2, xlab= c(6103)))
  # Save file
  dev.off()
  
  ### GRAPH 2 ###
  iss_name <- paste0("iss_", n1, n2, n3, "_", cc, "-", number_of_clusters, "_2.png")
  
  # Open png file
  png(
    paste0(root_dir, "iss/", iss_name),
    width = 1200,
    height = 900
  )
  # Plot 1
  par(fig=c(1/40*0,1/40*6,0,1), cex=1.0)
  res_410 <- c(sum(feats$q_410[cluster_list == cc] == 1, na.rm = T),
               sum(feats$q_410[cluster_list == cc] == 2, na.rm = T),
               sum(feats$q_410[cluster_list == cc] == 3, na.rm = T))
  try(barplot(res_410, names= 1:3, xlab=410))
  # Plot 2
  par(fig=c(1/40*6,1/40*13,0,1), cex=1.0, new=TRUE)
  res_4321 <- c(sum(feats$q_4321[cluster_list == cc], na.rm = T),
                sum(feats$q_4321.1[cluster_list == cc], na.rm = T),
                sum(feats$q_4321.2[cluster_list == cc], na.rm = T),
                sum(feats$q_4321.3[cluster_list == cc], na.rm = T))
  try(barplot(res_4321, names= 1:4, xlab=4321))
  # Plot 3
  par(fig=c(1/40*13,1/40*22,0,1), cex=1.0, new=TRUE)
  res_450 <- c(sum(feats$q_450[cluster_list == cc] == 1, na.rm = T),
               sum(feats$q_450[cluster_list == cc] == 2, na.rm = T),
               sum(feats$q_450[cluster_list == cc] == 3, na.rm = T),
               sum(feats$q_450[cluster_list == cc] == 4, na.rm = T),
               sum(feats$q_450[cluster_list == cc] == 5, na.rm = T),
               sum(feats$q_450[cluster_list == cc] == 6, na.rm = T))
  try(barplot(res_450, names= 1:6, xlab=450))
  # Plot 4
  par(fig=c(1/40*22,1/40*30,0,1), cex=1.0, new=TRUE)
  res_452 <- c(sum(feats$q_452[cluster_list == cc] == 1, na.rm = T),
               sum(feats$q_452[cluster_list == cc] == 2, na.rm = T),
               sum(feats$q_452[cluster_list == cc] == 3, na.rm = T),
               sum(feats$q_452[cluster_list == cc] == 4, na.rm = T),
               sum(feats$q_452[cluster_list == cc] == 5, na.rm = T))
  try(barplot(res_452, names= 1:5, xlab=452))
  # Plot 5
  par(fig=c(1/40*30,1/40*40,0,1), cex=1.0, new=TRUE)
  res_470 <- c(sum(feats$q_470[cluster_list == cc], na.rm = T),
                sum(feats$q_470.1[cluster_list == cc], na.rm = T),
                sum(feats$q_470.2[cluster_list == cc], na.rm = T),
                sum(feats$q_470.3[cluster_list == cc], na.rm = T),
                sum(feats$q_470.4[cluster_list == cc], na.rm = T),
                sum(feats$q_470.5[cluster_list == cc], na.rm = T),
                sum(feats$q_470.6[cluster_list == cc], na.rm = T))
  try(barplot(res_470, names= 1:7, xlab=470))
  #Save file
  dev.off()
  
  ### GRAPH 3 ###
  iss_name <- paste0("iss_", n1, n2, n3, "_", cc, "-", number_of_clusters, "_3.png")
  
  # Open png file
  png(
    paste0(root_dir, "iss/", iss_name),
    width = 1200,
    height = 900
  )
  # Plot 1
  par(fig=c(0,11/42,0,1), cex=1.0)
  res_4701 <- c(sum(feats$q_4701[cluster_list == cc], na.rm = T),
               sum(feats$q_4701.1[cluster_list == cc], na.rm = T),
               sum(feats$q_4701.2[cluster_list == cc], na.rm = T),
               sum(feats$q_4701.3[cluster_list == cc], na.rm = T),
               sum(feats$q_4701.4[cluster_list == cc], na.rm = T),
               sum(feats$q_4701.5[cluster_list == cc], na.rm = T),
               sum(feats$q_4701.6[cluster_list == cc], na.rm = T),
               sum(feats$q_4701.7[cluster_list == cc], na.rm = T))
  try(barplot(res_4701, names= 1:8, xlab=4701))
  # Plot 2
  par(fig=c(11/42,18/42,0,1), cex=1.0, new=TRUE)
  res_472 <- c(sum(feats$q_472[cluster_list == cc], na.rm = T),
                sum(feats$q_472.1[cluster_list == cc], na.rm = T),
                sum(feats$q_472.2[cluster_list == cc], na.rm = T),
                sum(feats$q_472.3[cluster_list == cc], na.rm = T))
  try(barplot(res_472, names= 1:4, xlab=472))
  # Plot 3
  par(fig=c(18/42,24/42,0,1), cex=1.0, new=TRUE)
  res_455 <- c(sum(feats$q_455[cluster_list == cc] == 1, na.rm = T),
               sum(feats$q_455[cluster_list == cc] == 2, na.rm = T),
               sum(feats$q_455[cluster_list == cc] == 3, na.rm = T))
  try(barplot(res_455, names= 1:3, xlab=455))
  # Plot 4
  par(fig=c(24/42,33/42,0,1), cex=1.0, new=TRUE)
  res_5418 <- c(sum(feats$q_5418[cluster_list == cc] == 1, na.rm = T),
                sum(feats$q_5418[cluster_list == cc] == 2, na.rm = T),
                sum(feats$q_5418[cluster_list == cc] == 3, na.rm = T),
                sum(feats$q_5418[cluster_list == cc] == 4, na.rm = T),
                sum(feats$q_5418[cluster_list == cc] == 5, na.rm = T),
                sum(feats$q_5418[cluster_list == cc] == 6, na.rm = T))
  try(barplot(res_5418, names= 1:6, xlab=5418))
  # Plot 5
  par(fig=c(33/42,1,0,1), cex=1.0, new=TRUE)
  res_4021 <- c(sum(feats$q_4021[cluster_list == cc] == 1, na.rm = T),
                sum(feats$q_4021[cluster_list == cc] == 2, na.rm = T),
                sum(feats$q_4021[cluster_list == cc] == 3, na.rm = T),
                sum(feats$q_4021[cluster_list == cc] == 4, na.rm = T),
                sum(feats$q_4021[cluster_list == cc] == 5, na.rm = T),
                sum(feats$q_4021[cluster_list == cc] == 6, na.rm = T))
  try(barplot(res_4021, names= 1:6, xlab=4021))
  #Save file
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
  prov_cluster <- c()
  for (ii in 1:length(provinces)) {
    prov_cluster[ii] <- sum(go2_feats$administrative_division[cluster_list == cc] == provinces[ii])
  }
  
  # Get percentages of acorn representation
  pc_prov <- prov_cluster
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
  par(fig=c(0,1,0,1), cex=1.0)
  plot_data <- sort(pc_prov[pc_prov != 0], decreasing = TRUE)[1:50]
  barplot(plot_data[!is.na(plot_data)], las=2)
  
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
    cnae_cluster[ii] <- sum(go2_feats$cnae[cluster_list == cc] == cnae[ii])
  }
  
  # Get percentages of acorn representation
  pc_cnae <- cnae_cluster
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
  par(fig=c(0,1,0,1), cex=1.0)
  plot_data <- sort(pc_cnae[pc_cnae != 0], decreasing = TRUE)[1:50]
  barplot(plot_data[!is.na(plot_data)], las=2)
  
  # Save file
  dev.off()
  
  ###############
  ##  TARIFFS  ##
  ###############

  tariff_names <- paste0("goi_", n1, n2, n3, "_", cc, "-", number_of_clusters, "_3.png")
  
  # Open png file
  png(
    paste0(root_dir, "goi/", tariff_names),
    width = 1200,
    height = 900
  )
  # Plot 1
  par(fig=c(0,11/17,0,1), cex=1.0)
  tariffs <-c(sum(feats$tariff[cluster_list == cc] == "2.0A",   na.rm = T),
              sum(feats$tariff[cluster_list == cc] == "2.0DHA", na.rm = T),
              sum(feats$tariff[cluster_list == cc] == "2.0DHS", na.rm = T),
              sum(feats$tariff[cluster_list == cc] == "2.1A"  , na.rm = T),
              sum(feats$tariff[cluster_list == cc] == "2.1DHA", na.rm = T),
              sum(feats$tariff[cluster_list == cc] == "2.1DHS", na.rm = T),
              sum(feats$tariff[cluster_list == cc] == "3.0A",   na.rm = T),
              sum(feats$tariff[cluster_list == cc] == "3.1A",   na.rm = T))
  try(barplot(tariffs, names= c("2.0A", "2.0DHA", "2.0DHS", "2.1A", "2.1DHA", "2.1DHS", "3.0A", "3.1A")))
  # Plot 2
  par(fig=c(11/17,1,0,1), cex=1.0, new=TRUE)
  try(boxplot(feats$p1_kw[cluster_list == cc], 
              feats$p2_kw[cluster_list == cc], 
              feats$p3_kw[cluster_list == cc],
              names= c("p1_kw", "p2_kw", "p3_kw")))
  #Save file
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
    par(fig=c(0,27/31,0,1), cex=1)
    column_names <- feats_set[[n1]][1:24]
    short_labels <- substr(column_names, 10, 18)
    cluster_elems <- w_feats[cluster_idx,]
    cluster_elems <- subset(cluster_elems, select = column_names)
    boxplot(cluster_elems, las=2, names=short_labels, col=boxplot_colors)
    # Plot cluster contents (feature 25)
    par(fig=c(27/31,1,0,1), cex=1, new=TRUE)
    column_names <- feats_set[[n1]][25]
    short_labels <- substr(column_names, 10, 16)
    cluster_elems <- w_feats[cluster_idx,]
    cluster_elems <- subset(cluster_elems, select = column_names)
    boxplot(cluster_elems, las=2, xlab=short_labels, col="darksalmon")
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

##############################################################################
##  GLOBAL ENVIRONMENT
##############################################################################

##  PATHS
# User defined variables
if (.Platform$OS.type == "windows") {
  feats_path <- "G:/Mi unidad/WHY/Features/feats_v1.15.csv"
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

##  COLORS
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

##  LOAD FEATURES
# Load feats
feats <- data.table::fread(
  file   = feats_path,
  header = TRUE,
  sep    = ","
)

##  LOOP
fnames <- list.files(path = paste0(root_dir, "data/"), pattern = "*.clValid2")

numel_df <- data.frame()

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
    
    if (skip_xxx2x & n4 != 2) {
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
          # get_go2_graphs()
        }
        # LCL
        if (n2 == 2) {
          # get_lcl_graphs()
        }
        # ISS
        if (n2 == 3) {
          # get_iss_graphs()
        }
  
        # Number of elements per cluster
        new_df <- data.frame(
          n1    = n1,
          n2    = n2,
          n3    = n3,
          cc    = cc,
          numel = sum(cluster_idx),
          pctel = sum(cluster_idx) / sum(row_conditions)
        )
        numel_df <- rbind(numel_df, new_df)
      }
    }
  }
}

# Save
data.table::fwrite(
  x         = numel_df,
  file      = paste0(root_dir, "numel_df.csv"),
  append    = F,
  quote     = F,
  sep       = ",",
  row.names = F,
  col.names = T,
  dateTimeAs = "write.csv"
)