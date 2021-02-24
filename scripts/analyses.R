#' #############################################################################
#' ##                                                                         ##
#' ##                        ANALYSES OF FEATURES                             ##
#' ##                                                                         ##
#' #############################################################################

library(whyT2.1)

################################################################################
analysis_val <- 1
feat_vers <- "feats_v1.03.csv"
################################################################################

analysis_fn <- function(analysis_val, feats) {
  
  ### ANALYSIS 1
  if (analysis_val == 1) {
    # Type of analysis
    mode = "kmeans"
    # Number of centers
    num_centers <- 6
    # Features to be analyzed
    column_subset <- c(
      "rel_mean_00h04hspr", "rel_mean_04h08hspr", "rel_mean_08h12hspr",
      "rel_mean_12h16hspr", "rel_mean_16h20hspr", "rel_mean_20h00hspr",
      "rel_mean_00h04hsum", "rel_mean_04h08hsum", "rel_mean_08h12hsum",
      "rel_mean_12h16hsum", "rel_mean_16h20hsum", "rel_mean_20h00hsum",
      "rel_mean_00h04haut", "rel_mean_04h08haut", "rel_mean_08h12haut",
      "rel_mean_12h16haut", "rel_mean_16h20haut", "rel_mean_20h00haut",
      "rel_mean_00h04hwin", "rel_mean_04h08hwin", "rel_mean_08h12hwin",
      "rel_mean_12h16hwin", "rel_mean_16h20hwin", "rel_mean_20h00hwin"
    )
    # Retrieve data
    retr_data <- subset(
      x      = feats,
      subset = 
        data_set == "lcl" & is_household == 1 & total_imputed_pct < 2/3,
      select = column_subset
    )
    
    if (mode == "nbclust") {
      library(NbClust)
      
      o <- NbClust(
        data = retr_data,
        method = "kmeans"
      )
      
      browser()
    }
    
    if (mode == "tsne") {
      library(Rtsne)
      tsne_results <- Rtsne(
        retr_data,
        perplexity = 30,
        check_duplicates = FALSE,
        pca_scale = TRUE
      ) 
      browser()
      plot(tsne_results$Y)
    }
    
    get_tot_withinss <- function()
    
    if (mode == "kmeans") {
      # K-means
      res_km <- stats::kmeans(retr_data, num_centers)
      
      retr_data_dist <- dist(retr_data)
      
      silh <- cluster::silhouette(x = res_km$cluster, dist = retr_data_dist)
      
      
      
      browser()

      # Plot
      # f <- factoextra::fviz_cluster(
      #   res_km, 
      #   data            = retr_data,
      #   show.clust.cent = FALSE,
      #   geom            = "point",
      #   pointsize       = 1
      # )
      # plot(f)
      # Barplot
      # for (cc in 1:num_centers) {
      #   boxplot(
      #     res_km$centers[cc,],
      #     col=c(
      #       rep("lightgreen", 6), 
      #       rep("lightgoldenrod", 6), 
      #       rep("rosybrown", 6), 
      #       rep("lightblue1", 6)
      #     ),
      #     main = paste("Cluster #", cc, " - ", sum(res_km$cluster == cc), " elements", sep="")
      #   )
      # }
    }
  }
  
  ### ANALYSIS 2
  if (analysis_val == 2) {
    print("Hi!")
  }
  
  ### ANALYSIS 3
  if (analysis_val == 3) {
    print("Hi!")
  }
}

################################################################################

# Fix random numbers
set.seed(1981)
# Folder of the file of features
feat_dir  <- "G:/Mi unidad/WHY/Datasets/@FEATURES/"
# Path to the file of features
feat_path <- paste(feat_dir, feat_vers, sep="")
# Load feats
feats <- data.table::fread(
  file   = feat_path,
  header = TRUE,
  sep    = ","
)
# Call to the analysis function
analysis_out <- analysis_fn(analysis_val, feats)