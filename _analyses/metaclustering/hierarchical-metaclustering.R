### COMPUTE t-SNE FROM HEATMAP MATRICES


tsne_metaclust <- function() {
  library(Rtsne)
  
  # Heatmap dir
  # hm_dir <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.06.08_km-som-var-cl/hmm/"
  hm_dir <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.05.15_3-cl-methods-hmp-scaled/hmm/"
  # Output path
  out_dir <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.05.15_3-cl-methods-hmp-scaled/tsne/"
  # Set of features
  ff <- 1
  # Dataset
  dd <- 4
  # Cluster method
  mm <- 5
  # Number of clusters
  nclust <- 24
  
  df <- data.frame()
  row_names <- c()
  
  for (cc in 1:nclust) {
    # Heatmap file name
    hm_fname <- paste0("hmm_", ff, dd, mm, "_", cc, "-", nclust, ".RData")
    print(hm_fname)
    # Load heatmap
    load(paste0(hm_dir, hm_fname))
    
    # Assemble the heatmaps in a dataframe
    m <- as.vector(m)
    m <- (m - min(m))/(max(m) - min(m))
    # m <- as.vector(scale(m))
    df <- rbind(df, m)
  }
  
  # Name the rows
  row.names(df) <- 1:nclust
  
  # COMPUTE HIERARCHICAL
  hier <- hclust(dist(df))
  plot(hier)
  
  # # COMPUTE t-SNE
  # tsne_out <- Rtsne(df, dims = 2, perplexity=2, verbose=TRUE, max_iter = 5000)
  # 
  # #Save file
  # png(
  #   paste0(out_dir, ff, dd, mm, ".png"),
  #   width  = 1200,
  #   height = 900
  # )
  # plot(tsne_out$Y, col="white")
  # text(tsne_out$Y, labels=row.names(df), cex=1)
  # dev.off()
}

tsne_metaclust()