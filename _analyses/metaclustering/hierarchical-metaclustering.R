### COMPUTE t-SNE FROM HEATMAP MATRICES


hier_metaclust <- function() {
  # library(Rtsne)
  
  # Heatmap dir
  # hm_dir <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.06.08_km-som-var-cl/hmm/"
  hm_dir <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.06.08_km-som-var-cl/hmm/"
  # Output path
  out_dir <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.06.08_km-som-var-cl/dendro/"
  # Set of features
  for (ff in 1) {
    # Dataset
    for (dd in c(2:5,7)) {
      # Cluster method
      for (mm in c(2,5)) {
        # Number of clusters
        if (dd == 2) nclust <- 16 # LCL 16 cl
        if (dd == 3) nclust <- 16 # ISS 16 cl
        if (dd == 4) nclust <- 30 # GOI 30 cl
        if (dd == 5) nclust <-  6 # POR  6 cl
        if (dd == 7) nclust <- 40 # all 40 cl
        
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
        #Save file
        png(
          paste0(out_dir, "dendro_", ff, dd, mm, ".png"),
          width  = 1200,
          height = 900
        )
        plot(hier)
        dev.off()
      }
    }
  }
  
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

hier_metaclust()