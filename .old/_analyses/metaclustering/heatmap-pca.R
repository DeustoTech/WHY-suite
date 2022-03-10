heatmap_pca <- function() {
  heatmap_dir <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.05.15_3-cl-methods-hmp-scaled/hmm/"
  df <- data.frame()
  row_names <- c()
  for (dd in 1:4) {
    for (mm in c(2,5)) {
      # Sort values
      order_vect <- numel_df$numel[numel_df$n2 == dd & numel_df$n3 == mm]
      cc_idx <- sort(order_vect, index.return=TRUE, decreasing = TRUE)

      for (cc in 1:24) {
        heatmap_fname <- paste0("hmm_1", dd, mm, "_", cc_idx$ix[cc], "-24.RData")
        print(heatmap_fname)
        load(paste0(heatmap_dir, heatmap_fname))
        m <- as.vector(m)
        m <- (m - min(m))/(max(m) - min(m))
        df <- rbind(df, m)
        row_names <- c(row_names, paste(dd, mm, cc, sep="-"))
      }
    }
  }
  row.names(df) <- row_names
  return(df)
}

get_row_names <- function() {
  numel_df_path <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.05.15_3-cl-methods-hmp-scaled/numel_df.csv"
  
  numel_df <- data.table::fread(
    file   = numel_df_path,
    header = TRUE,
    sep    = ","
  )
  
  return(numel_df)
}

numel_df <- get_row_names()
df <- heatmap_pca()

# library(ggfortify)
# pca_res <- prcomp(df, scale. = TRUE)
# autoplot(pca_res, data = df, shape = FALSE, label.size = 2)

library(Rtsne)
tsne_out <- Rtsne(df, dims = 2, perplexity=1.0, verbose=TRUE, max_iter = 5000)
png(
  "G:/Mi unidad/WHY/Github/why-T2.1/_analyses/heatmap-tsne.png",
  width = 10000,
  height = 10000
)
plot(tsne_out$Y, col="white")
text(tsne_out$Y, labels=row.names(df), cex=2)
#Save file
dev.off()

dist_vect <- dist(tsne_out$Y)
hclust_obj <- hclust(dist_vect)