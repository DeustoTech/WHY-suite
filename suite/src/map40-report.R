# MIDE DISTANCIAS ENTRE SETS DE CLUSTERS Y RELACIONA LAS MAS
# PARECIDAS ENTRE SI

map40 <- function(
  new_hmp_dir,
  new_hmp_fname,
  all_hmp_dir,
  all_hmp_fname,
  out_dir,
  tag = "PRE"
) {
  
  set.seed(1981)
  
  pre_path <- new_hmp_dir
  all_path <- all_hmp_dir
  out_path <- paste0(out_dir, "map40/")
  
  if (!dir.exists(out_path)) dir.create(out_path)
  
  # Cuantos ficheros hay con cada formato
  num_new_hmp <- length(
    list.files(
      path    = pre_path,
      pattern = paste0("^", new_hmp_fname[1]) # All strings starting with
    )
  )
  
  df1 <- data.frame(matrix(ncol = 8904, nrow = num_new_hmp+40))
  for (ii in 1:num_new_hmp) {
    load(paste0(pre_path, new_hmp_fname[1], ii, new_hmp_fname[2]))
    if (!exists("m")) m <- m_avg
    df1[ii,] <- as.vector(m) -min(m)/(max(m)-min(m))
    rm(m)
  }
  for (ii in 1:40) {
    load(paste0(all_path, all_hmp_fname[1], ii, all_hmp_fname[2]))
    if (!exists("m")) m <- m_avg
    df1[ii+num_new_hmp,] <- as.vector(m) -min(m)/(max(m)-min(m))
    rm(m)
  }
  
  # Matriz de distancias
  dist_df1 <- dist(df1)
  
  # Cogemos solo la parte que nos interesa
  m1_dist <- as.matrix(dist_df1)[1:num_new_hmp, (num_new_hmp+1):(num_new_hmp+40)]
  
  final_df <- data.frame()
  for (ii in 1:num_new_hmp) {
    min1_idx1 <- Rfast::nth(m1_dist[ii,], 1, descending = F, index.return = T)
    min1_idx2 <- Rfast::nth(m1_dist[ii,], 2, descending = F, index.return = T)
    min1_idx3 <- Rfast::nth(m1_dist[ii,], 3, descending = F, index.return = T)
    
    min1_val1 <- Rfast::nth(m1_dist[ii,], 1, descending = F, index.return = F)
    min1_val2 <- Rfast::nth(m1_dist[ii,], 2, descending = F, index.return = F)
    min1_val3 <- Rfast::nth(m1_dist[ii,], 3, descending = F, index.return = F)
    
    aux_df <- data.frame(
      idx         = ii,
      PRE_clust_1 = min1_idx1,
      PRE_clust_2 = min1_idx2,
      PRE_clust_3 = min1_idx3,
      PRE_value_1 = min1_val1,
      PRE_value_2 = min1_val2,
      PRE_value_3 = min1_val3
    )
    
    row.names(aux_df)<- ii
    
    final_df <- rbind(final_df, aux_df)
  }
  
  ###########
  ##  PRE  ##
  ###########

  for (ii in 1:num_new_hmp) {
    pdf(file = paste0(out_path, sprintf("%02d", ii), ".pdf"), paper="a4r")

    par(mfrow = c(2,2))
    image(
      t(matrix(as.numeric(df1[ii,]), nrow=24, ncol=371)),
      axes=FALSE,
      frame.plot=TRUE,
      xlab = paste0(tag, " #", ii)
    )
    image(
      t(matrix(as.numeric(df1[final_df$PRE_clust_1[ii]+num_new_hmp,]), nrow=24, ncol=371)),
      axes=FALSE,
      frame.plot=TRUE,
      xlab = paste0("Possible ALL #", final_df$PRE_clust_1[ii],
                    ", dist = ", round(final_df$PRE_value_1[ii],2))
    )
    image(
      t(matrix(as.numeric(df1[final_df$PRE_clust_2[ii]+num_new_hmp,]), nrow=24, ncol=371)),
      axes=FALSE,
      frame.plot=TRUE,
      xlab = paste0("Possible ALL #", final_df$PRE_clust_2[ii],
                    ", dist = ", round(final_df$PRE_value_2[ii],2))
    )
    image(
      t(matrix(as.numeric(df1[final_df$PRE_clust_3[ii]+num_new_hmp,]), nrow=24, ncol=371)),
      axes=FALSE,
      frame.plot=TRUE,
      xlab = paste0("Possible ALL #", final_df$PRE_clust_3[ii],
                    ", dist = ", round(final_df$PRE_value_3[ii],2))
    )

    dev.off()
  }
  
  # Heatmap distancias
  
  pdf(file = paste0(out_path, "distances", ".pdf"), paper="a4r")
    image(
      t(m1_dist[num_new_hmp:1,]^(-0.10)),
      axes = FALSE
    )
    axis(side=1, at=seq(0,1,length.out=40), labels = 1:40)
    axis(side=2, at=seq(1,0,length.out=num_new_hmp), labels = 1:num_new_hmp)
    title(tag)
  dev.off()
}