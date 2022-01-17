library(foreach)
library(lubridate)
#library(matrixStats)

################################################################################
# get_heatmap_matrix
################################################################################

get_heatmap_matrix <- function(fnames, .scale=FALSE, num_years=2) {
  
  # FUNCTION FOR GETTING THE INDEX IN THE OUTPUT MATRIX
  get_matrix_index <- function(date) {
    wk <- lubridate::isoweek(date)
    wd <- lubridate::wday(date, week_start=1)
    hr <- lubridate::hour(date)
    idx <- (wk-1)*7*24 + (wd-1)*24 + (hr+1)
    return(idx)
  }
  
  # FUNCTION FOR ALIGNING ANY TIME SERIES BY ISO WEEKS (1 TO 53)
  # INPUT: dataframe with pairs dataset-filename
  align_time_series <- function(fname) {
    # Load dataframe
    load(fname[[1]]) # UNCOMMENT!
    
	  ##############################################
# 	  print(fname)
#     # 69abbed24a5685b949114dd5162074c1.RData
#     browser()
#     load("C:/Users/carlos.quesada/Documents/WHY/2022.01.14 - go4 clValid analysis/go4_pre/69abbed24a5685b949114dd5162074c1.RData")
	  ##############################################
    
    # By hours
    t_factor <- cut(edf$df$times, breaks = "1 hour")
    # Aggregate by hour
    if (.scale) {
      aggr_data <- stats::aggregate(
        x   = scale(edf$df$values),
        by  = list(date_time = t_factor),
        FUN = sum
      )
    } else {
      aggr_data <- stats::aggregate(
        x   = edf$df$values,
        by  = list(date_time = t_factor),
        FUN = sum
      )
    }
    
    # Input vector of dates
    i_times_vect <- lubridate::ymd_hms(aggr_data$date_time, tz="UTC")
    # Input vector of values
    i_value_vect <- aggr_data$V1
    # Length of input vector
    i_len <- length(i_value_vect)
    # Number of years to be taken per time series (if "as much as possible" is
    # wanted, just set sp <- 0 and uncomment the commented while loop)
    nyears <- length(unique(lubridate::isoyear(i_times_vect)))
    # Length of one year
    len1yr <- 53*7*24
    # Output vector
    # o_times_vect <- rep(lubridate::as_datetime(NA), len1yr*nyears)
    o_value_vect <- rep(NA, len1yr*nyears)
    # Index of start of input vector
    i1 <- 1
    # Loop
    for(yy in 1:nyears) {
      # Get start index in output matrix
      o1 <- get_matrix_index(i_times_vect[i1])
      o1 <- o1 + (yy-1)*len1yr
      # Get end index in output matrix
      o2 <- yy*len1yr
      # Get end index of input vector
      i2 <- i1 + (o2 - o1)
      
      # THIS PART DOESN'T MATTER BECAUSE IT TAKES NaNs
      # i2_aux <- i1 + (o2 - o1)
      # i2 <- ifelse(i2_aux > i_len, i_len, i2_aux)
      
      # Complete the output vector
      # o_times_vect[o1:o2] <- i_times_vect[i1:i2]
      
	    ##############################################
	    # print(yy)
	    # print(i1)
	    # print(i2)
	    # print("---")
	    ##############################################
	    
      o_value_vect[o1:o2] <- i_value_vect[i1:i2]
      # Out of the loop if the sequence is finished
	    if ((i2+1) > i_len) break
      # Find the new i1 (it must be Mon, 1 WK at 00:00h)
      aux_idx <- get_matrix_index(i_times_vect[i2+1])
      i1 <- (i2 + 1) - (aux_idx - 1)
    }
    # o_times_matx <- array(as.character(o_times_vect), dim = c(24,53*7,nyears))
    o_value_matx <- array(o_value_vect, dim = c(24,53*7,nyears))
    # Compute the mean through array slices
    o_mean_matx <- apply(o_value_matx, 2, rowMeans, na.rm=TRUE)
    # Return matrix
    return(o_mean_matx)
  }
  
  # Align time series!
  out_list <- apply(fnames, 1, align_time_series)
  # Scale each column
  if (.scale) {
    out_list <- apply(out_list, 2, scale)
  }
  # Create matrix from means
  o_mean_mat <- matrix(
    data = rowMeans(out_list),
    nrow = 24
  )
  # Flip matrix
  o_mean_mat <- o_mean_mat[nrow(o_mean_mat):1,]
  
  # # Create matrix from Sds
  # o_sd_mat <- matrix(
  #   data = matrixStats::rowSds(out_list),
  #   nrow = 24
  # )
  # # Flip matrix
  # o_sd_mat <- o_sd_mat[nrow(o_sd_mat):1,]
  
  # return(list(mean=o_mean_mat, sd=o_sd_mat))
  return(o_mean_mat)
}

################################################################################
# plot_heatmap_matrix
################################################################################

plot_heatmap_matrix <- function(m, format_file=NA, file_path=NA, plot_width=800, plot_height = 600, subtitle=NULL) {
  # Format of output files
  if (format_file == "png") {
    png(
      file_path,
      width = plot_width,
      height = plot_height
    )
  }
  if (format_file == "pdf") {
    pdf(
      file_path,
      width = plot_width,
      height = plot_height
    )
  }
  
  # Month labels
  m_labels <- rep(NA, 371)
  m_labels[round(seq(1, 371, length.out=25))[seq(2,25,by=2)]] <- month.abb[1:12]
  # Plot heatmap
  image(
    t(m),
    useRaster=TRUE,
    axes=FALSE,
  )
  axis(1, at=seq(0, 1, length.out=371), labels=m_labels, las=0, tick=F)
  axis(2, at=seq(0, 1, length.out=24), labels=23:0, las=2, tick=F)
  title(sub=subtitle)
  
  # Format of output files
  if (format_file == "png" | format_file == "pdf") {
    dev.off()
  }
}


################################################################################
# clValid2_heatmaps
################################################################################

clValid2_heatmaps <- function() {
  ##############################################################################
  ##  CHECK AND COMPLETE IF NEEDED!!!
  ##############################################################################
  
  # User defined variables
  if (.Platform$OS.type == "windows") {
    feats_path  <- "C:/Users/carlos.quesada/Documents/WHY/2022.01.13 - go4 CORR feats/go4_pre_22.01.12/feats-go4_pre.csv"
    clValid_dir <- "C:/Users/carlos.quesada/Documents/WHY/2022.01.14 - go4 clValid analysis/"
    dataset_dir <- c(goi="C:/Users/carlos.quesada/Documents/WHY/2022.01.14 - go4 clValid analysis/go4_pre/")
    hmm_dir     <- "C:/Users/carlos.quesada/Documents/WHY/2022.01.14 - go4 clValid analysis/out/"
    hmp_dir     <- "C:/Users/carlos.quesada/Documents/WHY/2022.01.14 - go4 clValid analysis/out/"
  }
  if (.Platform$OS.type == "unix") {
    feats_path  <- "/home/ubuntu/carlos.quesada/disk/features/feats_go4_pre.csv"
    clValid_dir <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.01.13_go4-pre/data/"
    # As named vector for each dataset
    dataset_dir <- c(goi="/home/ubuntu/carlos.quesada/disk/go4_pre/imp/")
    hmm_dir     <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.01.13_go4-pre/hmm2/"
    hmp_dir     <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.01.13_go4-pre/hmp2/"
  }
  
  number_of_clusters <- 30
  # skip_xxx2x <- TRUE
  scale_hmm <- TRUE
  
  ################################################################################
  ##  LOAD FEATURES
  ################################################################################
  
  # Load feats
  feats <- data.table::fread(
    file   = feats_path,
    header = TRUE,
    sep    = ",",
    select = c("data_set", "file", "rel_imputed_na", "is_household", "minimum")
  )
  
  ################################################################################
  ##  LOOP
  ################################################################################
  fnames <- list.files(path = clValid_dir, pattern = "*.clValid2")
  fun_export <- c("get_heatmap_matrix", "plot_heatmap_matrix")
  
  # Setup parallel backend to use many processors
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)

  o <- foreach::foreach(ff = 1:length(fnames), .export=fun_export) %:%
    foreach::foreach(cc = number_of_clusters:1, .inorder = FALSE, .export=fun_export) %dopar% {
  
  # for(ff in 1:length(fnames)) {
  #   for(cc in 2:number_of_clusters) {
      
      # Working file name
      w_fname <- fnames[ff]
      # Working config file name
      w_cname <- paste0(strsplit(w_fname, ".clValid2"), ".config")
      
      w_cpath <- paste0(clValid_dir, w_cname)
      load(w_cpath)
      
      row_conditions <- rep(FALSE, nrow(feats))
      
      for(ii in 1:nrow(analysis_type$dd)) {
        row_conditions <- row_conditions | (
          feats$data_set == analysis_type$dd$key[ii] &
            feats$rel_imputed_na < analysis_type$dd$rel_imputed_na[ii] &
            feats$is_household == analysis_type$dd$is_household[ii] &
            # feats$sum_per_day > sum_pday &
            feats$minimum >= 0
        )
      }
      # Discard NA values
      row_conditions[is.na(row_conditions)] <- FALSE
      
      w_feats <- feats[row_conditions,]
      w_fpath <- paste0(clValid_dir, w_fname)
      
      load(w_fpath)
      
      ### Get the clustering 
      # HIERARCHICAL
      if (analysis_type$mm == "hierarchical") {
        cluster_list <- cutree(o@clusterObjs[["hierarchical"]], k=number_of_clusters)
      }
      # K-MEANS
      if (analysis_type$mm == "kmeans") {
        cluster_list <- o@clusterObjs[["kmeans"]][[as.character(number_of_clusters)]][["cluster"]]
      }
      # DIANA
      if (analysis_type$mm == "diana") {
        cluster_list <- cutree(o@clusterObjs[["diana"]], k=number_of_clusters)
      }
      # FANNY
      if (analysis_type$mm == "fanny") {
        cluster_list <- o@clusterObjs[["fanny"]][[as.character(number_of_clusters)]]$clustering
      }
      # SOM
      if (analysis_type$mm == "som") {
        cluster_list <- o@clusterObjs[["som"]][[as.character(number_of_clusters)]]$unit.classif
      }
      # PAM
      if (analysis_type$mm == "pam") {
        cluster_list <- o@clusterObjs[["pam"]][[as.character(number_of_clusters)]]$clustering
      }
      # SOTA
      if (analysis_type$mm == "sota") {
        cluster_list <- o@clusterObjs[["sota"]][[as.character(number_of_clusters)]]$clust
      }
      # CLARA
      if (analysis_type$mm == "clara") {
        cluster_list <- o@clusterObjs[["clara"]][[as.character(number_of_clusters)]]$clustering
      }
      # MODEL-BASED
      if (analysis_type$mm == "model") {
        cluster_list <- o@clusterObjs[["model"]][[as.character(number_of_clusters)]]$classification
      }
      
      # Get cluster indices
      idx <- cluster_list == cc
      # Set vector of paths
      paths_vector <- paste0(dataset_dir[w_feats$data_set[idx]], w_feats$file[idx], ".RData")
      
      # # DELETE THIS BLOCK
      # paths_vector <- list.files(dataset_dir, full.names = TRUE)[1:5]
      
      # Get heatmap matrix
      m <- get_heatmap_matrix(data.frame(paths_vector), .scale = scale_hmm, num_years=1)

      # for (type in c("M", "S")) {
      
      # Cluster loop
      hm_fname <- print(paste0(strsplit(w_fname, ".clValid2"), "-", cc))
      # hm_fname_type <- paste0(hm_fname, type)
      
      # File paths
      #hm_fname <- paste0(w_fname, "-", number_of_clusters)
      hmm_path <- paste0(hmm_dir, "hmm_", hm_fname, ".RData")
      hmp_path <- paste0(hmp_dir, "hmp_", hm_fname, ".png")

      # Save heatmap matrix
      save(m, file = hmm_path)
      
      # Generate heatmaps
      plot_heatmap_matrix(
        # m           = ifelse(type=="M", m$mean, m$sd),
        m           = m,
        format_file = "png",
        file_path   = hmp_path,
        plot_width  = 1200,
        plot_height = 900,
        subtitle    = hm_fname
      )
      
      # }
    # }
  }
  
  # Stop parallelization
  parallel::stopCluster(cl)
}

clValid2_heatmaps()
