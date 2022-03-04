library(foreach)
library(lubridate)

################################################################################
# get_heatmap_matrix
################################################################################

get_heatmap_matrix <- function(fnames, .scale=FALSE) {
  # FUNCTION FOR ALIGNING ANY TIME SERIES BY ISO WEEKS (1 TO 53)
  # INPUT: dataframe with pairs dataset-filename
  align_time_series <- function(fname) {
    # Load dataframe
    load(fname[[1]])
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
    # Vector of dates
    date_vect <- as.POSIXct(aggr_data$date_time, tz="GMT")
    # Number of years to be taken per time series (if "as much as possible" is
    # wanted, just set sp <- 0 and uncomment the commented while loop)
    nyears <- 2
    # Length of the output vector
    lov <- 53*7*24
    # Starting point (is set to catch the central part of the time series
    # thus avoiding artificially extended ends)
    # sp <- 0 
    sp <- floor(length(date_vect)/2) - floor((nyears*lov)/2)
    # Output list
    li <- 0
    out_list <- list()
    # Loop
    # while(sp + lov <= length(date_vect)) {
    while(li < nyears) {
      # Create output vector
      out_vect <- rep(NA, lov)
      # Get time triad of initial time
      ini_week <-
        as.numeric(strftime(as.Date(date_vect[sp+1]), format = "%V"))
      ini_wday <- lubridate::wday(date_vect[sp+1], week_start=1)
      ini_hour <- lubridate::hour(date_vect[sp+1])
      # Position in the (53 x 7) x 24 vector
      ini_posv <- (ini_week-1)*7*24 + (ini_wday-1)*24 + (ini_hour+1)
      # Data pointer
      pp <- sp + lov - ini_posv + 1
      # Fill the tail of output vector
      out_vect[ini_posv:lov] <- aggr_data[(sp+1):pp,2]
      # Get time triad of final time
      fin_week <-
        as.numeric(strftime(as.Date(date_vect[pp+1]), format = "%V"))
      fin_wday <- lubridate::wday(date_vect[pp+1], week_start=1)
      fin_hour <- lubridate::hour(date_vect[pp+1])
      # Get new data pointers
      qq <- pp - (fin_week-1)*7*24 - (fin_wday-1)*24 + 1
      rr <- qq + ini_posv - 2
      # Fill the head of output vector
      out_vect[1:(ini_posv-1)] <- aggr_data[qq:rr,2]
      # Output list
      li <- li + 1
      out_list[[li]] <- out_vect
      # Move the starting point
      sp <- rr
    }
    return(out_list)
  }
  
  # Align time series!
  out_list <- apply(fnames, 1, align_time_series)
  
  # Unlist two layers
  out_list <- do.call(rbind,do.call(rbind,out_list))
  # Create matrix from means
  o_mat <- matrix(
    data = colMeans(out_list),
    nrow = 24
  )
  # Flip matrix
  o_mat <- o_mat[nrow(o_mat):1,]
  
  return(o_mat)
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
    feats_path  <- "C:/Users/carlos.quesada/Documents/WHY/2021.12.28 - go4_feats/go4_pre_21.12.28/feats_go4_pre.csv"
    clValid_dir <- "C:/Users/carlos.quesada/Documents/GitHub/why-T2.1-results/clValid2/2021.12.27_go4-pre/data/"
    dataset_dir <- "G:/Mi unidad/WHY/Datasets/"
    hmm_dir     <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.05.05_3-cl-methods/hmm/"
    hmp_dir     <- "G:/Mi unidad/WHY/Analyses/clValid2/2021.05.05_3-cl-methods/hmp/"
  }
  if (.Platform$OS.type == "unix") {
    feats_path  <- "/home/ubuntu/carlos.quesada/disk/features/feats_go4_pre.csv"
    clValid_dir <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2021.12.27_go4-pre/data/"
    # As named vector for each dataset
    dataset_dir <- c(goi="/home/ubuntu/carlos.quesada/disk/go4_pre/imp/")
    hmm_dir     <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2021.12.27_go4-pre/hmm/"
    hmp_dir     <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2021.12.27_go4-pre/hmp/"
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
  
  # Setup parallel backend to use many processors
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)

  o <- foreach::foreach(ff = 1:length(fnames)) %:%
    foreach::foreach(cc = number_of_clusters:1, .inorder = FALSE) %dopar% {
  
  # for(ff in 1:length(fnames)) {
  #   for(cc in 1:number_of_clusters) {
      
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
      
      browser()
      
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
      
      # Cluster loop
      print(paste0(w_fname, " - ", cc))
      
      # Get cluster indices
      idx <- cluster_list == cc
      # Set vector of paths
      paths_vector <- paste0(dataset_dir[[w_feats$data_set[idx]]], w_feats$file[idx], ".RData")
      
      # File paths
      hm_fname <- paste0(w_fname, "-", number_of_clusters)
      hmm_path <- paste0(hmm_dir, "hmm_", hm_fname, ".RData")
      hmp_path <- paste0(hmp_dir, "hmp_", hm_fname, ".png")
      
      # Get heatmap matrix
      m <- get_heatmap_matrix(data.frame(paths_vector), .scale = scale_hmm)
      # Save heatmap matrix
      save(m, file = hmm_path)
  
      # Generate heatmaps
      plot_heatmap_matrix(
        m           = m,
        format_file = "png",
        file_path   = hmp_path,
        plot_width  = 1200,
        plot_height = 900,
        subtitle    = hm_fname
      )
    }
  # }
  
  # Stop parallelization
  parallel::stopCluster(cl)
}

clValid2_heatmaps()