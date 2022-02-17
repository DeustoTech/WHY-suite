################################################################################
# CARLOS QUESADA GRANJA
# 4 de febrero de 2022
# UNIVERSIDAD DE DEUSTO
# ---------------------
# Definitive file for generating analysis and reports from imputed folders
################################################################################

library(foreach)
library(clValid2)
library(mclust)
library(lubridate)
library(rmarkdown)

set.seed(1981)

################################################################################
##  Function to select the row conditions (common to analysis and heatmaps)
################################################################################

set_row_conditions <- function(feats, analysis_type) {
  # Initialization
  row_conditions <- rep(FALSE, nrow(feats))
  
  # Condition 1: Dataset key
  if (is.null(analysis_type$dd$key))
    cond_key <- TRUE
  else
    cond_key <- feats$data_set == analysis_type$dd$key
  
  # Condition 2: Percentage of imputed samples in TS
  if (is.null(analysis_type$dd$rel_imputed_na))
    cond_imp <- TRUE
  else
    cond_imp <- feats$rel_imputed_na < analysis_type$dd$rel_imputed_na
  
  # Condition 3: Household or not
  if (is.null(analysis_type$dd$is_household))
    cond_hhd <- TRUE
  else
    cond_hhd <- feats$is_household == analysis_type$dd$is_household
  
  # Condition 4: Type of tariff
  if (is.null(analysis_type$dd$ref_atr_tariff))
    cond_trf <- TRUE
  else
    cond_trf <- substr(feats$ref_atr_tariff,1,1) == analysis_type$dd$ref_atr_tariff
  
  # Mandatory condition
  cond_min <- feats$minimum >= 0
  
  # Check all conditions
  row_conditions <- row_conditions | (
    cond_key & cond_imp & cond_hhd & cond_trf & cond_min
  )
  
  # Discard NA values
  row_conditions[is.na(row_conditions)] <- FALSE
  
  return(row_conditions)
}

################################################################################
##  ANALYSIS USING THE clValid PACKAGE
##  R file with the analysis functions of the Rmd file
################################################################################

call_clValid2 <- function(output_dir, analysis_type, feats, feats_set) {
  
  # Set file name
  ff_name <- analysis_type$ff
  key     <- unique(unlist(lapply(analysis_type$dd, `[[`, 1)))
  len_key <- length(key)
  dd_name <- ifelse(len_key == 1, key, paste0(len_key, "ds")) 
  mm_name <- substr(analysis_type$mm,1,3)
  vv_name <- substr(analysis_type$vv,1,1)
  len_cl  <- length(analysis_type$cc)
  cc_name <- ifelse(len_cl == 1, paste0(analysis_type$cc[1], "cl"), "varCl")
  
  final_name <- paste(ff_name, dd_name, mm_name, cc_name, vv_name, sep="_")
  print(final_name)
  
  ##############################################################################
  ##  MAIN clValid ANALYSIS
  ##############################################################################
  nrow_feats <- nrow(feats)
  
  # Set row conditions
  row_conditions <- set_row_conditions(feats, analysis_type)
  
  row_names <- paste0(feats$data_set, "_", feats$file)
  feats_aux <- feats[row_conditions,]
  feats_aux <- subset(feats_aux, select = feats_set[[analysis_type$ff]])
  feats_aux <- as.matrix(feats_aux)
  # SCALE
  feats_aux <- scale(feats_aux)
  row.names(feats_aux) <- row_names[row_conditions]
  
  o <- clValid2::clValid(
    obj        = feats_aux,
    nClust     = analysis_type$cc,
    clMethods  = analysis_type$mm,
    validation = analysis_type$vv,
    maxitems   = nrow(feats_aux) + 1
  )
  
  # ANALYSIS FILE
  filename <- paste0(output_dir, final_name, ".clValid2")
  save("o", file = filename)
  # CONFIGURATION FILE
  filename <- paste0(output_dir, final_name, ".config")
  analysis_type$ff <- feats_set[[analysis_type$ff]]
  save("analysis_type", file = filename)
}

################################################################################
##  CALL TO THE FUNCTION
################################################################################

cluster_features <- function(
  feats_file, output_dir, ff_sel, dd_sel, mm_sel, vv_sel, cc_sel) {
  
  feats <- data.table::fread(
    file   = feats_file,
    header = TRUE,
    sep    = ","
  )
  
  ########################
  ##  SETS OF FEATURES  ##
  ########################
  
  feats_set <- list(
    # TYPE 1 -> "sAggr"
    sAggr = c(
      "rel_mean_00h04hspr", "rel_mean_04h08hspr", "rel_mean_08h12hspr",
      "rel_mean_12h16hspr", "rel_mean_16h20hspr", "rel_mean_20h00hspr",
      "rel_mean_00h04hsum", "rel_mean_04h08hsum", "rel_mean_08h12hsum",
      "rel_mean_12h16hsum", "rel_mean_16h20hsum", "rel_mean_20h00hsum",
      "rel_mean_00h04haut", "rel_mean_04h08haut", "rel_mean_08h12haut",
      "rel_mean_12h16haut", "rel_mean_16h20haut", "rel_mean_20h00haut",
      "rel_mean_00h04hwin", "rel_mean_04h08hwin", "rel_mean_08h12hwin",
      "rel_mean_12h16hwin", "rel_mean_16h20hwin", "rel_mean_20h00hwin",
      "rel_mean_weekday_pday"
    ),
    # TYPE 2 -> "peaks"
    peaks = c(
      "peak_hour_1", "off_peak_hour_1", "peak_month", "off_peak_month",
      "peak_weekday_pday"
    ),
    # TYPE 3 -> "tsfAC"
    tsfAC = c(
      "mean", "entropy", "seasonal_strength1", "seasonal_strength2",
      "seasonal_strength3", "ac_day_1", "ac_day_7", "ac_day_28"
    ),
    # TYPE 4 -> "cat22"
    cat22 = c(
      "DN_HistogramMode_5", "DN_HistogramMode_10", "CO_f1ecac", "CO_FirstMin_ac",
      "CO_HistogramAMI_even_2_5", "CO_trev_1_num", "MD_hrv_classic_pnn40",
      "SB_BinaryStats_mean_longstretch1", "SB_TransitionMatrix_3ac_sumdiagcov",
      "PD_PeriodicityWang_th0_01", "CO_Embed2_Dist_tau_d_expfit_meandiff",
      "IN_AutoMutualInfoStats_40_gaussian_fmmi", "FC_LocalSimple_mean1_tauresrat",
      "DN_OutlierInclude_p_001_mdrmd", "DN_OutlierInclude_n_001_mdrmd",
      "SP_Summaries_welch_rect_area_5_1", "SB_BinaryStats_diff_longstretch0",
      "SB_MotifThree_quantile_hh", "SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1",
      "SC_FluctAnal_2_dfa_50_1_2_logi_prop_r1",
      "SP_Summaries_welch_rect_centroid", "FC_LocalSimple_mean3_stderr"
    ),
    # TYPE 5 -> "sAggrP6"
    sAggrP6 = c(
      "rel_mean_td2.0_p6_00h08h_spr", "rel_mean_td2.0_p6_08h10h_spr",
      "rel_mean_td2.0_p6_10h14h_spr", "rel_mean_td2.0_p6_14h18h_spr", 
      "rel_mean_td2.0_p6_18h22h_spr", "rel_mean_td2.0_p6_22h00h_spr", 
      "rel_mean_td2.0_p6_00h08h_sum", "rel_mean_td2.0_p6_08h10h_sum",
      "rel_mean_td2.0_p6_10h14h_sum", "rel_mean_td2.0_p6_14h18h_sum", 
      "rel_mean_td2.0_p6_18h22h_sum", "rel_mean_td2.0_p6_22h00h_sum", 
      "rel_mean_td2.0_p6_00h08h_aut", "rel_mean_td2.0_p6_08h10h_aut",
      "rel_mean_td2.0_p6_10h14h_aut", "rel_mean_td2.0_p6_14h18h_aut", 
      "rel_mean_td2.0_p6_18h22h_aut", "rel_mean_td2.0_p6_22h00h_aut", 
      "rel_mean_td2.0_p6_00h08h_win", "rel_mean_td2.0_p6_08h10h_win",
      "rel_mean_td2.0_p6_10h14h_win", "rel_mean_td2.0_p6_14h18h_win", 
      "rel_mean_td2.0_p6_18h22h_win", "rel_mean_td2.0_p6_22h00h_win", 
      "rel_mean_weekday_drm"
    ),
    # TYPE 6 -> "sAggrDRM"
    sAggrDRM = c(
      "rel_mean_00h04hspr_drm", "rel_mean_04h08hspr_drm", "rel_mean_08h12hspr_drm",
      "rel_mean_12h16hspr_drm", "rel_mean_16h20hspr_drm", "rel_mean_20h00hspr_drm",
      "rel_mean_00h04hsum_drm", "rel_mean_04h08hsum_drm", "rel_mean_08h12hsum_drm",
      "rel_mean_12h16hsum_drm", "rel_mean_16h20hsum_drm", "rel_mean_20h00hsum_drm",
      "rel_mean_00h04haut_drm", "rel_mean_04h08haut_drm", "rel_mean_08h12haut_drm",
      "rel_mean_12h16haut_drm", "rel_mean_16h20haut_drm", "rel_mean_20h00haut_drm",
      "rel_mean_00h04hwin_drm", "rel_mean_04h08hwin_drm", "rel_mean_08h12hwin_drm",
      "rel_mean_12h16hwin_drm", "rel_mean_16h20hwin_drm", "rel_mean_20h00hwin_drm",
      "rel_mean_weekday_drm"
    )
  )
  
  cluster_codes <- list()
  
  # ff: SETS OF FEATURES
  for (ff in ff_sel) {
    # dd: DATASETS
    for (dd in dd_sel) {
      # mm: CLUSTER METHODS
      for (mm in mm_sel) {
        # vv: VALIDATION METHODS
        for (vv in vv_sel) {
          # cc: NUMBER OF CLUSTERS
          cluster_codes[length(cluster_codes)+1] <- list(list(
            ff = ff,
            dd = dd,
            mm = mm,
            vv = vv,
            cc = cc_sel
          ))
        }
      }
    }
  }
  
  for (cluster_code_ii in cluster_codes) {
    call_clValid2(output_dir, cluster_code_ii, feats, feats_set)
  }
}

################################################################################
# get_heatmap_matrix
################################################################################

get_heatmap_matrix <- function(fnames, .scale=FALSE) {
  
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
    data = rowMeans(out_list, na.rm = TRUE),
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

plot_heatmap_matrix <- function(
  m, format_file=NA, file_path=NA, plot_width=800, plot_height = 600, subtitle=NULL) {
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

clValid2_heatmaps <- function(
  feats_file, clValid_dir, hmm_dir, hmp_dir, dataset_dir, num_cluster, scale_hmm
) {
  # Load feats
  feats <- data.table::fread(
    file   = feats_file,
    header = TRUE,
    sep    = ",",
    select = c("data_set", "file", "rel_imputed_na", "is_household", "minimum")
  )
  
  # LOOP
  fnames <- list.files(path = clValid_dir, pattern = "*.clValid2")
  fun_export <- c("get_heatmap_matrix", "plot_heatmap_matrix", "set_row_conditions")
  
  # Setup parallel backend to use many processors
  cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)
  
  o <- foreach::foreach(ff = 1:length(fnames), .export=fun_export) %:%
    foreach::foreach(cc = 1:num_cluster, .inorder = FALSE, .export=fun_export) %dopar% {
      
      # for(ff in 1:length(fnames)) {
      # for(cc in 2:num_cluster) {
      
      # Working file name
      w_fname <- fnames[ff]
      # Working config file name
      w_cname <- paste0(strsplit(w_fname, ".clValid2"), ".config")
      
      w_cpath <- paste0(clValid_dir, w_cname)
      load(w_cpath)
      
      row_conditions <- set_row_conditions(feats, analysis_type)
      
      w_feats <- feats[row_conditions,]
      w_fpath <- paste0(clValid_dir, w_fname)
      
      load(w_fpath)
      
      ### Get the clustering 
      # HIERARCHICAL
      if (analysis_type$mm == "hierarchical") {
        cluster_list <- cutree(o@clusterObjs[["hierarchical"]], k=num_cluster)
      }
      # K-MEANS
      if (analysis_type$mm == "kmeans") {
        cluster_list <- o@clusterObjs[["kmeans"]][[as.character(num_cluster)]][["cluster"]]
      }
      # DIANA
      if (analysis_type$mm == "diana") {
        cluster_list <- cutree(o@clusterObjs[["diana"]], k=num_cluster)
      }
      # FANNY
      if (analysis_type$mm == "fanny") {
        cluster_list <- o@clusterObjs[["fanny"]][[as.character(num_cluster)]]$clustering
      }
      # SOM
      if (analysis_type$mm == "som") {
        cluster_list <- o@clusterObjs[["som"]][[as.character(num_cluster)]]$unit.classif
      }
      # PAM
      if (analysis_type$mm == "pam") {
        cluster_list <- o@clusterObjs[["pam"]][[as.character(num_cluster)]]$clustering
      }
      # SOTA
      if (analysis_type$mm == "sota") {
        cluster_list <- o@clusterObjs[["sota"]][[as.character(num_cluster)]]$clust
      }
      # CLARA
      if (analysis_type$mm == "clara") {
        cluster_list <- o@clusterObjs[["clara"]][[as.character(num_cluster)]]$clustering
      }
      # MODEL-BASED
      if (analysis_type$mm == "model") {
        cluster_list <- o@clusterObjs[["model"]][[as.character(num_cluster)]]$classification
      }
      
      # Get cluster indices
      idx <- cluster_list == cc
      # Set vector of paths
      paths_vector <- paste0(dataset_dir[w_feats$data_set[idx]], w_feats$file[idx], ".RData")
      
      # Get heatmap matrix
      m <- get_heatmap_matrix(data.frame(paths_vector), .scale = scale_hmm)
      
      # for (type in c("M", "S")) {
      
      # Cluster loop
      hm_fname <- print(paste0(strsplit(w_fname, ".clValid2"), "-", cc))
      # hm_fname_type <- paste0(hm_fname, type)
      
      # File paths
      #hm_fname <- paste0(w_fname, "-", num_cluster)
      hmm_path <- paste0(hmm_dir, "hmm_", hm_fname, ".RData")
      hmp_path <- paste0(hmp_dir, "hmp_", hm_fname, ".png")
      
      # Save heatmap matrix
      save(m, idx, file = hmm_path)
      
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