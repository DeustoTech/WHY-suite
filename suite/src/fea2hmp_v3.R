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
library(forecast)
library(ggplot2)
library(fitdistrplus)
library(sgt) # skewed generalized t distribution

set.seed(1981)

# List of distributions
distr_vect <- c(
  "unif", "norm", "lnorm", "exp", "cauchy", "gamma", "logis", "weibull", "sgt"
)

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
    cond_trf <-
      substr(feats$ref_atr_tariff,1,1) == analysis_type$dd$ref_atr_tariff
  
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

call_clValid2 <- function(output_dir, analysis_type, feats, feats_set, use_clValid2) {
  
  # Set file name
  ff_name <- analysis_type$ff
  key     <- unique(sapply(1:length(analysis_type$dd),
                           function(x) analysis_type$dd[[x]]$key))
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
  feats_aux <- feats[row_conditions, feats_set[[analysis_type$ff]]]
  # feats_aux <- subset(feats_aux, select = feats_set[[analysis_type$ff]])
  feats_aux <- as.matrix(feats_aux)
  # SCALE
  feats_aux <- scale(feats_aux)
  row.names(feats_aux) <- row_names[row_conditions]
  
  o <- if (use_clValid2) {
    clValid2::clValid(
      obj        = feats_aux,
      nClust     = analysis_type$cc,
      clMethods  = analysis_type$mm,
      validation = analysis_type$vv,
      maxitems   = nrow(feats_aux) + 1
    )
  } else {
    print("NOT USING clValid2!")
    kohonen::som(feats_aux, grid=kohonen::somgrid(1, analysis_type$cc))
  }
  
  # ANALYSIS FILE
  fname_extens <- ifelse(use_clValid2, ".clValid2", ".somObj")
  filename <- paste0(output_dir, final_name, fname_extens)
  save("o", file = filename)
  # CONFIGURATION FILE
  filename <- paste0(output_dir, final_name, ".config")
  analysis_type$ff <- feats_set[[analysis_type$ff]]
  save("analysis_type", file = filename)
}

################################################################################
##  check_subfolders()
################################################################################

check_subfolders <- function(o) {
  # All output subfolders
  o_sub <- c(
    paste0(o, "data/"),
    paste0(o, "hmp/"),
    paste0(o, "report/"),
    paste0(o, "acf/"),
    paste0(o, "dplot/"),
    paste0(o, "distr/")
  )
  # Create folders if they do NOT exist
  for (oo in o_sub) {
    if (!dir.exists(oo))
      dir.create(oo)
  }
}

################################################################################
##  SETS OF FEATIURES
################################################################################

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
    "DN_HistogramMode_5", "DN_HistogramMode_10", "CO_f1ecac",
    "CO_FirstMin_ac", "CO_HistogramAMI_even_2_5", "CO_trev_1_num",
    "MD_hrv_classic_pnn40", "SB_BinaryStats_mean_longstretch1",
    "SB_TransitionMatrix_3ac_sumdiagcov", "PD_PeriodicityWang_th0_01",
    "CO_Embed2_Dist_tau_d_expfit_meandiff",
    "IN_AutoMutualInfoStats_40_gaussian_fmmi",
    "FC_LocalSimple_mean1_tauresrat", "DN_OutlierInclude_p_001_mdrmd",
    "DN_OutlierInclude_n_001_mdrmd", "SP_Summaries_welch_rect_area_5_1",
    "SB_BinaryStats_diff_longstretch0", "SB_MotifThree_quantile_hh",
    "SC_FluctAnal_2_rsrangefit_50_1_logi_prop_r1",
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
    "rel_mean_00h04hspr_drm", "rel_mean_04h08hspr_drm",
    "rel_mean_08h12hspr_drm", "rel_mean_12h16hspr_drm",
    "rel_mean_16h20hspr_drm", "rel_mean_20h00hspr_drm",
    "rel_mean_00h04hsum_drm", "rel_mean_04h08hsum_drm",
    "rel_mean_08h12hsum_drm", "rel_mean_12h16hsum_drm",
    "rel_mean_16h20hsum_drm", "rel_mean_20h00hsum_drm",
    "rel_mean_00h04haut_drm", "rel_mean_04h08haut_drm",
    "rel_mean_08h12haut_drm", "rel_mean_12h16haut_drm",
    "rel_mean_16h20haut_drm", "rel_mean_20h00haut_drm",
    "rel_mean_00h04hwin_drm", "rel_mean_04h08hwin_drm",
    "rel_mean_08h12hwin_drm", "rel_mean_12h16hwin_drm",
    "rel_mean_16h20hwin_drm", "rel_mean_20h00hwin_drm",
    "rel_mean_weekday_drm"
  )
)


################################################################################
##  CALL TO THE FUNCTION
################################################################################

cluster_features <- function(
  feats_file, output_dir, ff_sel, dd_sel, mm_sel, vv_sel, cc_sel, use_clValid2=TRUE) {
  
  # Create folders if they do NOT exist
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  # Check that all subfolders in "output_dir" exist
  check_subfolders(output_dir)
  # Change dir to "/data"
  output_dir <- paste0(output_dir, "data/")
  
  # Open features file
  feats_ext <- substr(feats_file, nchar(feats_file)-2, nchar(feats_file))
  
  if (tolower(feats_ext) == "csv") {
    feats <- data.frame(
      data.table::fread(
        file   = feats_file,
        header = TRUE,
        sep    = ","
      )
    )
  } else {
    load(feats_file)
  }
  
  ########################
  ##  SETS OF FEATURES  ##
  ########################
  
  cluster_codes <- list()
  
  # ff: SETS OF FEATURES
  for (ff in ff_sel) {
    # dd: DATASETS
    # for (dd in dd_sel) {
      # mm: CLUSTER METHODS
      for (mm in mm_sel) {
        # vv: VALIDATION METHODS
        for (vv in vv_sel) {
          # cc: NUMBER OF CLUSTERS
          cluster_codes[length(cluster_codes)+1] <- list(
            list(
            ff = ff,
            dd = dd_sel,
            mm = mm,
            vv = vv,
            cc = cc_sel
            )
          )
        }
      }
    # }
  }
  
  for (cluster_code_ii in cluster_codes) {
    call_clValid2(output_dir, cluster_code_ii, feats, feats_set, use_clValid2)
  }
}

################################################################################
# get_matrix_index
# FUNCTION FOR GETTING THE INDEX IN THE OUTPUT MATRIX
################################################################################
get_matrix_index <- function(date) {
  wk <- lubridate::isoweek(date)
  wd <- lubridate::wday(date, week_start=1)
  hr <- lubridate::hour(date)
  idx <- (wk-1)*7*24 + (wd-1)*24 + (hr+1)
  return(idx)
}

################################################################################
# align_time_series
# FUNCTION FOR ALIGNING ANY TIME SERIES BY ISO WEEKS (1 TO 53)
# INPUT: dataframe with pairs dataset-filename
################################################################################

align_time_series <- function(fname, .scale) {
  # Load dataframe
  load(fname)
  # By hours
  t_factor <- cut(edf$df$times, breaks = "1 hour")
  # Aggregate by hour
  aggr_data <- aggregate(
    x   = edf$df$values,
    by  = list(date_time = t_factor),
    FUN = sum
  )
  # Check that upper whisker is not 0
  upwh <- upper_whisker(aggr_data$x)
  if(upwh == 0) return(NULL)
  # Scale data
  if(.scale) {
    aggr_data$x <- scale(
      aggr_data$x,
      center = FALSE,
      scale  = upwh
    )
  }
  # Input vector of dates
  i_times_vect <- lubridate::ymd_hms(aggr_data$date_time, tz="UTC")
  # Input vector of values
  i_value_vect <- aggr_data$x
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
    
    o_value_vect[o1:o2] <- i_value_vect[i1:i2]
    # Out of the loop if the sequence is finished
    if ((i2+1) > i_len) break
    # Find the new i1 (it must be Mon, 1 WK at 00:00h)
    aux_idx <- get_matrix_index(i_times_vect[i2+1])
    i1 <- (i2 + 1) - (aux_idx - 1)
  }
  # o_times_matx <- array(as.character(o_times_vect), dim = c(24,53*7,nyears))
  o_value_matx <- array(o_value_vect, dim = c(24,53*7,nyears))
  # # Compute the mean through array slices
  # o_mean_matx <- apply(o_value_matx, 2, rowMeans, na.rm=TRUE)
  # Return matrix
  return(o_value_matx)
}

################################################################################
# upper_whisker
# Q3 + IQR
################################################################################

upper_whisker <- function(x) {
  return(
    5/2 * quantile(x,3/4)[[1]] -
    3/2 * quantile(x,1/4)[[1]]
  )
}


################################################################################
# get_heatmap_matrix_v2
################################################################################
get_heatmap_matrix_v2 <- function(
  precomp_dir,
  datasets,
  filenames,
  .scale = TRUE
) {
  
  browser()
  
  # Unique datasets
  unique_datasets <- unique(datasets)
  # Initialize dataframe
  out_df <- data.frame()
  # Dataset loop
  for (dd in unique_datasets) {
    # Open the dataset of precomputed HMMs
    precomp_path <- paste0(precomp_dir, dd, ".csv")
    df <- data.frame(data.table::fread(precomp_path))
    # Select the proper rows and columns
    df <- df[df$file %in% filenames[datasets == dd], 4:8907]
    # Bind
    out_df <- rbind(out_df, df)
  }
  
  # Align time series!: Get a list of arrays, one list element per file
  out <- lapply(fnames, align_time_series, .scale)
  # Turn list of arrays into big array
  out <- unlist(out)
  out <- array(out, dim=c(24,371,length(out)/8904))
  
  # Create matrix from means
  o_mean_mat <- apply(out, 2, rowMeans, na.rm=TRUE)
  # Flip matrix
  o_mean_mat <- o_mean_mat[nrow(o_mean_mat):1,]
  
  # Create matrix from medians
  o_median_mat <- apply(out, 2, matrixStats::rowMedians, na.rm=TRUE)
  # Flip matrix
  o_median_mat <- o_median_mat[nrow(o_median_mat):1,]
  
  # Create matrix from standard deviations
  o_sd_mat <- apply(out, 2, matrixStats::rowSds, na.rm=TRUE)
  # Flip matrix
  o_sd_mat <- o_sd_mat[nrow(o_sd_mat):1,]
  
  # Create matrix from MADs
  o_mad_mat <- apply(out, 2, matrixStats::rowMads, na.rm=TRUE)
  # Flip matrix
  o_mad_mat <- o_mad_mat[nrow(o_mad_mat):1,]
  
  # Coefficient of variance: just divide! 
  o_rsd_mat <- o_sd_mat / o_mean_mat
  # o_rsd_mat[!is.finite(o_rsd_mat)] <- NA
  # Coefficient of MAD (or whatever)
  o_rmad_mat <- o_mad_mat / o_median_mat
  # o_rmad_mat[!is.finite(o_rmad_mat)] <- NA
  
  return(list(mean=o_mean_mat, sd=o_sd_mat, rsd=o_rsd_mat,
              median=o_median_mat, mad=o_mad_mat, rmad=o_rmad_mat))
}


################################################################################
# get_heatmap_matrix
# fnames as data.frame
################################################################################

get_heatmap_matrix <- function(fnames, .scale=TRUE) {
  # Align time series!: Get a list of arrays, one list element per file
  out <- lapply(fnames, align_time_series, .scale)
  # Turn list of arrays into big array
  out <- unlist(out)
  out <- array(out, dim=c(24,371,length(out)/8904))
  
  # Create matrix from means
  o_mean_mat <- apply(out, 2, rowMeans, na.rm=TRUE)
  # Flip matrix
  o_mean_mat <- o_mean_mat[nrow(o_mean_mat):1,]
  
  # Create matrix from medians
  o_median_mat <- apply(out, 2, matrixStats::rowMedians, na.rm=TRUE)
  # Flip matrix
  o_median_mat <- o_median_mat[nrow(o_median_mat):1,]
  
  # Create matrix from standard deviations
  o_sd_mat <- apply(out, 2, matrixStats::rowSds, na.rm=TRUE)
  # Flip matrix
  o_sd_mat <- o_sd_mat[nrow(o_sd_mat):1,]
  
  # Create matrix from MADs
  o_mad_mat <- apply(out, 2, matrixStats::rowMads, na.rm=TRUE)
  # Flip matrix
  o_mad_mat <- o_mad_mat[nrow(o_mad_mat):1,]
  
  # Coefficient of variance: just divide! 
  o_rsd_mat <- o_sd_mat / o_mean_mat
  # o_rsd_mat[!is.finite(o_rsd_mat)] <- NA
  # Coefficient of MAD (or whatever)
  o_rmad_mat <- o_mad_mat / o_median_mat
  # o_rmad_mat[!is.finite(o_rmad_mat)] <- NA
  
  return(list(mean=o_mean_mat, sd=o_sd_mat, rsd=o_rsd_mat,
              median=o_median_mat, mad=o_mad_mat, rmad=o_rmad_mat))
}

################################################################################
# fit_distribution()
#-------------------------------------------------------------------------------
# For the following named distributions, reasonable starting values will be
# computed if start is omitted (i.e. NULL) : "norm", "lnorm", "exp" and "pois",
# "cauchy", "gamma", "logis", "nbinom" (parametrized by mu and size), "geom",
# "beta", "weibull" from the stats package; "invgamma", "llogis", "invweibull",
# "pareto1", "pareto", "lgamma", "trgamma", "invtrgamma" from the actuar package
################################################################################

fit_distribution <- function(m) {
  # Output
  out <- vector("list", length = length(distr_vect))
  names(out)<- distr_vect
  start_params <- list(mu=0, sigma=1, lambda=0, p=2, q=100)
  # Vector m
  vectm <- as.vector(m[is.finite(m)])
  
  # Loop distributions
  for(dd in distr_vect) {
    if (dd != "sgt") {
      # Compute fitting
      tryCatch(
        {
          suppressWarnings(
            out[[dd]] <- fitdist(vectm, dd)
          )
        },
        error = function(e) out[[dd]] <- NULL
      )
    } else {
      # Skewed generalized t distribution
      tryCatch(
        {
          suppressWarnings(
            out[["sgt"]] <- fitdist(vectm, "sgt", start=start_params)
          )
        },
        error = function(e) out[["sgt"]] <- NULL
      )
    }
  }
  
  return(out)
}

################################################################################
# plot_distribution
################################################################################

plot_distribution <- function(
  d,
  format_file = "png",
  file_path   = paste0(getwd(), "/distrib.png"),
  plot_width  = 800,
  plot_height = 600,
  subtitle    = ""
) {
  
  # Format of output files
  if (format_file == "png")
    png(file_path, width = plot_width, height = plot_height)
  if (format_file == "pdf")
    pdf(file_path, width = plot_width, height = plot_height)
  
  # Plot distribution
  plot(d)
  title(subtitle, line=3)
  
  # Shutting down devices
  while(dev.off() != 1) {}
}

################################################################################
# plot_acf
################################################################################

plot_acf <- function(
  m,
  format_file = "png",
  file_path   = paste0(getwd(), "/acf.png"),
  plot_width  = 800,
  plot_height = 600,
  subtitle    = ""
) {

#  browser()
  # Format of output files
  if (format_file == "png")
    png(file_path, width = plot_width, height = plot_height)
  if (format_file == "pdf")
    pdf(file_path, width = plot_width, height = plot_height)
  
  # Plot autocorrelation
  plot(ggAcf(as.vector(m), lag.max = 168) + ggplot2::ggtitle(subtitle))
  
  # Shutting down devices
  while(dev.off() != 1) {}
}

################################################################################
# plot_heatmap_matrix
################################################################################

plot_heatmap_matrix <- function(
  m,
  format_file = "png",
  file_path   = paste0(getwd(), "/heatmap.png"),
  plot_width  = 800,
  plot_height = 600,
  subtitle    = NULL,
  col_palette = "YlOrRd"
) {
  
  # Format of output files
  if (format_file == "png")
    png(file_path, width = plot_width, height = plot_height)
  if (format_file == "pdf")
    pdf(file_path, width = plot_width, height = plot_height)
  
  # Plot heatmap
  image(
    t(m),
    useRaster = TRUE,
    axes      = FALSE,
    col       = hcl.colors(24, col_palette, rev = TRUE),
    zlim      = c(0, 1)
  )
  # Month labels
  m_labels <- rep(NA, 371)
  m_labels[round(seq(1, 371, length.out=25))[seq(2,25,by=2)]] <- month.abb[1:12]
  # Axis
  axis(1, at=seq(0, 1, length.out=371), labels=m_labels, las=0, tick=F)
  axis(2, at=seq(0, 1, length.out=24), labels=23:0, las=2, tick=F)
  # Title
  title(sub=subtitle)
  
  # Shutting down devices
  while(dev.off() != 1) {}
}

################################################################################
# clValid2_heatmaps
################################################################################

clValid2_heatmaps <- function(
  feats_file,
  clValid_dir,
  dir_names,
  dataset_dir,
  num_cluster,
  scale_hmm,
  num_cores = NULL
) {
  print("<<<---| fea2hmp VERSION 3 |--->>>")
  # Check subfolders
  check_subfolders(clValid_dir)
  # Data dir
  data_dir <- paste0(clValid_dir, "data/")
  
  # Open features file
  feats_ext <- tools::file_ext(feats_file)
  
  if (tolower(feats_ext) == "csv") {
    feats <- data.frame(
      data.table::fread(
        file   = feats_file,
        header = TRUE,
        sep    = ","
      )
    )
  } else if (tolower(feats_ext) == "rdata") {
    load(feats_file)
  } else {
    stop("Features file has a wrong extension (.csv or .RData only)")
  }
  
  # LOOP
  fnames <- list.files(path = data_dir, pattern="clValid2$|somObj$")
  fun_export <- c(
    "align_time_series",
    "distr_vect",
    "fit_distribution",
    "get_heatmap_matrix",
    "get_matrix_index",
    "plot_acf",
    "plot_distribution",
    "plot_heatmap_matrix",
    "set_row_conditions",
    "upper_whisker"
  )
  package_list <- c("fitdistrplus", "sgt", "forecast", "ggplot2")
  
  # Setup parallel backend to use many processors
  if (is.null(num_cores)) {
    cores <- parallel::detectCores() - 1
  } else {
    cores <- num_cores
  }
  cl <- parallel::makeCluster(cores, outfile = "")
  doParallel::registerDoParallel(cl)

  o <- foreach::foreach(
    ff        = 1:length(fnames),
    .export   = fun_export,
    .packages = package_list
  ) %:% foreach::foreach(
    cc        = 1:num_cluster,
    .inorder  = FALSE,
    .export   = fun_export,
    .packages = package_list
  ) %dopar% {
      
  # for(ff in 1:length(fnames)) {
  #   for(cc in 1:num_cluster) {

    ############################
    ### INITIALIZATION STUFF ###
    ############################
    # Print stuff
    print(paste0("File ", ff, " - Cluster ", cc))
    # Working file name
    w_fname <- fnames[ff]
    # Working config file name
    w_cname <- paste0(tools::file_path_sans_ext(w_fname), ".config")
    # Load configuration file
    w_cpath <- paste0(data_dir, w_cname)
    load(w_cpath)
    # Set the required feats
    row_conditions <- set_row_conditions(feats, analysis_type)
    w_feats <- feats[row_conditions, c("data_set", "file")]
    # Free 2 GB of data :-)
    rm(feats)
    # Open cluster object
    w_fpath <- paste0(data_dir, w_fname)
    load(w_fpath)
    
    ##############################
    ### GETTING THE CLUSTERING ###
    ##############################
    # HIERARCHICAL
    if (analysis_type$mm == "hierarchical") {
      cluster_list <-
        cutree(o@clusterObjs[["hierarchical"]], k=num_cluster)
    }
    # K-MEANS
    if (analysis_type$mm == "kmeans") {
      cluster_list <-
        o@clusterObjs[["kmeans"]][[as.character(num_cluster)]][["cluster"]]
    }
    # DIANA
    if (analysis_type$mm == "diana") {
      cluster_list <-
        cutree(o@clusterObjs[["diana"]], k=num_cluster)
    }
    # FANNY
    if (analysis_type$mm == "fanny") {
      cluster_list <-
        o@clusterObjs[["fanny"]][[as.character(num_cluster)]]$clustering
    }
    # SOM
    if (analysis_type$mm == "som") {
      if (class(o) == "kohonen") {
        cluster_list <- o$unit.classif
      } else {
        cluster_list <-
          o@clusterObjs[["som"]][[as.character(num_cluster)]]$unit.classif
      }
    }
    # PAM
    if (analysis_type$mm == "pam") {
      cluster_list <-
        o@clusterObjs[["pam"]][[as.character(num_cluster)]]$clustering
    }
    # SOTA
    if (analysis_type$mm == "sota") {
      cluster_list <-
        o@clusterObjs[["sota"]][[as.character(num_cluster)]]$clust
    }
    # CLARA
    if (analysis_type$mm == "clara") {
      cluster_list <-
        o@clusterObjs[["clara"]][[as.character(num_cluster)]]$clustering
    }
    # MODEL-BASED
    if (analysis_type$mm == "model") {
      cluster_list <-
        o@clusterObjs[["model"]][[as.character(num_cluster)]]$classification
    }
    
    ###############################################
    ### GETTING MATRICES AND DATA FOR PLOTTING ###
    ###############################################
    
    # Get cluster indices
    idx <- cluster_list == cc
    
    # # Get heatmap matrix
    # m <- get_heatmap_matrix_v2(
    #   precomp_dir = dataset_dir,
    #   datasets    = w_feats$data_set[idx],
    #   filenames   = w_feats$file[idx],
    #   .scale      = scale_hmm
    # )
    
    # Set vector of paths
    paths_vector <-
      paste0(dataset_dir[w_feats$data_set[idx]], w_feats$file[idx], ".RData")
    
    # Get heatmap matrix
    m <- get_heatmap_matrix(paths_vector, .scale = scale_hmm)
    # Get distributions
    d <- list()
    d$mean   <- fit_distribution(m$mean)
    d$sd     <- fit_distribution(m$sd)
    d$rsd    <- fit_distribution(m$rsd)
    d$median <- fit_distribution(m$median)
    d$mad    <- fit_distribution(m$mad)
    d$rmad   <- fit_distribution(m$rmad)
    
    # Cluster loop
    fname <- print(paste0(tools::file_path_sans_ext(w_fname), "-", cc))
    
    ##############################
    ### CREATION OF FILE PATHS ###
    ##############################
    # File paths: heatmaps (data)
    hd_path <- paste0(dir_names[["dplot"]], "hd_", fname, ".RData")
    # File paths: distributions (data)
    dd_path <- paste0(dir_names[["dplot"]], "dd_", fname, ".RData")
    # File paths: heatmaps (plots)
    hp_path <- c() 
    kp_path <- c()
    for (ii in 1:6) {
      hp_path[ii] <- paste0(dir_names[["hmp"]], "hp", ii, "_", fname, ".png")
      kp_path[ii] <- paste0(dir_names[["hmp"]], "kp", ii, "_", fname, ".png")
    }

    # File paths: distributions (plots)
    dp_path <- list(c(), c(), c(), c(), c(), c())
    for (dd in 1:length(distr_vect)) {
      for (ii in 1:6) {
        dp_path[[ii]][dd] <-
          paste0(dir_names[["distr"]], "dp", ii, "_", dd, "_", fname, ".png")
      }
    }
    # File paths: autocorrelations (plots)
    ap_path <- c()
    for (ii in 1:6) {
      ap_path[ii] <- paste0(dir_names[["acf"]], "ap", ii, "_", fname, ".png")
    }

    ####################################
    ### CREATION AND SAVING OF PLOTS ###
    ####################################
    # Save heatmap matrices
    save(m, idx, file = hd_path)
    # Save distribution lists
    save(d, idx, file = dd_path)
    
    # Useful vectors
    st   <- c("mean", "sd", "cvar", "median", "mad", "rmad")
    hcol <- c("YlOrRd", "Blues", "Greens", "YlOrRd", "Blues", "Greens")
    # # Plot heatmaps: outliers are left blank
    # for(ii in 1:6) {
    #   plot_heatmap_matrix(
    #     m           = m[[ii]],
    #     format_file = "png",
    #     file_path   = hp_path[ii],
    #     plot_width  = 1200,
    #     plot_height = 900,
    #     subtitle    = paste(st[ii], fname),
    #     col_palette = hcol[ii]
    #   )
    # }
    # Plot heatmaps: outliers are color-filled
    for(ii in 1:6) {
      mm <- m[[ii]]
      mm[mm > 1] <- 1
      plot_heatmap_matrix(
        m           = mm,
        format_file = "png",
        file_path   = kp_path[ii],
        plot_width  = 1200,
        plot_height = 900,
        subtitle    = paste(st[ii], fname),
        col_palette = hcol[ii]
      )
    }
    # Plot distributions
    for(ii in 1:6) {
      for(jj in 1:9) {
        if(!is.null(d[[ii]][[jj]])) {
          plot_distribution(
            d           = d[[ii]][[jj]],
            format_file = "png",
            file_path   = dp_path[[ii]][jj],
            plot_width  = 800,
            plot_height = 600,
            subtitle    = paste(st[ii], distr_vect[jj], fname)
          )
        }
      }
    }
    # Plot autocorrelations
    for(ii in 1:6) {
      plot_acf(
        m           = m[[ii]],
        format_file = "png",
        file_path   = ap_path[ii],
        plot_width  = 800,
        plot_height = 600,
        subtitle    = paste(st[ii], fname)
      )
    }

    #} # CURLY BRACKET FOR FOR-LOOPS
  }
  
  # Stop parallelization
  parallel::stopCluster(cl)
}
