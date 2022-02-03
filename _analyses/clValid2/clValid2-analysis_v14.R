## THIS VERSION SCALES DATA AND USES A DIFFERENT NUMBER OF CLUSTER PER DATASET
# TO BE USED WITH "selectable_variables_v2.R"

library(foreach)
library(clValid2)
library(mclust)

#######################################
##  Function to select the analyses  ##
#######################################

call_clValid2 <- function(analysis_type, feats, feats_set) {

  ##############################################################################
  ##  ANALYSIS USING THE clValid PACKAGE
  ##  R file with the analysis functions of the Rmd file
  ##############################################################################

  # Set file name
  ff_name <- analysis_type$ff
  key     <- unique(analysis_type$dd$key)
  len_key <- length(key)
  dd_name <- ifelse(len_key == 1, key, paste0(len_key, "varDS")) 
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
  row_conditions <- rep(TRUE, nrow_feats)
  
  ### CONDITIONS ---------------------------------------------------------------
  
  # Condition 1: Dataset key
  if (is.null(analysis_type$dd$key))
    cond_key <- TRUE
  else
    cond_key <- feats$data_set %in% analysis_type$dd$key
  
  # Condition 2: Percentage of imputed samples in TS
  if (is.null(analysis_type$dd$rel_imputed_na))
    cond_imp <- TRUE
  else
    cond_imp <- feats$rel_imputed_na < analysis_type$dd$rel_imputed_na
  
  # Condition 3: Household or not
  if (is.null(analysis_type$dd$is_household))
    cond_hhd <- TRUE
  else
    cond_hhd <- feats$is_household %in% analysis_type$dd$is_household
  
  # Condition 4: Type of tariff
  if (is.null(analysis_type$dd$ref_atr_tariff))
    cond_trf <- TRUE
  else
    cond_trf <- substr(feats$ref_atr_tariff,1,1) %in% analysis_type$dd$tariff
  
  # Mandatory condition
  cond_min <- feats$minimum >= 0
  
  # Check all conditions
  row_conditions <- row_conditions & (
    cond_key & cond_imp & cond_hhd & cond_trf & cond_min
  )
  
  # Discard NA values
  row_conditions[is.na(row_conditions)] <- FALSE
  
  ### --------------------------------------------------------------------------

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
    call_clValid2(cluster_code_ii, feats, feats_set)
  }
}

cluster_features(
  feats_file = "/home/ubuntu/carlos.quesada/disk/features/feats_go4_pre.csv",
  output_dir = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.02_go4-pre-only-2-tariffs/data/",
  # SELECTOR OF SETS OF FEATURES
  # e.g. "sAggr", "peaks", "tsfAC", "cat22", "sAggrP6", "sAggrDRM"
  ff_sel = c("sAggr", "sAggrP6", "sAggrDRM"),
  # SELECTOR OF DATASETS
  # Each sublist is a different analysis
  # Each analysis defines "key", "is_household", "rel_imputed_na", and "tariff"
  dd_sel = list(
    list(key="goi", is_household=NULL, rel_imputed_na=0.05, tariff="2")
  ),
  # SELECTOR OF CLUSTER METHODS
  # e.g. "hierarchical", "kmeans", "diana", "fanny", "som", "pam", "sota", "clara", "model"
  mm_sel = c("som"),
  # SELECTOR OF VALIDATION METHODS
  # e.g. "internal", "stability", "biological"
  vv_sel = c("internal"),
  # SELECTOR OF NUMBER OF CLUSTERS
  cc_sel = c(30)
)
