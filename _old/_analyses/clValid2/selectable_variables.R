################################################################################
##  ROW CONDITIONS (FUNCTION)
################################################################################

row_conditions_fun <- function(feats, n2) {
  return(
    feats$data_set %in% dset_keys[[n2]]$keys &
    feats$imputed_na_pct < dset_keys[[n2]]$imp_na_pct &
    feats$is_household %in% dset_keys[[n2]]$is_hhold &
    feats$sum_per_day > dset_keys[[n2]]$sum_pday &
    feats$minimum >= 0
  )
}

################################################################################
##  SELECTABLE VARIABLES
################################################################################

# 1st DIGIT: SETS OF FEATURES --------------------------------------------------
feats_set <- list(
  # TYPE 1
  c(
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
  # TYPE 2
  c(
    "peak_hour_1", "off_peak_hour_1", "peak_month", "off_peak_month",
    "peak_weekday_pday"
  ),
  # TYPE 3
  c(
    "mean", "entropy", "seasonal_strength1", "seasonal_strength2",
    "seasonal_strength3", "ac_day_1", "ac_day_7", "ac_day_28"
  ),
  # TYPE 4 - CATCH22
  c(
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
  # TYPE 5
  c(
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
  )
)

# 2nd DIGIT: DATASETS ----------------------------------------------------------
dset_keys <- list(
  # TYPE 1
  list(
    keys       = c("go2", "meg", "goi"),
    is_hhold   = 1,
    imp_na_pct = 0.1,
    sum_pday   = 0.1
  ),
  # TYPE 2
  list(
    keys       = c("lcl"),
    is_hhold   = 1,
    imp_na_pct = 0.1,
    sum_pday   = 0.1
  ),
  # TYPE 3
  list(
    keys       = c("iss"),
    is_hhold   = 1,
    imp_na_pct = 0.1,
    sum_pday   = 0.1
  ),
  # TYPE 4
  list(
    keys       = c("go2", "meg", "goi"),
    is_hhold   = 0:1,
    imp_na_pct = 0.1,
    sum_pday   = 0.1
  ),
  # TYPE 5
  list(
    keys       = c("por"),
    is_hhold   = c(0, 1, NA),
    imp_na_pct = 0.1,
    sum_pday   = 0.1
  ),
  # TYPE 6
  list(
    keys       = c("nee"),
    is_hhold   = c(0, 1, NA),
    imp_na_pct = 0.1,
    sum_pday   = 0.1
  ),
  # TYPE 7 - ALL
  list(
    keys       = c("go2", "meg", "lcl", "iss", "por", "nee", "goi"),
    is_hhold   = c(0, 1, NA),
    imp_na_pct = 0.1,
    sum_pday   = 0.1
  )
)

# 3rd DIGIT: CLUSTERING METHOD -------------------------------------------------
cluster_methods <- c(
  # TYPE 1
  "hierarchical",
  # TYPE 2
  "kmeans",
  # TYPE 3
  "diana",
  # TYPE 4
  "fanny",
  # TYPE 5
  "som",
  # TYPE 6
  "pam",
  # TYPE 7
  "sota",
  # TYPE 8
  "clara",
  # TYPE 9
  "model"
)

# 4th DIGIT: VALIDATION --------------------------------------------------------
validation <- list(
  # TYPE 1
  i = "internal", 
  # TYPE 2
  s = "stability", 
  # TYPE 3
  b = "biological"
)

# 5th DIGIT: NUMBER OF CLUSTERS ------------------------------------------------
cluster_set <- list(
  # TYPE 1
  c(5, 10, 15, 20, seq(22, 40, 2), 50, 60, 70),
  # TYPE 2
  24,
  # TYPE 3 (ISS & LCL)
  16,
  # TYPE 4 (GOI)
  30,
  # TYPE 5 (POR)
  6,
  # TYPE 6 (all)
  40
)