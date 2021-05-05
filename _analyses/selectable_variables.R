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
  # TYPE 4
  c(
    # CATCH22 - TO BE FILLED
  )
)

# 2nd DIGIT: DATASETS ----------------------------------------------------------
dset_keys <- list(
  # TYPE 1
  list(keys = c("go2", "meg"), is_hhold = 1,   imp_na_pct = 0.1, sum_pday = 0.1),
  # TYPE 2
  list(keys = c("lcl"),        is_hhold = 1,   imp_na_pct = 0.1, sum_pday = 0.1),
  # TYPE 3
  list(keys = c("iss"),        is_hhold = 1,   imp_na_pct = 0.1, sum_pday = 0.1),
  # TYPE 4
  list(keys = c("go2", "meg"), is_hhold = 0:1, imp_na_pct = 0.1, sum_pday = 0.1)
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
  24
)