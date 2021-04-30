is_an_outlier <- function() {
  # FILE TO CHECK THAT k-MEANS IS PERFORMING OK
  feats_dir  <- "G:/Mi unidad/WHY/Features/"
  out_dir    <- "G:/Mi unidad/WHY/Analyses/clValid/"

  feats_vers <- "v1.11"

  analysis_type <- 14111

  feats_path <- paste0(feats_dir, "feats_", feats_vers, ".csv")

  feats <- data.table::fread(
    file   = feats_path,
    header = TRUE,
    sep    = ","
  )

  # Extract digits
  extr_digits <- as.integer(
    substring(
      analysis_type,
      seq(nchar(analysis_type)),
      seq(nchar(analysis_type))
    )
  )

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
    list(keys = c("go2", "meg"), is_hhold = 1,   imp_na_pct = 0.1),
    # TYPE 2
    list(keys = c("lcl"),        is_hhold = 1,   imp_na_pct = 0.1),
    # TYPE 3
    list(keys = c("iss"),        is_hhold = 1,   imp_na_pct = 0.1),
    # TYPE 4
    list(keys = c("go2", "meg"), is_hhold = 0:1, imp_na_pct = 0.1)
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
    c(5, 10, 15, 20, seq(22, 40, 2), 50, 60, 70)
  )

################################################################################


  # SET CASE OF ANALYSIS
  ft_set     <- feats_set[[extr_digits[1]]]
  dsets      <- dset_keys[[extr_digits[2]]]$keys
  imp_na_pct <- dset_keys[[extr_digits[2]]]$imp_na_pct
  is_hhold   <- dset_keys[[extr_digits[2]]]$is_hhold
  clMethods  <- cluster_methods[[extr_digits[3]]]
  valid      <- validation[[extr_digits[4]]]
  nClust     <- cluster_set[[extr_digits[5]]]

  row_conditions <-
    feats$data_set %in% dsets &
    feats$imputed_na_pct < imp_na_pct &
    feats$is_household %in% is_hhold

  feats <- feats[row_conditions,]
  feats_names <- paste0(feats$data_set, "_", feats$file)
  # feats <- subset(feats, select = ft_set)
  # feats <- as.matrix(feats)
  # row.names(feats) <- feats_names

  load("G:/Mi unidad/WHY/Analyses/clValid/data/o_14111.clValid")
  hier.k100 <- cutree(o@clusterObjs$hierarchical, k = 100)
  elems_per_cl <- sapply(1:100, function(x) sum(hier.k100 == x))
  print(elems_per_cl)

  idx <- which(hier.k100 == 3)[1]
  this_name <- feats_names[idx]
  folder_file <- stringr::str_split(this_name, "_")
  load(paste0("G:/Mi unidad/WHY/Datasets/", folder_file[[1]][1], "/ext/",
              folder_file[[1]][2], ".RData"))
  whyT2.1::plot_dataframe(edf$df)

}

is_an_outlier()
