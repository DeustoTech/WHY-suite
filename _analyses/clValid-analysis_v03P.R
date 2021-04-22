library(foreach)
library(clValid)
library(mclust)

feats_vers = "v1.11"

get_cluster_analysis <- function(analysis_type) {
  
  ##############################################################################
  ##  ANALYSIS USING THE clValid PACKAGE
  ##  R file with the analysis functions of the Rmd file
  ##############################################################################
  
  print(analysis_type)
  
  ##############################################################################
  ##  Constants
  ##############################################################################
  
  # User defined variables
  if (.Platform$OS.type == "windows") {
    feats_dir <- "G:/Mi unidad/WHY/Features/"
    out_dir   <- "G:/Mi unidad/WHY/Analyses/clValid/"
  }
  if (.Platform$OS.type == "unix") {
    feats_dir <- "/home/ubuntu/carlos.quesada/disk/features/"
    out_dir   <- "/home/ubuntu/carlos.quesada/analyses/clValid/"
  }
  
  ##############################################################################
  ##  ANALYSIS TYPE
  ##############################################################################
  
  # Extract digits
  extr_digits <- as.integer(
    substring(
      analysis_type,
      seq(nchar(analysis_type)),
      seq(nchar(analysis_type))
    )
  )
  
  # SET CASE OF ANALYSIS
  ft_set     <- feats_set[[extr_digits[1]]]
  dsets      <- dset_keys[[extr_digits[2]]]$keys
  imp_na_pct <- dset_keys[[extr_digits[2]]]$imp_na_pct
  is_hhold   <- dset_keys[[extr_digits[2]]]$is_hhold
  clMethods  <- cluster_methods[[extr_digits[3]]]
  valid      <- validation[[extr_digits[4]]]
  nClust     <- cluster_set[[extr_digits[5]]]
  
  ##############################################################################
  ##  MAIN clValid ANALYSIS
  ##############################################################################
  
  feats_path <- paste0(feats_dir, "feats_", feats_vers, ".csv")
  
  feats <- data.table::fread(
    file   = feats_path,
    header = TRUE,
    sep    = ","
  )
  
  row_conditions <- 
    feats$data_set %in% dsets &
    feats$imputed_na_pct < imp_na_pct &
    feats$is_household %in% is_hhold
  
  row.names(feats) <- paste0(feats$data_set, "_", feats$file)
  feats <- feats[row_conditions,]
  # feats <- feats[1:50,]
  feats <- subset(feats, select = ft_set)
  feats <- as.matrix(feats)
  
  o <- clValid::clValid(
    obj        = feats,
    nClust     = nClust,
    clMethods  = clMethods,
    validation = valid,
    maxitems   = nrow(feats) + 1
  )
  
  filename <- paste0(out_dir, "o_", analysis_type, ".clValid")
  save("o", file = filename)
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
##  CALL TO THE FUNCTION
################################################################################

# Setup parallel backend to use many processors
cores <- 9 #parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

foreach::foreach(
  ii             = c(11:19, 21:29, 31:39, 41:49) * 100 + 10011,
  .inorder       = FALSE,
  .errorhandling = "remove",
  .packages      = c("clValid", "mclust")
) %dopar% {
  get_cluster_analysis(ii)
}

# Stop parallelization
parallel::stopCluster(cl)
