get_cluster_analysis <- function(analysis_type) {
  
  ##############################################################################
  ##  ANALYSIS USING THE clValid PACKAGE
  ##  R file with the analysis functions of the Rmd file
  ##############################################################################
  
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
  
  cluster_set_1 <- c(5, 10, 15, 20, seq(22, 40, 2), 50, 60, 70)
  
  if (analysis_type == 1) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("hierarchical", "kmeans", "diana", "fanny", "som", "pam", "sota", "clara", "model")
    is_hhold   <- 1
    nClust     <- 10:16
    validation <- "stability"
  }
  
  if (analysis_type == 2) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("hierarchical", "kmeans", "diana", "fanny", "som", "pam", "sota", "clara", "model")
    is_hhold   <- 1
    nClust     <- 8:24
    validation <- "stability"
  }
  
  
  if (analysis_type == 3) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("hierarchical", "kmeans", "diana", "fanny", "som", "pam", "sota", "clara", "model")
    is_hhold   <- 1
    nClust     <- 2:40
    validation <- "stability"
  }
  
  if (analysis_type == 4) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("hierarchical", "kmeans", "diana", "fanny", "som", "pam", "sota", "clara", "model")
    is_hhold   <- 1
    nClust     <- 2:90
    validation <- "stability"
  }
  
  #-----------------------------------------------------------------------------
  
  if (analysis_type == 11) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("hierarchical")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 12) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("kmeans")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 13) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("diana")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 14) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("fanny")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 15) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("som")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 16) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("pam")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 17) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("sota")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 18) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("clara")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 19) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("model")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  # ----------------------------------------------------------------------------
  
  if (analysis_type == 21) {
    feats_vers <- "v1.11"
    dsets      <- "lcl"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("hierarchical")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 22) {
    feats_vers <- "v1.11"
    dsets      <- "lcl"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("kmeans")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 23) {
    feats_vers <- "v1.11"
    dsets      <- "lcl"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("diana")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 24) {
    feats_vers <- "v1.11"
    dsets      <- "lcl"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("fanny")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 25) {
    feats_vers <- "v1.11"
    dsets      <- "lcl"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("som")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 26) {
    feats_vers <- "v1.11"
    dsets      <- "lcl"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("pam")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 27) {
    feats_vers <- "v1.11"
    dsets      <- "lcl"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("sota")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 28) {
    feats_vers <- "v1.11"
    dsets      <- "lcl"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("clara")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 29) {
    feats_vers <- "v1.11"
    dsets      <- "lcl"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("model")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  # ----------------------------------------------------------------------------
  
  if (analysis_type == 31) {
    feats_vers <- "v1.11"
    dsets      <- "iss"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("hierarchical")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 32) {
    feats_vers <- "v1.11"
    dsets      <- "iss"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("kmeans")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 33) {
    feats_vers <- "v1.11"
    dsets      <- "iss"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("diana")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 34) {
    feats_vers <- "v1.11"
    dsets      <- "iss"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("fanny")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 35) {
    feats_vers <- "v1.11"
    dsets      <- "iss"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("som")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 36) {
    feats_vers <- "v1.11"
    dsets      <- "iss"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("pam")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 37) {
    feats_vers <- "v1.11"
    dsets      <- "iss"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("sota")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 38) {
    feats_vers <- "v1.11"
    dsets      <- "iss"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("clara")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 39) {
    feats_vers <- "v1.11"
    dsets      <- "iss"
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("model")
    is_hhold   <- 1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  #-----------------------------------------------------------------------------
  
  if (analysis_type == 41) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("hierarchical")
    is_hhold   <- 0:1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 42) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("kmeans")
    is_hhold   <- 0:1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 43) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("diana")
    is_hhold   <- 0:1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 44) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("fanny")
    is_hhold   <- 0:1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 45) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("som")
    is_hhold   <- 0:1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 46) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("pam")
    is_hhold   <- 0:1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 47) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("sota")
    is_hhold   <- 0:1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 48) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("clara")
    is_hhold   <- 0:1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  if (analysis_type == 49) {
    feats_vers <- "v1.11"
    dsets      <- c("go2", "meg")
    feats_set  <- 1
    imp_na_pct <- 0.1
    clMethods  <- c("model")
    is_hhold   <- 0:1
    nClust     <- cluster_set_1
    validation <- "stability"
  }
  
  ##############################################################################
  ##  FUNCTION to choose a set of features
  ##############################################################################
  
  get_feats_set <- function(idx) {
    if (idx == 1) {
      x <- c(
        "rel_mean_00h04hspr", "rel_mean_04h08hspr", "rel_mean_08h12hspr",
        "rel_mean_12h16hspr", "rel_mean_16h20hspr", "rel_mean_20h00hspr",
        "rel_mean_00h04hsum", "rel_mean_04h08hsum", "rel_mean_08h12hsum",
        "rel_mean_12h16hsum", "rel_mean_16h20hsum", "rel_mean_20h00hsum",
        "rel_mean_00h04haut", "rel_mean_04h08haut", "rel_mean_08h12haut",
        "rel_mean_12h16haut", "rel_mean_16h20haut", "rel_mean_20h00haut",
        "rel_mean_00h04hwin", "rel_mean_04h08hwin", "rel_mean_08h12hwin",
        "rel_mean_12h16hwin", "rel_mean_16h20hwin", "rel_mean_20h00hwin",
        "rel_mean_weekday_pday")
    }
    if (idx == 2) {
      x <- c(
        "peak_hour_1", "off_peak_hour_1", "peak_month", "off_peak_month",
        "peak_weekday_pday")
    }
    if (idx == 3) {
      x <- c(
        "mean", "entropy", "seasonal_strength1", "seasonal_strength2",
        "seasonal_strength3", "ac_day_1", "ac_day_7", "ac_day_28")
    }
    if (idx == 4) {
      x <- c() # TO BE FILLED
    }
    return(x)
  }
  
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
  feats <- subset(feats, select = get_feats_set(feats_set))
  feats <- as.matrix(feats)
  
  o <- clValid::clValid(
    obj        = feats,
    nClust     = nClust,
    clMethods  = clMethods,
    validation = validation,
    maxitems   = nrow(feats) + 1
  )
  
  filename <- paste0(out_dir, "o_", analysis_type, ".clValid")
  save("o", file = filename)
}

################################################################################
##  CALL TO THE FUNCTION
################################################################################

library(foreach)
library(clValid)
library(mclust)

# Setup parallel backend to use many processors
cores <- 3 #parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

foreach::foreach(
  ii             = c(45, 47, 48), #c(11:19, 21:29, 31:39, 41:49),
  .inorder       = FALSE,
  .errorhandling = "remove",
  .packages      = c("clValid", "mclust")
) %dopar% {
  get_cluster_analysis(ii)
}

# Stop parallelization
parallel::stopCluster(cl)
