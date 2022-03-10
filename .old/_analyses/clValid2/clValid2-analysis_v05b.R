## THIS VERSION SCALES DATA AND USES A DIFFERENT NUMBER OF CLUSTER PER DATASET

library(foreach)
library(clValid2)
library(mclust)

feats_vers <- "go4_pre_v1"

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
    out_dir   <- "G:/Mi unidad/WHY/Analyses/clValid2/"
    source("G:/Mi unidad/WHY/Github/why-T2.1/_analyses/selectable_variables.R")
  }
  if (.Platform$OS.type == "unix") {
    feats_dir <- "/home/ubuntu/carlos.quesada/disk/features/go4_pre_21.12.26/"
    out_dir   <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2021.12.27_pre-post/data/"
    source("/home/ubuntu/carlos.quesada/analyses/selectable_variables.R")
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
  sum_pday   <- dset_keys[[extr_digits[2]]]$sum_pday
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
    feats$is_household %in% is_hhold &
    feats$sum_per_day > sum_pday &
    feats$minimum >= 0

  row_names <- paste0(feats$data_set, "_", feats$file)
  feats <- feats[row_conditions,]
  feats <- subset(feats, select = ft_set)
  feats <- as.matrix(feats)
  ## SCALE
  feats <- scale(feats)
  row.names(feats) <- row_names[row_conditions]

  o <- clValid2::clValid(
    obj        = feats,
    nClust     = nClust,
    clMethods  = clMethods,
    validation = valid,
    maxitems   = nrow(feats) + 1
  )

  filename <- paste0(out_dir, "o_", analysis_type, ".clValid2")
  save("o", file = filename)
}

################################################################################
##  CALL TO THE FUNCTION
################################################################################

cluster_codes <- vector()

# ff: SETS OF FEATURES
for (ff in 1) {
  # dd: DATASETS
  for (dd in c(7)) {
	# mm: CLUSTER METHODS
    for (mm in c(5)) {
	  # vv: VALIDATION METHODS
      for (vv in 1:2) {
	    # cc: NUMBER OF CLUSTERS
	    if (dd == 2) cc <- 1 # LCL 16 cl (cc <- 3)
		if (dd == 3) cc <- 1 # ISS 16 cl (cc <- 3)
		if (dd == 4) cc <- 1 # GOI 30 cl (cc <- 4)
		if (dd == 5) cc <- 1 # POR  6 cl (cc <- 5)
		if (dd == 7) cc <- 1 # all 40 cl (cc <- 6)
        cluster_codes <- c(cluster_codes, as.numeric(paste0(ff, dd, mm, vv, cc)))
      }
    }
  }
}

for (ii in cluster_codes) {
  get_cluster_analysis(ii)
}
