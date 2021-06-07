library(foreach)
library(clValid2)
library(mclust)

feats_vers <- "v1.12"

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
    feats_dir <- "/home/ubuntu/carlos.quesada/disk/features/"
    out_dir   <- "/home/ubuntu/carlos.quesada/analyses/clValid2/"
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
    feats$sum_per_day > sum_pday

  row.names(feats) <- paste0(feats$data_set, "_", feats$file)
  feats <- feats[row_conditions,]
  feats <- subset(feats, select = ft_set)
  feats <- as.matrix(feats)

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

# Setup parallel backend to use many processors
cores <- 3 #parallel::detectCores() - 1
cl <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

foreach::foreach(
  ii             = c (
    c(11,12,15,                     41,42,45) * 100 + 10011,
    c(11,12,15, 21,22,25, 31,32,35, 41,42,45) * 100 + 10021
    ),
  .inorder       = FALSE,
  .errorhandling = "remove",
  .packages      = c("clValid2", "mclust")
) %dopar% {
  get_cluster_analysis(ii)
}

# Stop parallelization
parallel::stopCluster(cl)
