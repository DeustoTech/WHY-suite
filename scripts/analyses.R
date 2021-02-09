#' #############################################################################
#' ##                                                                         ##
#' ##                        ANALYSES OF FEATURES                             ##
#' ##                                                                         ##
#' #############################################################################

library(whyT2.1)

################################################################################
analysis_val <- 1
feat_vers <- "1.0"
################################################################################

analysis_fn <- function(analysis_val, feat_path) {
  if (analysis_val == 1) {
    print("Hi!")
  }
}

# Folder of the file of features
feat_dir  <- "G:/.shortcut-targets-by-id/1g1D2rJfAektwZCB-O_F0EHxDYHxJmhmc/20WHY 
datasets/Features/"
# Path to the file of features
feat_path <- paste(feat_dir, "feats_v", feat_vers, ".csv")
# Call to the analysis function
analysis_out <- analysis_fn(analysis_val, feat_path)