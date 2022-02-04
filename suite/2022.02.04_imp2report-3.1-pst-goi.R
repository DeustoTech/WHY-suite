source("clValid2-analysis-heatmaps_v01P.R")

################################################################################
##  VARIABLE DEFINITION
################################################################################

# "actions":
# [1] "cluster" for cluster analysis
# [2] "heatmap" for heatmap plots
# [3] "report"  for cluster report
actions     <- c("report")
clValid_dir <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.02_go4-pst-only-2-tariffs/data/"
dataset_dir <- c(
  goi="/home/ubuntu/carlos.quesada/disk/go4_pst/imp/"
)
hmm_dir     <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.02_go4-pst-only-2-tariffs/hmm/"
hmp_dir     <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.02_go4-pst-only-2-tariffs/hmp/"
feats_file  <- "/home/ubuntu/carlos.quesada/disk/features/feats_go4_pst.csv"
ff_sel      <- c("sAggrP6", "sAggrDRM")
# Instructions for "dd_sel" variable:
# Each sublist is an OR, each element within the sublist is an AND
# Each NULL element within the sublist means ALL TRUE
dd_sel      <- list(
  list(key="goi", is_household=NULL, rel_imputed_na=0.05, tariff="2")
)
mm_sel      <- c("som")
vv_sel      <- c("internal")
cc_sel      <- c(30)
rmd_title   <- "Cluster Report v3.1: POST-COVID GoiEner"
rmd_dir     <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.02_go4-pst-only-2-tariffs/report/"

################################################################################
##  imp2report() CALL
################################################################################

imp2report(
  actions     = actions,
  clValid_dir = clValid_dir,
  dataset_dir = dataset_dir,
  hmm_dir     = hmm_dir,
  hmp_dir     = hmp_dir,
  feats_file  = feats_file,
  ff_sel      = ff_sel,
  dd_sel      = dd_sel,
  mm_sel      = mm_sel,
  vv_sel      = vv_sel,
  cc_sel      = cc_sel,
  rmd_title   = rmd_title,
  rmd_dir     = rmd_dir
)