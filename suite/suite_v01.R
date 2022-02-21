################################################################################
##  CARLOS QUESADA GRANJA
##  FEBRUARY 17, 2022
##  UNIVERSIDAD DE DEUSTO
##  ---------------------
##  THE x2y PROCESS:
##  raw > imp > fea > clu > hmp > rep
################################################################################

# FILES IN SUBFOLDER 'src'
raw2imp_src <- "goiener-ext-3v3.R"
imp2fea_src <- "new_feats.R"
fea2hmp_src <- "clValid2-analysis-heatmaps_v01P.R"
hmp2rep_src <- "clValid2-summary_report_v03.Rmd"
no_file_src <- "no-file.png"

################################
## RAW FILES TO IMPUTED FILES ##
################################
raw2imp <- function(
  raw_dir,
  imp_dir,
  dset_key,
  mdata_file,
  from_date = "first",
  to_date   = "last",
  min_yrs   = 1,
  wwgen     = FALSE
) {
  source(paste(getwd(), "src", raw2imp_src, sep="/"))
  print("## RAW TO IMPUTED ##")
  extend_dataset_v2(
    input_folder            = raw_dir,
    output_folder           = imp_dir,
    dset_key                = dset_key,
    metadata_files          = mdata_file,
    working_with_generation = wwgen,
    min_years               = min_yrs,
    from_date               = from_date,
    to_date                 = to_date
  )
}

###############################
## IMPUTED FILES TO FEATURES ##
###############################
imp2fea <- function(
  imp_dir,
  fea_file,
  analysis = "extra"
) {
  source(paste(getwd(), "src", imp2fea_src, sep="/"))
  print("## IMPUTED TO FEATURES ##")
  get_features(
    input_folder     = imp_dir,
    output_path      = fea_file,
    type_of_analysis = analysis
  )
}

# Bind files together

######################
## CLUSTER ANALYSIS ##
######################
fea2clu <- function(
  fea_file,
  clu_dir,
  ff_sel,
  dd_sel,
  mm_sel,
  vv_sel,
  cc_sel
) {
  source(paste(getwd(), "src", fea2hmp_src, sep="/"))
  print("## CLUSTER ANALYSES ##")
  cluster_features(
    feats_file = fea_file,
    output_dir = clu_dir,
    ff_sel     = ff_sel,
    dd_sel     = dd_sel,
    mm_sel     = mm_sel,
    vv_sel     = vv_sel,
    cc_sel     = cc_sel
  )
}

############################
## GENERATION OF HEATMAPS ##
############################
clu2hmp <- function(
  fea_file,
  clu_dir,
  hmm_dir,
  hmp_dir,
  dset_dir,
  cc,
  cores = NULL
) {
  source(paste(getwd(), "src", fea2hmp_src, sep="/"))
  print("## HEATMAP PLOTS ##")
  clValid2_heatmaps(
    feats_file  = fea_file,
    clValid_dir = clu_dir,
    hmm_dir     = hmm_dir,
    hmp_dir     = hmp_dir,
    dataset_dir = dset_dir,
    num_cluster = cc,
    scale_hmm   = TRUE,
    num_cores   = cores
  )
}

##############################
## CREATION OF BASIC REPORT ##
##############################
hmp2rep <- function(
  rep_title,
  rep_file,
  hmp_dir,
  hmm_dir,
  ff,
  dd,
  mm,
  cc
) {
  print("## RMarkDown REPORT ##")
  params_list <- list(
    rmd_title   = rep_title,
    hmp_dir     = hmp_dir,
    hmm_dir     = hmm_dir,
    nofile_path = paste(getwd(), "src", no_file_src, sep="/"),
    ff          = ff,
    dd          = dd,
    mm          = mm,
    cc          = cc
  )
  rmarkdown::render(
    input       = paste(getwd(), "src", hmp2rep_src, sep="/"),
    output_file = rep_file,
    params      = params_list
  )
}