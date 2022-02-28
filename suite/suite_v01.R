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
hmp2m40_src <- "map40.R"
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
  fea_dir,
  analysis = "extra"
) {
  source(paste(getwd(), "src", imp2fea_src, sep="/"))
  print("## IMPUTED TO FEATURES ##")
  get_features(
    input_folder     = imp_dir,
    output_path      = fea_dir,
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
  dset_dir,
  cc,
  cores = NULL
) {
  source(paste(getwd(), "src", fea2hmp_src, sep="/"))
  print("## HEATMAP PLOTS ##")
  clValid2_heatmaps(
    feats_file  = fea_file,
    clValid_dir = paste0(clu_dir, "data/"),
    hmm_dir     = paste0(clu_dir, "hmm/"),
    hmp_dir     = paste0(clu_dir, "hmp/"),
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
  rep_type = c("basic", "map40"),
  rep_title,
  clu_dir,
  rep_fname,
  ff,
  dd,
  mm,
  cc,
  new_hmp_fname = NULL,
  tag = "PRE"
) {
  ## BASIC #####################################################################
  if ("basic" %in% rep_type) {
    print("## RMARKDOWN BASIC REPORT ##")
    params_list <- list(
      rmd_title   = rep_title,
      hmp_dir     = paste0(clu_dir, "hmp/"),
      hmm_dir     = paste0(clu_dir, "hmm/"),
      nofile_path = paste(getwd(), "src", no_file_src, sep="/"),
      ff          = ff,
      dd          = dd,
      mm          = mm,
      cc          = cc
    )
    rmarkdown::render(
      input       = paste(getwd(), "src", hmp2rep_src, sep="/"),
      output_file = paste0(clu_dir, "report/", rep_fname),
      params      = params_list
    )
  }
  ## MAP40 #####################################################################
  if ("map40" %in% rep_type) {
    source(paste(getwd(), "src", hmp2m40_src, sep="/"))
    print("## MAP TO 40 ##")
    map40(
      new_hmp_dir   = paste0(clu_dir, "hmm/"),
      all_hmp_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2021.06.08_km-som-var-cl/hmm/",
      new_hmp_fname = new_hmp_fname, #c("hmm_sAggrDRM_goi_som_30cl_i-", ".RData"),
      all_hmp_fname = c("hmm_175_", "-40.RData"),
      out_dir       = clu_dir,
      tag           = tag
    )
  }
}