################################################################################
##  CARLOS QUESADA GRANJA
##  FEBRUARY 17, 2022
##  UNIVERSIDAD DE DEUSTO
##  ---------------------
##  THE x2y PROCESS:
##  raw > imp > fea > clu > hmp > rep
################################################################################

# FILES IN SUBFOLDER 'src'
raw2imp_src <- c("raw2imp.R", "dataset-specific-funs.R")
imp2fea_src <- "imp2fea.R"
fea2hmp_src <- "fea2hmp_v3.R"
hmp2rep_src <- "hmp2rep.R"

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
  wwgen     = FALSE,
  parallel  = TRUE
) {
  print("####################")
  print("####################")
  print("####################")
  print("## RAW TO IMPUTED ##")
  print("####################")
  print("####################")
  print("####################")
  
  for(ss in raw2imp_src) source(paste(getwd(), "src", ss, sep="/"))
  
  if (parallel) {
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
  } else {
  	  extend_dataset_v2_non_parallel(
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
}

###############################
## IMPUTED FILES TO FEATURES ##
###############################
imp2fea <- function(
  imp_dir,
  fea_dir,
  analysis = "extra",
  max_feats = 1000,
  limited_to = NULL
) {
  print("#########################")
  print("#########################")
  print("#########################")
  print("## IMPUTED TO FEATURES ##")
  print("#########################")
  print("#########################")
  print("#########################")
  
  source(paste(getwd(), "src", imp2fea_src, sep="/"))
  
  get_features(
    input_folder     = imp_dir,
    output_path      = fea_dir,
    type_of_analysis = analysis,
    max_feats        = max_feats,
    limited_to       = limited_to
  )
}

######################
## CLUSTER ANALYSIS ##
######################
fea2clu <- function(
  fea_dir,
  fea_file = NULL,
  clu_dir,
  ff_sel,
  dd_sel,
  mm_sel,
  vv_sel,
  cc_sel,
  use_clValid2 = TRUE
) {
  print("######################")
  print("######################")
  print("######################")
  print("## CLUSTER ANALYSES ##")
  print("######################")
  print("######################")
  print("######################")
  
  source(paste(getwd(), "src", fea2hmp_src, sep="/"))
  
  cluster_features(
    feats_file = ifelse(
      is.null(fea_file),
      paste0(fea_dir, "feats.csv"),
      paste0(fea_dir, fea_file)
    ),
    output_dir   = clu_dir,
    ff_sel       = ff_sel,
    dd_sel       = dd_sel,
    mm_sel       = mm_sel,
    vv_sel       = vv_sel,
    cc_sel       = cc_sel,
    use_clValid2 = use_clValid2
  )
}

############################
## GENERATION OF HEATMAPS ##
############################
clu2hmp <- function(
  fea_dir,
  fea_file = NULL,
  clu_dir,
  preco_file,
  cc,
  cores = NULL
) {
  print("###################")
  print("###################")
  print("###################")
  print("## HEATMAP PLOTS ##")
  print("###################")
  print("###################")
  print("###################")
  
  source(paste(getwd(), "src", fea2hmp_src, sep="/"))
  
  dirs <- list(
    dplot  = paste0(clu_dir, "dplot/" ),
    hmp    = paste0(clu_dir, "hmp/"   ),
    report = paste0(clu_dir, "report/"),
    acf    = paste0(clu_dir, "acf/"   ),
    distr  = paste0(clu_dir, "distr/" )
  )
  clValid2_heatmaps(
    feats_file = ifelse(
      is.null(fea_file),
      paste0(fea_dir, "feats.csv"),
      paste0(fea_dir, fea_file)
    ),
    clValid_dir = clu_dir,
    dir_names   = dirs,
    preco_file  = preco_file,
    num_cluster = cc,
    scale_hmm   = TRUE,
    num_cores   = cores
  )
}

##############################
## CREATION OF BASIC REPORT ##
##############################
hmp2rep <- function(
  rep_type = c("basic", "sd", "rsd", "map40", "scroll"),
  rep_title,
  rep_title_short = "PRE",
  clu_dir,
  rep_fname,
  ff,
  dd,
  mm,
  cc,
  hmp_fname_patt = NULL
) {
  print("###############")
  print("###############")
  print("###############")
  print("## REPORTING ##")
  print("###############")
  print("###############")
  print("###############")
  
  source(paste(getwd(), "src", hmp2rep_src, sep="/"))
  
  reporting(
    rep_type      = rep_type,
    rep_title     = rep_title,
    clu_dir       = clu_dir,
    rep_fname     = rep_fname,
    ff            = ff,
    dd            = dd,
    mm            = mm,
    cc            = cc,
    new_hmp_fname = hmp_fname_patt,
    tag           = rep_title_short
  )
}