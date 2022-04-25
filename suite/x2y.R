################################################################################
##  CARLOS QUESADA GRANJA
##  FEBRUARY 17, 2022
##  UNIVERSIDAD DE DEUSTO
##  ---------------------
##  THE x2y PROCESS:
##  raw > imp > fea > clu > hmp > rep
################################################################################
source("suite_v01.R")

# Raw folders
raw_dir <- list(
  "edrp" = "/home/ubuntu/carlos.quesada/disk/edrp/raw/",
  "iss"  = "/home/ubuntu/carlos.quesada/disk/iss/raw/",
  "lcl"  = "/home/ubuntu/carlos.quesada/disk/lcl/raw/",
  "nee"  = "/home/ubuntu/carlos.quesada/disk/nee/raw/",
  "por"  = "/home/ubuntu/carlos.quesada/disk/por/raw/"
)

# Imputation folders # AS IS IN clu2hmp!
imp_dir <- list(
  "edrp" = "/home/ubuntu/carlos.quesada/disk/edrp/imp/",
  "goi"  = "/home/ubuntu/carlos.quesada/disk/goi4_pre/imp/",
  "iss"  = "/home/ubuntu/carlos.quesada/disk/iss/imp/",
  "lcl"  = "/home/ubuntu/carlos.quesada/disk/lcl/imp/",
  "nee"  = "/home/ubuntu/carlos.quesada/disk/nee/imp/",
  "por"  = "/home/ubuntu/carlos.quesada/disk/por/imp/"
)

imp_dir_goi4_pst <- list(
  "goi" = "/home/ubuntu/carlos.quesada/disk/goi4_pst/imp/"
)

# Metadata files
mdata_file <- list(
  "edrp" = "/home/ubuntu/carlos.quesada/disk/edrp/meta/edrp_geography_data_v2.csv",
  "goi"  = "/home/ubuntu/carlos.quesada/disk/goi4/meta/goi4_meta.csv",
  "iss"  = "/home/ubuntu/carlos.quesada/disk/iss/meta/iss_meta.csv",
  "lcl"  = "/home/ubuntu/carlos.quesada/disk/lcl/meta/lcl_meta.csv",
  "nee"  = "/home/ubuntu/carlos.quesada/disk/nee/meta/nee_meta.csv",
  "por"  = NULL
)

# Feature folders
fea_dir <- list(
  "edrp" = "/home/ubuntu/carlos.quesada/disk/features/edrp_22.03.23/",
  "iss"  = "/home/ubuntu/carlos.quesada/disk/features/iss_22.02.23/",
  "lcl"  = "/home/ubuntu/carlos.quesada/disk/features/lcl_22.02.23/",
  "nee"  = "/home/ubuntu/carlos.quesada/disk/features/nee_22.02.23/",
  "por"  = "/home/ubuntu/carlos.quesada/disk/features/por_22.02.17/"
)

# Feature files
fea_file <- list(
  "all"      = "/home/ubuntu/carlos.quesada/disk/features/feats_v2.00.csv",
  "edrp"     = "/home/ubuntu/carlos.quesada/disk/features/edrp_22.03.23/feats.csv",
  "goi4_pre" = "/home/ubuntu/carlos.quesada/disk/features/feats_go4_pre.csv",
  "goi4_pst" = "/home/ubuntu/carlos.quesada/disk/features/feats_go4_pst.csv",
  "iss"      = "/home/ubuntu/carlos.quesada/disk/features/iss_22.02.23/feats_6084.csv",
  "lcl"      = "/home/ubuntu/carlos.quesada/disk/features/lcl_22.02.23/feats_5270.csv",
  "nee"      = "/home/ubuntu/carlos.quesada/disk/features/nee_22.02.23/feats_64.csv",
  "por"      = "/home/ubuntu/carlos.quesada/disk/features/por_22.02.17/feats_351.csv"
)

# Cluster folders (ClValid2)
clu_dir <- list(
  "all"      = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.03.03_all-40cl/",
  "all-km"   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.03.03_all-40cl-kmeans/",
  "all-pam"  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.03.03_all-40cl-pam/",
  "edrp"     = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.03.23_edrp/",
  "goi4_pre" = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.21_go4-pre-20cl-som/",
  "goi4_pst" = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.21_go4-pst-20cl-som/",
  "iss"      = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.03.02_iss-16cl/",
  "lcl"      = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.03.02.2_lcl-16cl/",
  "nee"      = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.23_nee/",
  "por"      = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.17_por-6cl/"
)

# Instructions for "dd_sel" variable:
# Each sublist is an OR, each element within the sublist is an AND
# Each NULL element within the sublist means ALL TRUE.
dd_sel_edrp  <- list(
  list(key="edrp", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff=NULL)
)
dd_sel_por  <- list(
  list(key="por", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff=NULL)
)
dd_sel_goi4 <- list(
  list(key="goi", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff="2")
)
dd_sel_nee  <- list(
  list(key="nee", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff=NULL)
)
dd_sel_lcl  <- list(
  list(key="lcl", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff=NULL)
)
dd_sel_iss  <- list(
  list(key="iss", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff=NULL)
)
dd_sel_all  <- list(
  list(key="por", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff=NULL),
  list(key="goi", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff="2"),
  list(key="nee", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff=NULL),
  list(key="lcl", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff=NULL),
  list(key="iss", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff=NULL)
)
################################################################################
operation <- 16
################################################################################

# 2022.04.05 - Re-running all datasets using new clustering (MEDIANS+MAD+RMAD)
if (operation == 22) {
  clu2hmp(
    fea_file = fea_file[["lcl"]],
    clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_lcl-16cl/",
    dset_dir = imp_dir,
    cc       = 16,
    cores    = 24
  )
  
  hmp2rep(
    rep_type  = c("scroll"),
    rep_title = "Cluster Report: LCL, 16 clusters",
    clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_lcl-16cl/",
    rep_fname = "cluster_report_lcl_16cl_som.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_lcl,
    mm        = c("som"),
    cc        = 16
  )
  
  #-----
  
  # clu2hmp(
  #   fea_file = fea_file[["iss"]],
  #   clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_iss-16cl/",
  #   dset_dir = imp_dir,
  #   cc       = 16,
  #   cores    = 24
  # )
  # 
  # hmp2rep(
  #   rep_type  = c("scroll"),
  #   rep_title = "Cluster Report: ISS, 16 clusters",
  #   clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_iss-16cl/",
  #   rep_fname = "cluster_report_iss_16cl_som.html",
  #   ff        = c("sAggrDRM"),
  #   dd        = dd_sel_iss,
  #   mm        = c("som"),
  #   cc        = 16
  # )
  
  #-----
  
  # clu2hmp(
  #   fea_file = fea_file[["goi4_pre"]],
  #   clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_goi-pre-20cl/",
  #   dset_dir = imp_dir,
  #   cc       = 20,
  #   cores    = 8
  # )
  # 
  # hmp2rep(
  #   rep_type  = c("scroll"),
  #   rep_title = "Cluster Report: GOI-pre, 20 clusters",
  #   clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_goi-pre-20cl/",
  #   rep_fname = "cluster_report_goi-pre_20cl_som.html",
  #   ff        = c("sAggrDRM"),
  #   dd        = dd_sel_goi4,
  #   mm        = c("som"),
  #   cc        = 20
  # )
  
  #-----
  
  # clu2hmp(
  #   fea_file = fea_file[["goi4_pst"]],
  #   clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_goi-pst-20cl/",
  #   dset_dir = imp_dir_goi4_pst,
  #   cc       = 20,
  #   cores    = 8
  # )
  # 
  # hmp2rep(
  #   rep_type  = c("scroll"),
  #   rep_title = "Cluster Report: GOI-post, 20 clusters",
  #   clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_goi-pst-20cl/",
  #   rep_fname = "cluster_report_goi-pst_20cl_som.html",
  #   ff        = c("sAggrDRM"),
  #   dd        = dd_sel_goi4,
  #   mm        = c("som"),
  #   cc        = 20
  # )
  
  #-----
  
  # clu2hmp(
  #   fea_file = fea_file[["por"]],
  #   clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_por-6cl/",
  #   dset_dir = imp_dir,
  #   cc       = 6,
  #   cores    = 24
  # )
  # 
  # hmp2rep(
  #   rep_type  = c("scroll"),
  #   rep_title = "Cluster Report: POR, 6 clusters",
  #   clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_por-6cl/",
  #   rep_fname = "cluster_report_por_6cl_som.html",
  #   ff        = c("sAggrDRM"),
  #   dd        = dd_sel_por,
  #   mm        = c("som"),
  #   cc        = 6
  # )
  
  #-----
  
  # clu2hmp(
  #   fea_file = fea_file[["all"]],
  #   clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_all-40cl/",
  #   dset_dir = imp_dir,
  #   cc       = 40,
  #   cores    = 8
  # )
  # 
  # hmp2rep(
  #   rep_type  = c("scroll"),
  #   rep_title = "Cluster Report: ALL, 40 clusters",
  #   clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng2_all-40cl/",
  #   rep_fname = "cluster_report_all_40cl_som.html",
  #   ff        = c("sAggrDRM"),
  #   dd        = dd_sel_all,
  #   mm        = c("som"),
  #   cc        = 40
  # )
}


# 2022.04.05 - Re-running all datasets using new clustering
if (operation == 21) {
  clu2hmp(
    fea_file = fea_file[["iss"]],
    clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng_iss-16cl/",
    dset_dir = imp_dir,
    cc       = 16,
    cores    = 24
  )

  hmp2rep(
    rep_type  = c("scroll"),
    rep_title = "Cluster Report: ISS, 16 clusters",
    clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng_iss-16cl/",
    rep_fname = "cluster_report_iss_16cl_som.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_iss,
    mm        = c("som"),
    cc        = 16
  )
  
  #-----
  
  clu2hmp(
    fea_file = fea_file[["goi4_pre"]],
    clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng_goi-pre-20cl/",
    dset_dir = imp_dir,
    cc       = 20,
    cores    = 8
  )
  
  hmp2rep(
    rep_type  = c("scroll"),
    rep_title = "Cluster Report: GOI-pre, 20 clusters",
    clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng_goi-pre-20cl/",
    rep_fname = "cluster_report_goi-pre_20cl_som.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_goi4,
    mm        = c("som"),
    cc        = 20
  )
  
  #-----
  
  clu2hmp(
    fea_file = fea_file[["goi4_pst"]],
    clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng_goi-pst-20cl/",
    dset_dir = imp_dir_goi4_pst,
    cc       = 20,
    cores    = 8
  )
  
  hmp2rep(
    rep_type  = c("scroll"),
    rep_title = "Cluster Report: GOI-post, 20 clusters",
    clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng_goi-pst-20cl/",
    rep_fname = "cluster_report_goi-pst_20cl_som.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_goi4,
    mm        = c("som"),
    cc        = 20
  )
  
  #-----
  
  clu2hmp(
    fea_file = fea_file[["por"]],
    clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng_por-6cl/",
    dset_dir = imp_dir,
    cc       = 6,
    cores    = 24
  )
  
  hmp2rep(
    rep_type  = c("scroll"),
    rep_title = "Cluster Report: POR, 6 clusters",
    clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng_por-6cl/",
    rep_fname = "cluster_report_por_6cl_som.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_por,
    mm        = c("som"),
    cc        = 6
  )
  
  #-----
  
  clu2hmp(
    fea_file = fea_file[["all"]],
    clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng_all-40cl/",
    dset_dir = imp_dir,
    cc       = 40,
    cores    = 8
  )
  
  hmp2rep(
    rep_type  = c("scroll"),
    rep_title = "Cluster Report: ALL, 40 clusters",
    clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.05_ng_all-40cl/",
    rep_fname = "cluster_report_all_40cl_som.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_all,
    mm        = c("som"),
    cc        = 40
  )
}


# 2022.04.04 - Scroll report for LCL
if (operation == 20) {
  hmp2rep(
    rep_type  = c("scroll"),
    rep_title = "Cluster Report: LCL, 16 clusters",
    clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.01_ng_lcl-16cl/",
    rep_fname = "cluster_report_lcl_2cl_som.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_lcl,
    mm        = c("som"),
    cc        = 16
  )
}

# 2022.04.01 - LCL con nuevas graficas
if (operation == 19) {
  clu2hmp(
    fea_file = fea_file[["lcl"]],
    clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.01_ng_lcl-16cl/",
    dset_dir = imp_dir,
    cc       = 16 #,
    # cores    = 20
  )
}

# 2022.04.01 - NEEA test con nuevas graficas
if (operation == 18) {
  clu2hmp(
    fea_file = fea_file[["nee"]],
    clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.04.01_newgraphs_nee/",
    dset_dir = imp_dir,
    cc       = 2,
    cores    = 24
  )
}

# 2022.03.23 - EDRP test (merging feature files)
if (operation == 17) {
  imp2fea_src <- "new_feats.R"
  source(paste(getwd(), "src", imp2fea_src, sep="/"))
  post_features(fea_dir[["edrp"]])
}

# 2022.03.23 - EDRP test
if (operation == 16) {
  raw2imp(
    raw_dir    = raw_dir[["edrp"]],
    imp_dir    = imp_dir[["edrp"]],
    dset_key   = "edrp",
    mdata_file = mdata_file[["edrp"]],
    min_yrs    = 1
  )

  imp2fea(
    imp_dir = imp_dir[["edrp"]],
    fea_dir = fea_dir[["edrp"]]
  )
  
  fea2clu(
    fea_file = fea_file[["edrp"]],
    clu_dir  = clu_dir[["edrp"]],
    ff_sel   = c("sAggrDRM"),
    dd_sel   = dd_sel_edrp,
    mm_sel   = c("som"),
    vv_sel   = c("internal"),
    cc_sel   = 18
  )
  
  clu2hmp(
    fea_file = fea_file[["edrp"]],
    clu_dir  = clu_dir[["edrp"]],
    dset_dir = imp_dir,
    cc       = 18,
    cores    = 16
  )
  
  hmp2rep(
    rep_type  = c("scroll"),
    rep_title = "Cluster Report: EDRP, 16 clusters, SOM",
    clu_dir   = clu_dir[["edrp"]],
    rep_fname = "cluster_report_edrp_16cl_som.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_edrp,
    mm        = c("som"),
    cc        = 2
  )
}

# 2022.03.21 - Test for generating new reports including RSD
if (operation == 15) {
  clu2hmp(
    fea_file = fea_file[["nee"]],
    clu_dir  = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.03.21_nee-rsd/",
    dset_dir = imp_dir,
    cc       = 2,
    cores    = 1
  )
  
  hmp2rep(
    rep_type  = c("rsd"),
    rep_title = "Cluster Report: NEEA, 2 clusters, including RSD",
    clu_dir   = "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.03.21_nee-rsd/",
    rep_fname = "cluster_report_nee_2cl_som.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_nee,
    mm        = c("som"),
    cc        = 2
  )
}

# 2022.03.03 - Los 40 principales, kmeans, pam
if (operation == 14) {
  fea2clu(
    fea_file = fea_file[["all"]],
    clu_dir  = clu_dir[["all-km"]],
    ff_sel   = c("sAggrDRM"),
    dd_sel   = dd_sel_all,
    mm_sel   = c("kmeans"),
    vv_sel   = c("internal"),
    cc_sel   = 40
  )
  
  clu2hmp(
    fea_file = fea_file[["all"]],
    clu_dir  = clu_dir[["all-km"]],
    dset_dir = imp_dir,
    cc       = 40,
    cores    = 2
  )
  
  hmp2rep(
    rep_type  = c("sd"),
    rep_title = "Cluster Report: All (5 datasets), 40 clusters, k-means",
    clu_dir   = clu_dir[["all-km"]],
    rep_fname = "cluster_report_5ds_40cl_kme.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_all,
    mm        = c("som"),
    cc        = 40
  )
  
  hmp2rep(
    rep_type        = "map40",
    clu_dir         = clu_dir["all-km"],
    hmp_fname_patt  = c("hmm_sAggrDRM_5ds_kme_40cl_i-", ".RData"),
    rep_title_short = "ALL(km)-40cl"
  )
   
  #---
  
    fea2clu(
    fea_file = fea_file[["all"]],
    clu_dir  = clu_dir[["all-pam"]],
    ff_sel   = c("sAggrDRM"),
    dd_sel   = dd_sel_all,
    mm_sel   = c("pam"),
    vv_sel   = c("internal"),
    cc_sel   = 40
  )
  
  clu2hmp(
    fea_file = fea_file[["all"]],
    clu_dir  = clu_dir[["all-pam"]],
    dset_dir = imp_dir,
    cc       = 40,
    cores    = 2
  )
  
  hmp2rep(
    rep_type  = c("sd"),
    rep_title = "Cluster Report: All (5 datasets), 40 clusters, pam",
    clu_dir   = clu_dir[["all-pam"]],
    rep_fname = "cluster_report_5ds_40cl_pam.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_all,
    mm        = c("pam"),
    cc        = 40
  )
  
  hmp2rep(
    rep_type        = "map40",
    clu_dir         = clu_dir["all-pam"],
    hmp_fname_patt  = c("hmm_sAggrDRM_5ds_pam_40cl_i-", ".RData"),
    rep_title_short = "ALL(pam)-40cl"
  )
}

# 2022.03.03 - map 40 de los 40 ppales
if (operation == 13) {
  hmp2rep(
    rep_type        = "map40",
    clu_dir         = clu_dir["all"],
    hmp_fname_patt  = c("hmm_sAggrDRM_5ds_som_40cl_i-", ".RData"),
    rep_title_short = "ALL(new)-40cl"
  )
}

# 2022.03.03 - Los 40 principales
if (operation == 12) {
  # fea2clu(
  #   fea_file = fea_file[["all"]],
  #   clu_dir  = clu_dir[["all"]],
  #   ff_sel   = c("sAggrDRM"),
  #   dd_sel   = dd_sel_all,
  #   mm_sel   = c("som"),
  #   vv_sel   = c("internal"),
  #   cc_sel   = 40
  # )
  
  # clu2hmp(
  #   fea_file = fea_file[["all"]],
  #   clu_dir  = clu_dir[["all"]],
  #   dset_dir = imp_dir,
  #   cc       = 40,
  #   cores    = 2
  # )
  
  hmp2rep(
    rep_type  = c("sd"),
    rep_title = "Cluster Report: All (5 datasets), 40 clusters",
    clu_dir   = clu_dir[["all"]],
    rep_fname = "cluster_report_5ds_40cl.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_all,
    mm        = c("som"),
    cc        = 40
  )
}

# 2022.03.02 - clustering ISS
if (operation == 11) {
  raw2imp(
    raw_dir   = raw_dir[["iss"]],
    imp_dir   = imp_dir[["iss"]],
    dset_key  = "iss",
    mdata_file= mdata_file[["iss"]],
    min_yrs   = 1,
    wwgen     = FALSE
  )
  
  imp2fea(
    imp_dir = imp_dir[["iss"]],
    fea_dir = fea_dir[["iss"]]
  )
  
  fea2clu(
    fea_file = fea_file[["iss"]],
    clu_dir  = clu_dir[["iss"]],
    ff_sel   = c("sAggrDRM"),
    dd_sel   = dd_sel_iss,
    mm_sel   = c("som"),
    vv_sel   = c("internal"),
    cc_sel   = 16
  )
  
  clu2hmp(
    fea_file = fea_file[["iss"]],
    clu_dir  = clu_dir[["iss"]],
    dset_dir = imp_dir,
    cc       = 16
  )
  
  hmp2rep(
    rep_type  = c("sd"),
    rep_title = "Cluster Report: ISSDA, 16 clusters",
    clu_dir   = clu_dir[["iss"]],
    rep_fname = "cluster_report_iss_16cl.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_iss,
    mm        = c("som"),
    cc        = 16
  )
}

# 2022.03.02 - clustering LCL
if (operation == 10) {
  # raw2imp(
  #   raw_dir   = raw_dir[["lcl"]],
  #   imp_dir   = imp_dir[["lcl"]],
  #   dset_key  = "lcl",
  #   mdata_file= mdata_file[["lcl"]],
  #   min_yrs   = 1,
  #   wwgen     = FALSE
  # )
  # 
  # imp2fea(
  #   imp_dir = imp_dir[["lcl"]],
  #   fea_dir = fea_dir[["lcl"]]
  # )
  # 
  # fea2clu(
  #   fea_file = fea_file[["lcl"]],
  #   clu_dir  = clu_dir[["lcl"]],
  #   ff_sel   = c("sAggrDRM"),
  #   dd_sel   = dd_sel_lcl,
  #   mm_sel   = c("som"),
  #   vv_sel   = c("internal"),
  #   cc_sel   = 16
  # )
  # 
  # clu2hmp(
  #   fea_file = fea_file[["lcl"]],
  #   clu_dir  = clu_dir[["lcl"]],
  #   dset_dir = imp_dir,
  #   cc       = 16
  # )
  
  hmp2rep(
    rep_type  = c("sd"),
    rep_title = "Cluster Report: Low Carbon London, 16 clusters",
    clu_dir   = clu_dir[["lcl"]],
    rep_fname = "cluster_report_lcl_16cl.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_lcl,
    mm        = c("som"),
    cc        = 16
  )
}

# 2022.02.27 - tidying up NEE
if (operation == 9) {
  xdir <- "C:/Users/carlos.quesada/Documents/WHY/2022.02.27 - Arreglando NEE/"
  raw2imp(
    raw_dir   = paste0(xdir, "raw/"),
    imp_dir   = paste0(xdir, "imp/"),
    dset_key  = "nee",
    mdata_file= paste0(xdir, "meta/nee_meta.csv"),
    from_date = "first",
    to_date   = ymd("2020-03-15"),
    min_yrs   = 1,
    wwgen     = FALSE
  )
}

# 2022.02.23 - raw2rep NEEA
if (operation == 8) {
  # raw2imp(
  #   raw_dir   = raw_dir[["nee"]],
  #   imp_dir   = imp_dir[["nee"]],
  #   dset_key  = "nee",
  #   mdata_file= mdata_file[["nee"]],
  #   from_date = "first",
  #   to_date   = ymd("2020-03-15"),
  #   min_yrs   = 1,
  #   wwgen     = FALSE
  # )
  # 
  # imp2fea(
  #   imp_dir = imp_dir[["nee"]],
  #   fea_dir = fea_dir[["nee"]]
  # )
  # 
  # fea2clu(
  #   fea_file = fea_file[["nee"]],
  #   clu_dir  = clu_dir_nee,
  #   ff_sel   = c("sAggrDRM"),
  #   dd_sel   = dd_sel_nee,
  #   mm_sel   = c("som"),
  #   vv_sel   = c("internal"),
  #   cc_sel   = c(2)
  # )
  # 
  # clu2hmp(
  #   fea_file = fea_file[["nee"]],
  #   clu_dir  = clu_dir_nee,
  #   dset_dir = imp_dir,
  #   cc       = 2
  # )
  
  hmp2rep(
    rep_type      = c("sd"),
    rep_title     = "Cluster Report: NEEA, 2 clusters",
    clu_dir       = clu_dir_nee,
    rep_fname     = "cluster_report_neea_2cl.html",
    ff            = c("sAggrDRM"),
    dd            = dd_sel_nee,
    mm            = c("som"),
    cc            = 2
  )
}

# 2022.02.23 - features of ISS and LCL
if (operation == 7) {
  imp2fea(
    imp_dir  = imp_dir[["iss"]],
    fea_dir  = fea_dir[["iss"]]
  )
  
  imp2fea(
    imp_dir  = imp_dir[["lcl"]],
    fea_dir  = fea_dir[["lcl"]]
  )
}

# 2022.02.21 - pre/post to ALL 20cl
if (operation == 6) {
  hmp2rep(
    rep_type      = "map40",
    clu_dir       = clu_dir_goi4_pre,
    new_hmp_fname = c("hmm_sAggrDRM_goi_som_20cl_i-", ".RData"),
    tag           = "PRE-20cl"
  )
  
  hmp2rep(
    rep_type      = "map40",
    clu_dir       = clu_dir_goi4_pst,
    new_hmp_fname = c("hmm_sAggrDRM_goi_som_20cl_i-", ".RData"),
    tag           = "POST-20cl"
  )
}

# 2022.02.21 - go4-pst-20cl-som
if (operation == 5) {
  fea2clu(
    fea_file = fea_file_goi4_pst,
    clu_dir  = clu_dir_goi4_pst,
    ff_sel   = c("sAggrDRM"),
    dd_sel   = dd_sel_goi4,
    mm_sel   = c("som"),
    vv_sel   = c("internal"),
    cc_sel   = c(20)
  )
  
  clu2hmp(
    fea_file = fea_file_goi4_pst,
    clu_dir  = clu_dir_goi4_pst,
    dset_dir = imp_dir_goi4_pst,
    cc       = 20,
    cores    = 5
  )
  
  hmp2rep(
    rep_title = "Cluster Report: GoiEner POST, 20 clusters",
    clu_dir   = clu_dir_goi4_pst,
    rep_fname = "report.html",
    ff        = c("sAggrDRM"),
    dd        = dd_sel_goi4,
    mm        = c("som"),
    cc        = 20
  )
}

# 2022.02.21 - go4-pre-20cl-som
if (operation == 4) {
  fea2clu(
    fea_file = fea_file_goi4_pre,
    clu_dir  = paste0(clu_dir_goi4_pre, "data/"),
    ff_sel   = c("sAggrDRM"),
    dd_sel   = dd_sel_goi4,
    mm_sel   = c("som"),
    vv_sel   = c("internal"),
    cc_sel   = c(20)
  )
  
  clu2hmp(
    fea_file = fea_file_goi4_pre,
    clu_dir  = paste0(clu_dir_goi4_pre, "data/"),
    hmm_dir  = paste0(clu_dir_goi4_pre, "hmm/"),
    hmp_dir  = paste0(clu_dir_goi4_pre, "hmp/"),
    dset_dir = imp_dir,
    cc       = 20
  )
  
  hmp2rep(
    rep_title = "Cluster Report: GoiEner PRE, 20 clusters",
    rep_file  = paste0(clu_dir_goi4_pre, "report/report.html"),
    hmm_dir   = paste0(clu_dir_goi4_pre, "hmm/"),
    hmp_dir   = paste0(clu_dir_goi4_pre, "hmp/"),
    ff        = c("sAggrDRM"),
    dd        = dd_sel_goi4,
    mm        = c("som"),
    cc        = 20
  )
}

# 2022.02.17 - fea2rep POR
if (operation == 3) {
  fea2clu(
    fea_file = fea_file_por,
    clu_dir  = paste0(clu_dir_por, "data/"),
    ff_sel   = c("sAggrP6", "sAggrDRM"),
    dd_sel   = dd_sel_por,
    mm_sel   = c("som"),
    vv_sel   = c("internal"),
    cc_sel   = c(6)
  )

  clu2hmp(
    fea_file = fea_file_por,
    clu_dir  = paste0(clu_dir_por, "data/"),
    hmm_dir  = paste0(clu_dir_por, "hmm/"),
    hmp_dir  = paste0(clu_dir_por, "hmp/"),
    dset_dir = imp_dir,
    cc       = 6
  )
  
  hmp2rep(
    rep_title = "Cluster Report: Elergone Energia",
    rep_file  = paste0(clu_dir_por, "report/report.html"),
    hmm_dir   = paste0(clu_dir_por, "hmm/"),
    hmp_dir   = paste0(clu_dir_por, "hmp/"),
    ff        = c("sAggrP6", "sAggrDRM"),
    dd        = dd_sel_por,
    mm        = c("som"),
    cc        = 6
  )
}

# 2022.02.17 - imp2fea POR
if (operation == 2) {
  imp2fea(
    imp_dir  = imp_dir[["por"]],
    fea_file = fea_dir_por
  )
}

# 2022.02.17 - raw2imp POR
if (operation == 1) {
  raw2imp(
    raw_dir    = raw_dir_por,
    imp_dir    = imp_dir[["por"]],
    dset_key   = "por",
    mdata_file = mdata_file_por,
    from_date  = "first",
    to_date    = "last",
    min_yrs    = 1,
    wwgen      = FALSE
  )
}