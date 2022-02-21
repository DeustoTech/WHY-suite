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
raw_dir_iss <- "/home/ubuntu/carlos.quesada/disk/iss/raw/"
raw_dir_lcl <- "/home/ubuntu/carlos.quesada/disk/lcl/raw/"
raw_dir_por <- "/home/ubuntu/carlos.quesada/disk/por/raw/"

# Imputation folders
imp_dir <- c(
  "iss" = "/home/ubuntu/carlos.quesada/disk/iss/imp/",
  "lcl" = "/home/ubuntu/carlos.quesada/disk/lcl/imp/",
  "por" = "/home/ubuntu/carlos.quesada/disk/por/imp/",
  "goi" = "/home/ubuntu/carlos.quesada/disk/goi4_pre/imp/"
)

imp_dir_goi4_pst <- c(
  "goi" = "/home/ubuntu/carlos.quesada/disk/goi4_pst/imp/"
)

# Metadata files
mdata_file_iss <- "/home/ubuntu/carlos.quesada/disk/iss/meta/iss_meta.csv"
mdata_file_lcl <- "/home/ubuntu/carlos.quesada/disk/lcl/meta/lcl_meta.csv"
mdata_file_por <- NULL

# Feature folders
fea_dir_por <- "/home/ubuntu/carlos.quesada/disk/features/por_22.02.17/"

# Feature files
fea_file_por      <- "/home/ubuntu/carlos.quesada/disk/features/por_22.02.17/feats_351.csv"
fea_file_goi4_pre <- "/home/ubuntu/carlos.quesada/disk/features/feats_go4_pre.csv"
fea_file_goi4_pst <- "/home/ubuntu/carlos.quesada/disk/features/feats_go4_pst.csv"

# Cluster folders (ClValid2)
clu_dir_por      <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.17_por-6cl/"
clu_dir_goi4_pre <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.21_go4-pre-20cl-som/"
clu_dir_goi4_pst <- "/home/ubuntu/carlos.quesada/analyses/clValid2/2022.02.21_go4-pst-20cl-som/"

# Instructions for "dd_sel" variable:
# Each sublist is an OR, each element within the sublist is an AND
# Each NULL element within the sublist means ALL TRUE.
dd_sel_por <- list(
  list(key="por", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff=NULL)
)
dd_sel_goi4 <- list(
  list(key="goi", is_household=NULL, rel_imputed_na=0.05, ref_atr_tariff="2")
)

################################################################################
operation <- 5
################################################################################

# 2022.02.21 - go4-pst-20cl-som
if (operation == 5) {
  # fea2clu(
  #   fea_file = fea_file_goi4_pst,
  #   clu_dir  = paste0(clu_dir_goi4_pst, "data/"),
  #   ff_sel   = c("sAggrDRM"),
  #   dd_sel   = dd_sel_goi4,
  #   mm_sel   = c("som"),
  #   vv_sel   = c("internal"),
  #   cc_sel   = c(20)
  # )
  
  clu2hmp(
    fea_file = fea_file_goi4_pst,
    clu_dir  = paste0(clu_dir_goi4_pst, "data/"),
    hmm_dir  = paste0(clu_dir_goi4_pst, "hmm/"),
    hmp_dir  = paste0(clu_dir_goi4_pst, "hmp/"),
    dset_dir = imp_dir_goi4_pst,
    cc       = 20,
    cores    = 3
  )
  
  hmp2rep(
    rep_title = "Cluster Report: GoiEner POST, 20 clusters",
    rep_file  = paste0(clu_dir_goi4_pst, "report/report.html"),
    hmm_dir   = paste0(clu_dir_goi4_pst, "hmm/"),
    hmp_dir   = paste0(clu_dir_goi4_pst, "hmp/"),
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