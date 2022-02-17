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
raw_dir_por <- "/home/ubuntu/carlos.quesada/disk/por/raw/"

# Imputation folders
imp_dir_por <- "/home/ubuntu/carlos.quesada/disk/por/imp/"

# Metadata files
mdata_file_iss <- "/home/ubuntu/carlos.quesada/disk/iss/meta/iss_meta.csv"
mdata_file_lcl <- "/home/ubuntu/carlos.quesada/disk/lcl/meta/lcl_meta.csv"
mdata_file_por <- NULL

################################################################################
operation <- 1
################################################################################

# 2022.02.17 - raw2imp POR
if (operation == 1) {
  raw2imp(
    raw_dir    = raw_dir_por,
    imp_dir    = imp_dir_por,
    dset_key   = "por",
    mdata_file = mdata_file_por,
    from_date  = "first",
    to_date    = "last",
    min_yrs    = 1,
    wwgen      = FALSE
  )
}