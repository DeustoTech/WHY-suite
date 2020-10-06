# Carlos Quesada - Universidad de Deusto
# 2020.10.05
# Get features of all the datasets contained in a folder

library(whyT2.1)

folder_path      <- "G:/Mi unidad/WHY/Datasets/lcl/"
from_date        <- ISOdate(2013, 2, 1, 0, 0, 0)
to_date          <- ISOdate(2013, 2, 28, 23, 30, 0)
dset_key         <- "lcl"
allowed_na       <- 0
type_of_analysis <- "extra"

o <- get_features_of_datasets_in_folder(
  folder_path, from_date, to_date, dset_key, allowed_na, type_of_analysis)
