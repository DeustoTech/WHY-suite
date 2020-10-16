#' This is a compilation of simple scripts to execute functions from the 
#' `whyT2.1` library. 
#' 
#' LIST OF SCRIPTS:
#' 
#' `#1` GENERATION OF EXTENDED DATASETS FROM FOLDER OF RAW DATASETS
#' Given a folder of raw datasets, generate the extended datasets, i.e. create time series of at least 2 years long.

################################################################################
script_selection <- 1
################################################################################

library(whyT2.1)

scripts <- function(script_selection) {
  # SCRIPT #1
  if (script_selection == 1) {
    input_folder  <- "G:/Mi unidad/WHY/Datasets/lcl/"
    output_folder <- "G:/Mi unidad/WHY/Datasets/lcl-ext/"
    extend_datasets(input_folder,output_folder)
  }
}

# Execute
scripts(script_selection)
