source("goiener-ext-3v3.R")

## Metadata files
metadata_goi <- "C:/Users/carlos.quesada/Documents/WHY/2022.02.01 - Corrigiendo goiener-ext-3.R/old/Contratos_goi_meg_cor_20211222.csv"
metadata_lcl <- "G:/Mi unidad/WHY/Datos (raw)/Low Carbon London/informations_households.csv"
metadata_iss <- "G:/Mi unidad/WHY/Datos (raw)/ISSDA/CER Electricity Revised March 2012/CER_Electricity_Documentation/allocations.csv"
metadata_nee <- "G:/Mi unidad/WHY/Datos (raw)/NEEA/sites.csv"

#Function call for "goi"
extend_dataset_v2(
  input_folder            = "C:/Users/carlos.quesada/Documents/WHY/2022.02.01 - Corrigiendo goiener-ext-3.R/iss/", #"G:/Mi unidad/WHY/Datasets/nee/raw/",
  output_folder           = "C:/Users/carlos.quesada/Documents/WHY/2022.02.01 - Corrigiendo goiener-ext-3.R/iss/",
  dset_key                = "iss",
  metadata_files          = metadata_iss,
  working_with_generation = FALSE,
  min_years               = 1,
  from_date               = "first",
  to_date                 = "last"
)